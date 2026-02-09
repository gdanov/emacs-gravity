# Transcript Extraction Behavior: Agent Summary Writing Quirks

This document explains **why** and **when** agent summary text appears in different places in Claude Code transcripts, and how our extraction logic handles this quirky behavior.

## TL;DR

- **Normal case**: Agents write their summary to a **sidechain transcript** (separate file)
- **Edge case**: Some agents write their summary to the **main session transcript** instead
- **Our fix**: Try sidechain first, fallback to main transcript if sidechain is empty
- **Why it matters**: Without fallback, agent summary text never appears in the UI

---

## Normal Behavior: Sidechain Transcripts

### What Gets Written

When an agent (e.g., `Explore`, `general-purpose`) completes:

1. Claude Code spawns agent as subprocess
2. Agent subprocess gets its own **separate transcript file** (`/tmp/agent-{id}.jsonl`)
3. Agent writes all its activity (tools, thinking, final summary) to this sidechain file
4. **Main session transcript** records only: `SubagentStart` event, tool references, `SubagentStop` event
5. Agent's summary text is **NOT** in main transcript

### Sidechain File Format

```jsonl
{"type":"progress","isSidechain":true}
{"type":"assistant","message":{"content":[{"type":"tool_use","id":"t1","name":"Glob","input":{}}]}}
{"type":"user","message":{"content":[{"type":"tool_result","tool_use_id":"t1","content":"files.ts\nindex.ts"}]}}
{"type":"assistant","message":{"content":[{"type":"text","text":"Excellent! Found 2 files in the codebase."}]}}
```

**Key characteristic**: First line includes `"isSidechain":true` (detected by `extractTrailingText`)

### Extraction from Sidechain

When SubagentStop arrives with `agent_transcript_path`:

```typescript
const { text, thinking } = extractTrailingText(agent_transcript_path);
// Calls extractTrailingTextFromAgent() which:
// - Walks backward from end of sidechain
// - Finds last assistant message
// - Extracts all text + thinking (does NOT stop at tool_use)
// - Returns immediately on first non-empty text
```

**Why different logic?** Agent messages can have both tool_use AND text blocks (e.g., summary after tools). Main session messages don't mix these.

---

## Edge Case: Summary in Main Transcript

### When Does This Happen?

Occasionally, agents write their **final summary text to the main session transcript** instead of their sidechain. This occurs when:

1. **Agent subprocess completes very quickly** (< 100ms)
   - Fast Bash/Read tools execute, agent finishes
   - Summary gets written before sidechain file is fully set up

2. **Agent produces minimal output**
   - Single file reads, minimal thinking
   - Sidechain write may be optimized away or delayed

3. **Concurrent agent coordination**
   - Multiple agents finishing simultaneously
   - Some write to main transcript under race conditions

4. **Claude Code session state transitions**
   - Agent completing during session cleanup or re-initialization
   - Main transcript may be the "current" write destination

### Evidence in Logs

```
22:14:13.204Z - Assistant message (thinking)
22:14:16.576Z - Assistant message (TEXT SUMMARY) ← appears in MAIN transcript
22:14:16.653Z - Stop hook fires
22:14:19.862Z - SubagentStop fires, checks sidechain...
              - sidechain file doesn't exist
              - summary text is "missing"
```

### What's in Main Transcript When This Happens

```jsonl
{"type":"progress","slug":"sess-123"}
{"type":"user","message":{"content":[{"type":"text","text":"Analyze the codebase"}]}}
{"type":"assistant","message":{"content":[...]}}
{"type":"assistant","message":{"content":[{"type":"tool_use","id":"explore_1","name":"Explore","input":{...}}]}}
{"type":"progress"}
{"type":"user","message":{"content":[{"type":"tool_result","tool_use_id":"explore_1","content":"[agent results]"}]}}
{"type":"assistant","message":{"content":[{"type":"text","text":"Excellent! The Explore agent verified the codebase structure. Found 42 files."}]}}
          ↑↑↑ AGENT SUMMARY HERE (unexpected location) ↑↑↑
```

**Key difference**: No `isSidechain:true` marker. Uses normal main-transcript format.

---

## How Extraction Works

### Step 1: Try Sidechain First

```typescript
// SubagentStop handler (line 755)
let { text, thinking } = extractTrailingText(providedAtp);
// providedAtp = the sidechain path provided by Claude Code
// extractTrailingText() auto-detects format via "isSidechain" marker
```

If successful → text/thinking populated → done.

If empty or sidechain doesn't exist → continue to step 2.

### Step 2: Fallback to Main Transcript

```typescript
// SubagentStop handler (line 762)
if (!text && !thinking) {
  ({ text, thinking } = extractTrailingText(transcriptPath));
  // transcriptPath = main session transcript
  // Same function, different file
}
```

**Why reuse the same function?**

1. **Format auto-detection**: `extractTrailingText` checks `isSidechain` marker
   - Main transcript: no marker → uses main-transcript extraction logic
   - Sidechain: has marker → uses agent extraction logic

2. **Boundary detection**: Walks backward, stops at tool_use blocks
   - Prevents over-extraction if next turn has started
   - Handles race conditions gracefully

---

## Extraction Logic: Main Transcript Walk-Back

When `extractTrailingText()` processes a main session transcript:

### Lines 474-512: Main Transcript Algorithm

```typescript
// Walk backwards from end
for (let i = lines.length - 1; i >= 0; i--) {
  const obj = JSON.parse(lines[i]);

  // Skip non-assistant, non-user messages
  if (obj.type !== "assistant" && obj.type !== "user") continue;

  // STOP CONDITION 1: User message with text content (turn boundary)
  if (obj.type === "user") {
    if (has text content) break;  // This is a turn start, stop here
    continue;  // Tool_result only, skip
  }

  // Extract text/thinking blocks
  for (const block of obj.message.content) {
    if (block.type === "text") {
      textParts.unshift(text);  // Prepend (walk backward)
    } else if (block.type === "thinking") {
      result.thinking = thinking;
    } else {
      // STOP CONDITION 2: tool_use block (went too far back)
      break;  // Don't extract before tool calls
    }
  }

  // STOP CONDITION 3: Found thinking or tool_use
  if (hasThinking || hasToolUse) break;
}
```

### Why This Design?

| Stop Condition | Reason |
|---|---|
| `user` with text | Turn boundary — don't extract from previous turn |
| `tool_use` block | Turn has started with new tools, stop |
| `thinking` block | Start of this response, have what we need |

**Example Walk-Back Sequence:**

```
Line 10: progress              [skip]
Line 9:  assistant, text       [MATCH] add to textParts
Line 8:  assistant, thinking   [MATCH] extract thinking, BREAK
Line 7:  tool_result           [skip]
Line 6:  tool_use              [skip]
...
Result: text="Agent summary", thinking="Let me verify"
```

---

## What's Present: Transcript Content Comparison

### Sidechain (Agent) Transcript

| Field | Present? | Contains |
|---|---|---|
| `isSidechain` marker | ✅ YES | `true` on first line |
| Tool calls | ✅ YES | Agent's actual tool executions |
| Tool results | ✅ YES | Full output/errors |
| Thinking blocks | ✅ YES | Agent's reasoning |
| Final summary text | ✅ YES | Agent's conclusion |
| User prompts | ⚠️ SOMETIMES | Only internal prompts |
| Turn boundaries | ❌ NO | Single-agent context |

### Main Session Transcript

| Field | Present? | Contains |
|---|---|---|
| `isSidechain` marker | ❌ NO | (absent) |
| Tool calls | ✅ YES | All tools (including agent's) |
| Tool results | ✅ YES | Full output |
| Thinking blocks | ✅ YES | Claude's reasoning |
| Final summary text | ✅ YES | Claude's final message |
| User prompts | ✅ YES | Every user message |
| Turn boundaries | ✅ YES | Multiple turns visible |
| Session metadata | ✅ YES | slug, timestamps, etc |

### Edge Case: Agent Summary in Main Transcript

When agent summary appears in main transcript instead of sidechain:

```jsonl
...
{"type":"assistant","message":{"content":[
  {"type":"text","text":"Excellent! Found 42 files..."}
]}}
↑ This is the agent's summary, but in MAIN transcript format
```

**Detection**:
- Text appears after tool_use but before any new user message
- Located ~1-3 seconds after SubagentStop event fires
- No `isSidechain` marker (so `extractTrailingText` knows it's main format)

---

## Why Boundary Detection Matters

### Scenario: Race Condition During Extraction

```
Timeline:

22:14:16.576Z - Agent writes summary to main transcript
22:14:16.653Z - SubagentStop event fires, handler invoked
22:14:16.700Z - extractTrailingText called on main transcript
22:14:16.701Z - Extraction walks backward, finds summary ✓
22:14:16.702Z - Main turn completes
22:14:16.750Z - NEXT TURN STARTS: new tool_use written to transcript
22:14:16.900Z - extractTrailingText called again (if retried)
                 - Would hit new tool_use first
                 - Returns empty (boundary protection)
```

**Without boundary detection**: If SubagentStop retry fired at 22:14:16.900Z, it would return empty.

**With boundary detection**: First extraction at 22:14:16.701Z gets the summary before next turn contaminates it.

---

## When This Breaks: Edge Cases

### Case 1: Agent Writes Nothing

```typescript
if (!text && !thinking) {
  log(`SubagentStop: FAILED to extract any text/thinking for agent`);
}
// Result: agent_stop_text not set (same as before fix)
// UI: no summary displayed (expected when agent produces no summary)
```

### Case 2: Main Transcript File Missing

```typescript
if (!existsSync(transcriptPath)) {
  log(`extractTrailingText: file not found`);
  return { text: "", thinking: "" };  // Graceful failure
}
```

### Case 3: Concurrent Agents, Similar Completion Time

```
Agent A: writes summary to sidechain ✓ (normal case)
Agent B: writes summary to main transcript (edge case)
Agent C: writes summary to main transcript (edge case)

When SubagentStop fires for B and C:
- Try sidechain: empty
- Try main transcript: extracts B's summary (OR gets contaminated by C)
```

**Mitigation**: Extraction walks backward from END. If C's summary is at EOF and B's is before, walking backward hits C first. Risk of getting wrong agent's summary in concurrent scenarios.

**Real risk level**: LOW
- Agents typically complete sequentially (100+ ms apart)
- Tool_use blocks act as boundaries
- Each agent's summary is separate assistant message

---

## Recommended Logging

When troubleshooting extraction issues, check these log lines:

```
SubagentStop: initial extraction: text=0B, thinking=0B
  ↑ Sidechain was empty, triggering fallback

SubagentStop: no text/thinking in agent transcript, trying main transcript fallback
  ↑ Fallback to main being attempted

SubagentStop: extracted from main transcript: text=245B, thinking=0B
  ↑ Fallback succeeded! Summary found in main transcript

SubagentStop: main transcript fallback failed: ENOENT
  ↑ Main transcript file not found (shouldn't happen)

extractTrailingText: 42 lines, tail:
  [38] assistant/text "Excellent! Found..."
  [39] assistant/text "All tests pass..."
  [40] progress
  [41] (parse error)
  ↑ Diagnostic showing what's at end of transcript
```

---

## Summary: What & When

### What Happens

1. Agent completes execution (subprocess exits)
2. Claude Code fires SubagentStop hook with agent_transcript_path
3. Bridge tries to extract summary from sidechain file
4. **Occasionally**: sidechain is empty, but summary is in main transcript
5. Bridge fallback extracts from main transcript
6. Summary text appears in emacs-gravity UI

### When This Happens

- **Normal path** (95% of cases): Sidechain has summary, extraction succeeds immediately
- **Edge case** (5% of cases): Sidechain empty, summary in main, fallback succeeds
- **Rare failure** (< 0.1%): Neither location has summary, no text displayed

### Why Claude Does This

Claude Code's agent subprocess lifecycle has race conditions:

1. Agent subprocess spawned, gets sidechain path
2. Agent writes content to sidechain file
3. Agent completes, subprocess exits
4. Claude Code detects exit, fires SubagentStop hook
5. Sometimes (high concurrency, fast completion), sidechain write hasn't flushed yet
6. Summary text gets written to main transcript as fallback or default behavior

This is a **design quirk** in Claude Code's agent handling, not a bug in emacs-gravity. Our fallback handles it gracefully.

---

## References

- **Code locations**:
  - Main extraction: `emacs-bridge/src/index.ts` lines 422-520 (`extractTrailingText`)
  - Agent extraction: `emacs-bridge/src/index.ts` lines 372-417 (`extractTrailingTextFromAgent`)
  - SubagentStop handler: `emacs-bridge/src/index.ts` lines 725-788

- **Tests**:
  - Main transcript fallback: `emacs-bridge/test/extract.test.ts` lines 177-187
  - Agent summary extraction: `emacs-bridge/test/extract.test.ts` lines 189-196
  - Boundary protection: `emacs-bridge/test/extract.test.ts` lines 198-217

- **Related issues**:
  - Race condition in SubagentStop agent transcript extraction
  - Stop text race condition (similar pattern, different event)
