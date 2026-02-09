# Research: Sending Prompts to Claude Code from Emacs

> **See also:** @/Users/gdanov/work/playground/emacs-gravity/ARCHITECTURE.md for system architecture overview and module structure.

## TL;DR

Emacs can fully control a Claude Code session by spawning `claude -p --input-format stream-json --output-format stream-json` as a subprocess. Multi-turn prompts via JSON on stdin, structured responses on stdout, all hooks fire normally so gravity UI works. Can `--resume <session-id>` to take over an existing terminal session.

**Expanded scope**: We want emacs-gravity to function not just as an observer (with feedback) UI, but also as a driver — prompts are sent from within Emacs, not via the terminal. For this we need to start the Claude process ourselves.

---

## The Mechanism: Agent SDK Streaming Input via CLI

### How It Works

```
Emacs (claude-gravity.el)
  ├── spawns: claude -p --input-format stream-json --output-format stream-json
  │     ├── stdin  ← JSON user messages (prompts, images)
  │     └── stdout → JSON response stream (text, tool calls, results, metadata)
  └── hooks still fire → gravity UI populated as usual
```

1. Emacs starts Claude Code as a subprocess in print mode with streaming JSON I/O
2. User types prompt in Emacs → sent as JSON on stdin:
   ```json
   {"type":"user","message":{"role":"user","content":"Fix the bug in auth.py"}}
   ```
3. Claude responds via stdout as newline-delimited JSON events
4. All existing hooks (PreToolUse, PostToolUse, etc.) fire normally → gravity UI updates
5. Multi-turn: send another JSON message on stdin after Claude finishes a turn
6. Can `--resume <session-id>` to continue a previous session
7. Can `--continue` to continue most recent session in the directory

### Output Access (stream-json stdout)

With `--output-format stream-json --verbose --include-partial-messages`, we get **much richer output** than hooks alone:

| Event type | What it contains |
|---|---|
| `{"type":"system","subtype":"init","session_id":"..."}` | Session ID, initialization |
| `{"type":"stream_event","event":{"type":"content_block_delta",...}}` | Streaming text tokens as they generate |
| `{"type":"stream_event","event":{"type":"content_block_start","content_block":{"type":"tool_use","name":"Edit"}}}` | Tool call start with name and args |
| `{"type":"assistant","message":{...}}` | Complete assistant message (after streaming) |
| `{"type":"result","result":"...","session_id":"..."}` | Turn complete, final text result |

This gives us **streaming text** (not available via hooks), tool call arguments as they stream in, and structured metadata — all in addition to the hook-based gravity UI.

### Session Takeover Flow

To take over an existing terminal session:
1. User has Claude Code running in terminal → gravity UI shows session with ID
2. User kills the terminal session (or it finishes)
3. From Emacs: resume with:
   ```
   claude -p --resume <session-id> --input-format stream-json --output-format stream-json
   ```
4. Emacs now owns the session — full multi-turn control with complete history

### One-Shot Variant

For quick single prompts without a persistent process:
```bash
claude -p --resume <session-id> --output-format json "Do this next thing"
```
- `--output-format json` returns structured result with session_id, usage, etc.
- `--fork-session` creates a branch (preserves original session unchanged)
- Useful for quick commands from Emacs transient menu

---

## SDK Infrastructure: Full Claude Code Equivalence

**Does `claude -p` / Agent SDK have access to everything?** Yes.

The docs state: "The Agent SDK gives you the same tools, agent loop, and context management that power Claude Code." Specifically:

| Feature | Available in `-p` mode? |
|---|---|
| All built-in tools (Read, Edit, Bash, Glob, Grep, WebSearch, WebFetch) | Yes |
| CLAUDE.md / .claude/CLAUDE.md project memory | Yes (with `--setting-sources project`) |
| MCP servers | Yes (via `--mcp-config` or project config) |
| Plugins | Yes (via `--plugin-dir`) |
| Hooks (PreToolUse, PostToolUse, Stop, etc.) | Yes |
| Skills (.claude/skills/*.md) | Yes |
| Slash commands (.claude/commands/*.md) | Yes (described as task, not `/command`) |
| Subagents | Yes (via `--agents` JSON) |
| Session resume/fork | Yes (via `--resume`, `--fork-session`) |
| Permission modes | Yes (via `--permission-mode`) |
| Token budgets | Yes (via `--max-budget-usd`) |

### Two Ways to Start the Process

**Option 1: CLI (`claude -p`)** — Emacs spawns the `claude` binary directly via `make-process`.

**Option 2: Agent SDK** — Our Node.js bridge uses `@anthropic-ai/claude-agent-sdk` to manage the Claude process, communicating with Emacs over the existing Unix socket.

| Aspect | CLI `claude -p` | Agent SDK (Node.js) |
|---|---|---|
| **Auth** | Subscription (same as terminal) | API key (`ANTHROPIC_API_KEY`) |
| **Emacs integration** | `make-process` + process filter (native Elisp) | Bridge manages process; Emacs gets events via socket |
| **I/O protocol** | stdin/stdout JSON (direct) | SDK objects + callbacks in Node.js |
| **Streaming text** | `--include-partial-messages` on stdout | `StreamEvent` objects in SDK loop |
| **Session resume** | `--resume <id>` flag | `options.resume = sessionId` |
| **Hooks** | Fire via `--plugin-dir` (same as terminal) | SDK has native hook callbacks (no shell scripts) |
| **Permission handling** | `--permission-prompt-tool` or hook-based | `canUseTool` callback in SDK |
| **MCP servers** | `--mcp-config` flag | `mcpServers` option |
| **Tool approval** | `--allowedTools` flag | `allowedTools` option |
| **Dependencies** | None (claude binary already installed) | `npm install @anthropic-ai/claude-agent-sdk` |
| **Complexity** | Lower — Emacs talks to process directly | Higher — bridge as intermediary |
| **Structured output** | `--json-schema` flag | `jsonSchema` option + `structured_output` field |
| **Image support** | Via stream-json stdin messages | Native in message objects |
| **Process lifecycle** | Emacs owns process directly (kill, signal) | Bridge owns; Emacs signals via socket |

**Recommendation**: Start with **CLI `claude -p`** — simpler, no extra dependency, Emacs manages the process natively. The Agent SDK is worth revisiting if we need deeper control (custom tool approval callbacks, structured outputs with typed schemas, or running the bridge as a persistent daemon that manages multiple sessions).

---

## Key CLI Flags

| Flag | Purpose |
|---|---|
| `-p` / `--print` | Non-interactive mode (required) |
| `--input-format stream-json` | Accept JSON messages on stdin for multi-turn |
| `--output-format stream-json` | Newline-delimited JSON events on stdout |
| `--include-partial-messages` | Stream text/tool deltas as they generate |
| `--verbose` | Full turn-by-turn output |
| `--resume <id>` | Continue a specific session by ID |
| `--continue` | Continue most recent session in cwd |
| `--fork-session` | Branch session on resume (don't modify original) |
| `--allowedTools "..."` | Auto-approve specific tools |
| `--permission-prompt-tool` | MCP tool to handle permission prompts |
| `--plugin-dir <path>` | Load our plugin for hooks |
| `--model <model>` | Override model (sonnet/opus/haiku) |
| `--max-turns <n>` | Limit agentic turns |
| `--max-budget-usd <n>` | Budget cap |
| `--replay-user-messages` | Echo user messages back on stdout (for tracking) |

## Input Message Format (stdin)

```json
{"type":"user","message":{"role":"user","content":"Your prompt here"}}
```

With image:
```json
{"type":"user","message":{"role":"user","content":[
  {"type":"text","text":"Review this diagram"},
  {"type":"image","source":{"type":"base64","media_type":"image/png","data":"..."}}
]}}
```

Messages can be queued — send multiple, they process sequentially.

---

## What About Injecting into an Already-Running Terminal Session?

**Not possible natively.** Claude Code has no IPC socket or reverse channel for prompt injection. The hook system is one-way (Claude → plugins) except for PermissionRequest and AskUserQuestion responses.

**But this doesn't matter.** For externally-controlled Claude instances, hooks handle observation. For prompt control, take over the session: kill terminal, `--resume` from Emacs.

---

## Architecture: View Model with Dual Adapters

### Current State (tightly coupled)

Today `claude-gravity.el` has hooks → direct plist mutation → renderers read plists:

```
Hook events → claude-gravity-handle-event → setf/plist-put → session plist ← renderers read directly
```

No separation between data ingestion, state management, and rendering. Event handlers mutate raw plists; renderers compute groupings on-the-fly from those plists.

### Target Architecture

```
┌─────────────────────────────────────────────────────────┐
│                    VIEW MODEL (core)                     │
│                                                         │
│  claude-gravity--sessions hash table                    │
│  Session plist: :state, :prompts, :agents, :tasks,     │
│                 :files, :current-turn, :status,         │
│                 :claude-status, :token-usage, :plan,    │
│                 :streaming-text, :managed-process       │
│                                                         │
│  API:                                                   │
│    claude-gravity-model-add-tool (session tool-alist)   │
│    claude-gravity-model-update-tool (session tid result)│
│    claude-gravity-model-add-prompt (session text meta)  │
│    claude-gravity-model-set-status (session status)     │
│    claude-gravity-model-add-agent (session agent-alist) │
│    claude-gravity-model-update-agent (session aid data) │
│    claude-gravity-model-append-text (session text)      │
│    claude-gravity-model-set-token-usage (session usage) │
│    claude-gravity-model-session-start (session-id cwd)  │
│    claude-gravity-model-session-end (session-id)        │
│    ... (thin wrappers around current plist mutations)   │
│                                                         │
│  Each model function schedules a debounced refresh.     │
└────────────┬────────────────────────┬───────────────────┘
             │                        │
    ┌────────┴────────┐     ┌─────────┴──────────┐
    │  HOOKS ADAPTER  │     │ JSON-OUTPUT ADAPTER │
    │                 │     │                     │
    │ Translates:     │     │ Translates:         │
    │  SessionStart   │     │  system/init        │
    │  SessionEnd     │     │  stream_event       │
    │  PreToolUse     │     │  assistant          │
    │  PostToolUse    │     │  result             │
    │  SubagentStart  │     │                     │
    │  SubagentStop   │     │ + streaming text    │
    │  UserPromptSub. │     │   accumulation      │
    │  Stop           │     │ + tool call parsing │
    │  Notification   │     │                     │
    │  PermissionReq. │     │ Source: stdout of    │
    │  AskUserQuestion│     │ managed claude -p   │
    │                 │     │ process              │
    │ Source: socket  │     │                     │
    │ from bridge     │     │                     │
    └─────────────────┘     └─────────────────────┘

    Both adapters call the same Model API functions.
    Neither knows about the other.

┌─────────────────────────────────────────────────────────┐
│                MAGIT-SECTION RENDERER                    │
│                                                         │
│  Reads ONLY from View Model (session plists).           │
│  Knows nothing about hooks or json-output.              │
│                                                         │
│  claude-gravity--render-overview                        │
│  claude-gravity--render-session-buffer                  │
│    ├── insert-header                                    │
│    ├── insert-plan                                      │
│    ├── insert-streaming-text (NEW: live assistant text) │
│    ├── insert-turns                                     │
│    │   └── insert-turn-children                         │
│    │       ├── insert-tool-item                         │
│    │       └── insert-agent-branch                      │
│    ├── insert-files                                     │
│    └── insert-allow-patterns                            │
└─────────────────────────────────────────────────────────┘
```

### What Changes

**Core View Model** — Extract a clean mutation API from `claude-gravity-handle-event`:
- Current: 10 event branches with inline `setf`/`plist-put` (~300 lines)
- Target: Each branch calls model functions instead of direct mutation
- The model functions are thin wrappers (1-3 lines each) — not a big refactor
- New field: `:streaming-text` for live assistant text from json-output adapter
- New field: `:managed-process` for the Emacs-owned claude subprocess

**Hooks Adapter** — Basically today's `claude-gravity-handle-event`, but calling model API:
- Already exists, just needs mechanical refactoring
- Still handles bidirectional flows (PermissionRequest, AskUserQuestion)
- No functional change

**JSON-Output Adapter** — New code:
- Process filter for managed claude subprocess stdout
- Parses stream-json events (newline-delimited JSON)
- Maps each event type to model API calls:
  - `system/init` → `model-session-start`
  - `stream_event/content_block_delta/text_delta` → `model-append-text`
  - `stream_event/content_block_start/tool_use` → `model-add-tool`
  - `assistant` → finalize tool entries, store complete message
  - `result` → `model-set-status idle`, store final text
- Handles streaming text accumulation (hooks don't provide this)

**Renderer** — Minimal changes:
- New section: `insert-streaming-text` for live assistant output
- Everything else reads from model as before
- No awareness of data source

### Overlap and Deduplication

When Emacs manages the process AND hooks fire (both adapters active):
- Both adapters write to the same session plist via model API
- Tools: hooks adapter gets PreToolUse/PostToolUse; json-output gets tool_use events
- Need dedup by `tool_use_id` — model API checks for existing entry before adding
- Streaming text only from json-output (hooks don't provide it)
- Prompts only from hooks adapter (UserPromptSubmit) or Emacs-initiated sends
- Status transitions: both adapters may set status — last write wins (both correct)

### Migration Path

1. **Phase 1**: Extract model API functions (mechanical refactor, no behavior change) — **DONE**
2. **Phase 2**: Add managed process + json-output adapter (new feature, hooks still work) — **DONE**
3. **Phase 3**: Optionally run without hooks (json-output only mode for simpler setups) — open (backlog)

---

## Verification Steps

1. `echo '{"type":"user","message":{"role":"user","content":"What is 2+2?"}}' | claude -p --input-format stream-json --output-format stream-json --verbose`
2. Verify hooks fire when using `-p` mode with `--plugin-dir`
3. Test `--resume` with a known session ID
4. Test multi-turn: two sequential prompts via stdin
5. Test in Emacs: `start-process` with stream-json, verify process filter receives output

---

## Beads Breakdown

### Epic: Emacs-Driven Claude Sessions

**Phase 1: Extract View Model API** — DONE
- 17 `claude-gravity-model-*` functions extracted from `claude-gravity-handle-event`
- Pure mechanical refactor, no behavior change

**Phase 2: Managed Claude Process + JSON-Output Adapter** — DONE
- `claude-gravity-start-session` spawns `claude -p --input-format stream-json --output-format stream-json --verbose --include-partial-messages`
- `claude-gravity-resume-session` resumes with `--resume <session-id>`
- `claude-gravity-send-prompt` sends JSON messages on stdin (keybind `s`)
- JSON-output adapter parses all stream-json events: system/init, stream_event (text_delta, content_block_start/tool_use, message_start), assistant, user (tool_result), result
- Streaming text display: `claude-gravity-insert-streaming-text` renders live assistant text with `>>> Claude is responding...` heading
- Tool dedup via `claude-gravity--session-has-tool-p` (checks root + agent tools by tool_use_id)
- Session re-keying: temp managed-* ID → real UUID on system/init
- Process sentinel handles exit/crash, marks session ended

**Phase 3: Hooks-Free Mode** — open (backlog)
- Run gravity without hooks (json-output only)
- For simpler setups where user doesn't want a plugin installed
- All state from stdout parsing alone

### Verified End-to-End
- Session init + re-keying
- Streaming text accumulation (text_delta) and cleanup (result/assistant)
- Tool creation from content_block_start and completion from user event
- Token usage from result event
- Multi-turn prompts via stdin
