# Session Handling & View Model Specification

Reference specification for how session state works in emacs-gravity. Covers data structures, lifecycle, state transitions, turn management, bidirectional flows, and the inbox system.

For system-level architecture (hooks, bridge, socket), see [ARCHITECTURE.md](../ARCHITECTURE.md).
For data model diagrams and rendering mapping, see [session-data-model.md](session-data-model.md).

---

## 1. Data Structures

### 1.1 Session Registry

Global hash table `claude-gravity--sessions` maps session-id (string) to a session plist. Defined in `claude-gravity-session.el:15`.

**Identity & Source**

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `:session-id` | string | — | UUID assigned by Claude Code |
| `:cwd` | string | `""` | Normalized project root (git root, fully expanded) |
| `:project` | string | `""` | `(file-name-nondirectory cwd)` |
| `:source` | string | `"claude-code"` | Event source: `"claude-code"` or `"opencode"` |
| `:slug` | string\|nil | nil | Human-readable session name from Claude Code |
| `:display-name` | string\|nil | nil | User-set display name (overrides slug) |
| `:branch` | string\|nil | nil | Git branch at session time |
| `:temp-id` | string\|nil | nil | Pre-startup temp ID (for tmux re-keying) |
| `:instance-port` | integer\|nil | nil | OpenCode instance port |
| `:instance-dir` | string\|nil | nil | OpenCode instance directory |

**Lifecycle**

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `:status` | symbol | `active` | `active` or `ended` |
| `:claude-status` | symbol | `idle` | `idle` or `responding` |
| `:start-time` | time | `(current-time)` | Session creation time |
| `:last-event-time` | time | `(current-time)` | Updated on every event |
| `:pid` | integer\|nil | nil | Claude Code OS process ID |
| `:ignored` | boolean\|nil | nil | When t, PermissionRequests pass through to TUI |
| `:permission-mode` | string\|nil | nil | Current permission mode from hook payloads |
| `:title` | string\|nil | nil | Session title (from OpenCode SessionStatus/SessionUpdate) |

**Conversational State**

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `:turns` | tlist | turn-0 pre-allocated | Turn tree (see §1.2) |
| `:current-turn` | integer | 0 | Index of the active turn |
| `:total-tool-count` | integer | 0 | Cached count across all turns |
| `:plan` | plist\|nil | nil | `(:content STRING :file-path STRING :allowed-prompts LIST)` |
| `:token-usage` | alist\|nil | nil | Cumulative token counters from Claude Code |
| `:prev-token-usage` | alist\|nil | nil | Baseline for per-turn delta computation |
| `:streaming-text` | string\|nil | nil | Live text accumulator (daemon/managed sessions) |

**Indexes**

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `:tool-index` | hash-table | empty | tool_use_id → tool alist (same object as in tree) |
| `:agent-index` | hash-table | empty | agent_id → agent alist (same object as in tree) |
| `:tasks` | hash-table | empty | taskId → task alist |
| `:files` | hash-table | empty | filepath → `((ops . LIST) (last-touched . TIME))` |

**UI / Display**

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `:buffer` | buffer\|nil | nil | Owned session detail buffer |
| `:header-line-cache` | string\|nil | nil | Cached header-line (invalidated on refresh) |
| `:allow-patterns` | list\|nil | nil | From `.claude/settings.local.json` |
| `:model-name` | string\|nil | nil | Short model display name |
| `:model-id` | string\|nil | nil | Full model identifier |
| `:effort-level` | string\|nil | nil | Effort level from bridge enrichment |
| `:notifications` | list | nil | Legacy notification list |

**Managed Process / Tmux**

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `:awaiting-clear` | boolean\|nil | nil | Set when clear-and-proceed is in flight |
| `:clear-plan-path` | string\|nil | nil | Plan file path for post-clear auto-prompt |
| `:clear-timeout` | timer\|nil | nil | Timeout for awaiting-clear state |
| `:tmux-prompt-sent` | boolean\|nil | nil | Dedup flag: prompt already created by send-prompt |
| `:daemon-prompt-sent` | boolean\|nil | nil | Dedup flag for daemon-originated prompts |

**StatusLine (from StatusLine events)**

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `:cost` | float\|nil | nil | Total cost in USD |
| `:context-pct` | integer\|nil | nil | Context window usage percentage |
| `:sl-input-tokens` | integer\|nil | nil | Total input tokens |
| `:sl-output-tokens` | integer\|nil | nil | Total output tokens |
| `:sl-duration-ms` | integer\|nil | nil | Total duration in milliseconds |
| `:sl-lines-added` | integer\|nil | nil | Total lines added |
| `:sl-lines-removed` | integer\|nil | nil | Total lines removed |

### 1.2 Turn Tree

The turn tree mirrors the screen layout. Hooks write to the tree at insertion time; the renderer iterates without grouping.

```
Session :turns (tlist)
└── Turn Node (alist)
     ├── prompt (alist)          — user prompt entry
     ├── steps (tlist)          — response steps
     │    └── Step Node (alist)
     │         ├── thinking      — assistant thinking text
     │         ├── text          — assistant monologue text
     │         └── tools (tlist) — tool alists
     ├── agents (tlist)
     │    └── Agent (alist)
     │         ├── steps (tlist) — agent's own response steps
     │         │    └── Step → Tools (same structure)
     │         └── task-tool     — bidirectional link to spawning Task tool
     ├── tasks (list)            — task alists for this turn
     ├── stop_text               — trailing assistant text (from Stop)
     └── stop_thinking           — trailing thinking (from Stop)
```

**Turn Node** (created by `make-turn-node`)

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `turn-number` | integer | — | 0-based, monotonic |
| `prompt` | alist\|nil | nil | See Prompt entry below |
| `steps` | tlist | empty | Response steps |
| `agents` | tlist | empty | Agents spawned in this turn |
| `tasks` | list | nil | Task alists |
| `tool-count` | integer | 0 | Root tools (incremented at insertion) |
| `agent-count` | integer | 0 | Agents (incremented at insertion) |
| `frozen` | boolean\|nil | nil | Set when turn is complete |
| `stop_text` | string\|nil | nil | Trailing assistant text |
| `stop_thinking` | string\|nil | nil | Trailing thinking |
| `token-in` | integer\|nil | nil | Per-turn input token delta |
| `token-out` | integer\|nil | nil | Per-turn output token delta |

**Step Node** (created by `make-step-node`)

| Field | Type | Description |
|-------|------|-------------|
| `thinking` | string\|nil | Assistant thinking block |
| `text` | string\|nil | Assistant monologue text |
| `tools` | tlist | Tool alists in this step |

**Prompt Entry** (stored as turn-node's `prompt` key)

| Field | Type | Description |
|-------|------|-------------|
| `text` | string | Prompt text (XML-stripped for user prompts) |
| `type` | symbol\|nil | `question` (AskUserQuestion), `phase-boundary` (ExitPlanMode), nil (normal) |
| `submitted` | time | When prompt was sent |
| `elapsed` | float\|nil | Seconds to Stop (set by `model-finalize-last-prompt`) |
| `tool_use_id` | string\|nil | For question prompts: links to AskUserQuestion tool |
| `answer` | string\|nil | For question prompts: user's answer |

**Tool Alist** (in step's `tools` tlist)

| Field | Type | Description |
|-------|------|-------------|
| `tool_use_id` | string | Unique ID |
| `name` | string | Tool name: "Read", "Edit", "Bash", "Task", etc. |
| `input` | alist | Tool-specific parameters |
| `status` | string | `"running"`, `"done"`, `"error"` |
| `result` | any\|nil | Response (set on PostToolUse) |
| `timestamp` | time | Creation time |
| `turn` | integer | Turn number at creation |
| `duration` | float\|nil | Seconds (set on completion) |
| `permission_mode` | string\|nil | Permission mode from hook payload |
| `model` | string\|nil | Model used for this tool call |
| `requested_model` | string\|nil | Requested model |
| `assistant_text` | string\|nil | Text BEFORE this tool (bridge-extracted) |
| `assistant_thinking` | string\|nil | Thinking BEFORE this tool |
| `post_text` | string\|nil | Text AFTER this tool (bridge-extracted) |
| `post_thinking` | string\|nil | Thinking AFTER this tool |
| `parent_agent_id` | string\|nil | Owning agent (nil = root) |
| `ambiguous` | boolean\|nil | True when agent attribution uncertain |
| `candidate-agents` | list\|nil | Possible agent IDs when ambiguous |
| `agent` | alist\|nil | Bidirectional link to agent (for Task tools) |
| `auto_approved` | boolean\|nil | Bridge auto-approved this tool |

**Agent Alist** (in turn-node's `agents` tlist)

| Field | Type | Description |
|-------|------|-------------|
| `agent_id` | string | Unique agent ID |
| `type` | string | `"Explore"`, `"general-purpose"`, `"Plan"`, etc. |
| `status` | string | `"running"` or `"done"` |
| `timestamp` | time | Creation time |
| `turn` | integer | Turn number at creation |
| `duration` | float\|nil | Seconds (set on SubagentStop) |
| `steps` | tlist | Agent's own response steps |
| `tool-count` | integer | Tools in this agent |
| `transcript_path` | string\|nil | Path to agent transcript file |
| `stop_text` | string\|nil | Agent summary text |
| `stop_thinking` | string\|nil | Agent final thinking |
| `task-tool` | alist\|nil | Bidirectional link to spawning Task tool |
| `transcript_parsed` | boolean\|nil | Set by RET → parse transcript |
| `transcript_prompt` | string\|nil | Extracted from transcript |
| `transcript_model` | string\|nil | Extracted from transcript |
| `transcript_tool_count` | integer\|nil | Extracted from transcript |

**Task Alist** (in `:tasks` hash-table and turn-node `tasks` list)

| Field | Type | Description |
|-------|------|-------------|
| `taskId` | string | Task ID (from PostToolUse response) |
| `subject` | string | Task subject line |
| `description` | string\|nil | Task description |
| `activeForm` | string\|nil | Current activity text (shown in UI) |
| `status` | string | `"pending"`, `"in_progress"`, `"completed"` |
| `turn` | integer | Turn number at creation |

### 1.3 tlist (O(1) Append List)

A cons cell `(HEAD . LAST-CONS)` where HEAD is the items list and LAST-CONS points to the tail. Defined in `claude-gravity-core.el:217`.

| Operation | Function | Complexity |
|-----------|----------|------------|
| Create | `tlist-new` → `(nil . nil)` | O(1) |
| Append | `tlist-append tl item` | O(1) |
| Items | `tlist-items tl` → list | O(1) |
| Last | `tlist-last-item tl` → item | O(1) |
| Length | `tlist-length tl` | O(n) |

**Why it exists:** Standard `append` in loops is O(n) per append → O(n²) total. The tlist maintains a tail pointer, making append O(1). Used for turns, steps, tools, and agents — all growing lists.

### 1.4 Hash-Table Indexes

Three hash-table indexes provide O(1) lookup for objects stored in the tree. (`:files` is also a hash-table but tracks file operations rather than providing ID-based lookup — see §1.1.)

- **`:tool-index`** — `tool_use_id → tool alist`. The stored value is the *same* alist object that lives in the turn tree. Mutations via `setf (alist-get ...)` on either reference are visible everywhere.
- **`:agent-index`** — `agent_id → agent alist`. Same pointer-sharing semantics.
- **`:tasks`** — `taskId → task alist`. Keyed by real taskId (from PostToolUse), but during creation a temp key `_pending_<tool_use_id>` is used until the real ID arrives.

**Dedup invariant:** `model-add-tool` checks the tool-index before inserting. If a tool with the same `tool_use_id` already exists, the insertion is skipped. This prevents duplicate tools when hooks and JSON-output adapters are both active.

### 1.5 Inbox Items

Global list `claude-gravity--inbox`, newest-first. Monotonic ID counter `claude-gravity--inbox-counter`.

| Field | Type | Description |
|-------|------|-------------|
| `id` | integer | Monotonic, unique per Emacs session |
| `type` | symbol | `permission`, `question`, `plan-review`, or `idle` |
| `session-id` | string | Session this item belongs to |
| `project` | string\|nil | Project name (from session) |
| `label` | string | Display label (slug or short-id) |
| `timestamp` | time | When the item was created |
| `summary` | string | Human-readable summary (generated by `inbox-summary`) |
| `data` | alist | Event payload (tool_name, tool_input, etc.) |
| `socket-proc` | process\|nil | Bridge socket connection (alive = bridge waiting) |

**Action buffer tracking:** `claude-gravity--inbox-action-buffers` (hash-table, `id → buffer`) maps inbox item IDs to their open action buffers. Used by dismiss logic to find and kill buffers.

---

## 2. Session Lifecycle

### 2.1 State Machine

```
                         SessionStart
[not exists] ───────────────────────────▶ [active/idle]
                                              │
                               events ────────┤ (self-heal: ended→active
                                              │  on any non-SessionEnd event)
                                              │
                         SessionEnd           │
                    ┌─────────────────────────┤
                    │                         │
                    ▼                         │
            tmux alive? ─── yes ──▶ [parked]  │
                    │                  │       │
                    no          SessionStart   │
                    │            (re-key)      │
                    ▼                  │       │
                 [ended]               └──────▶┘
```

**Self-healing:** If a session is marked `ended` but receives any event other than `SessionEnd`, it is automatically set back to `active`. This handles cases where SessionEnd fired prematurely (e.g., `/clear` race).

### 2.2 Creation — `ensure-session`

`claude-gravity--ensure-session(session-id, cwd)` either returns an existing session or creates a new one.

Initial values for a new session:
1. All identity fields set from arguments
2. `:status` = `active`, `:claude-status` = `idle`
3. `:turns` = tlist with pre-allocated turn-0 (`make-turn-node 0`)
4. `:current-turn` = 0
5. All indexes created as empty hash-tables
6. `load-allow-patterns` reads `.claude/settings.local.json`

**Migration:** `migrate-session` is called on every `get-session` and `ensure-session` to ensure `:tool-index`, `:agent-index`, and `:turns` exist on older sessions.

### 2.3 Re-keying

Claude Code assigns session IDs at startup, but emacs-gravity may create placeholder sessions before the ID is known (tmux-launched sessions use a temp-id). Re-keying updates the session's identity when the real ID arrives.

**Path 1: Tmux temp-id re-key** (gravity-launched sessions)
1. User starts session via tmux → gravity assigns temp-id, stores in `claude-gravity--tmux-pending`
2. SessionStart arrives with `temp_id` in payload matching a pending entry
3. Session is moved from temp-id key to real session-id in the hash table
4. Buffer renamed, tmux mapping updated under real ID
5. If `:awaiting-clear` is set → soft re-key (§2.4), else full reset

**Path 2: Tmux-name re-key** (auto-registered sessions without temp-id)
1. SessionEnd parks the session in `claude-gravity--tmux-rekey-sessions` keyed by stable tmux session name
2. Next SessionStart's `tmux_session` field matches the parked session
3. Session is re-keyed under the new session-id

**Path 3: Daemon pending re-key**
1. Same pattern as tmux, using `claude-gravity--daemon-pending` hash table
2. Delegated to `daemon-rekey-session`

### 2.4 Soft Re-key — /clear with State Preservation

`claude-gravity--soft-rekey-session(session, new-id)` re-keys identity without resetting conversational state. Used by clear-and-proceed flow.

Preserved: turns, tools, agents, tasks, files, indexes, buffer.

Updated:
1. `:session-id` = new-id
2. `:status` = `active`, `:claude-status` = `idle`
3. Previous turn frozen
4. New turn inserted with `phase-boundary` prompt: "Context cleared — implementing plan"
5. `:awaiting-clear` and `:clear-timeout` cleared

### 2.5 Session Reset — Full State Wipe

`claude-gravity--reset-session(session)` clears all conversational state while keeping the same plist identity (pointer stability for buffer ownership).

Reset to initial values: `claude-status`, `start-time`, `plan`, `files`, `tasks`, `tool-index`, `agent-index`, `turns` (re-allocated with turn-0), `current-turn`, `total-tool-count`, `header-line-cache`, `permission-mode`, `slug`, `status`, `token-usage`, `prev-token-usage`.

`load-allow-patterns` is re-run.

### 2.6 Session End

On `SessionEnd` event:
1. Mark session ended via `model-session-end` (sets `:status` = `ended`, `:claude-status` = `idle`) — unless `:awaiting-clear` is set (suppress to keep session active)
2. Check if tmux process is still alive:
   - **Alive** → This is a `/clear`. Park session for re-key:
     - If `:temp-id` exists: move session back under temp-id in hash table, re-register in tmux-pending
     - Else: park in `tmux-rekey-sessions` keyed by tmux session name
   - **Dead** → Clean up tmux mapping
3. Remove all inbox items for this session
4. Clear notification indicator

---

## 3. Event–State Transition Matrix

All events pass through `claude-gravity-handle-event`. Before dispatch:
- Auto-register tmux association from bridge's `tmux_session` field
- Update PID, slug, branch via `model-update-session-meta`
- Self-heal ended sessions back to active (except on SessionEnd)
- On turn boundaries (UserPromptSubmit, Stop, SessionEnd): dismiss stale inbox items, clear turn-auto-approve for session

| Event | Session Effect | Claude Status | Turn Effect | Inbox Effect | Special Cases |
|-------|---------------|---------------|-------------|--------------|---------------|
| **SessionStart** | Reset existing or create new | idle | Turn-0 re-allocated | — | Re-keying (3 paths); auto-focus session buffer; model/effort captured |
| **SessionEnd** | `status` → `ended` | idle | — | Remove all for session | Tmux-alive → park for re-key; `awaiting-clear` suppresses ended marking |
| **UserPromptSubmit** | Ensure session | responding | New turn (freeze prev, increment `current-turn`, create turn-node with prompt) | Remove idle items | Dedup: skip if `tmux-prompt-sent`/`daemon-prompt-sent` flag set; XML-strip prompt text |
| **Stop** | — | idle | Finalize elapsed, set `stop_text`/`stop_thinking`, finalize token delta | Replace idle item with new one (turn + snippet) | Clear-and-proceed: sends `/clear` to tmux |
| **PreToolUse** | Ensure session, track file | responding | Add tool to current step (or new step) | — | AskUserQuestion → creates question prompt, advances turn; model name set |
| **PostToolUse** | Track file, track task | — | Complete tool (status→done, store result, duration) | — | AskUserQuestion → update answer; ExitPlanMode → set plan, create phase-boundary prompt, advance turn; token delta updated |
| **PostToolUseFailure** | Track file, track task | — | Complete tool (status→error, store error) | — | — |
| **SubagentStart** | Ensure session | — | Add agent to current turn's agents tlist, link to Task tool | — | Agent pre-allocates all `setf`-target keys as nil |
| **SubagentStop** | — | — | Complete agent (status→done, duration, stop_text, stop_thinking) | — | — |
| **Notification** | — | — | — | — | Logged only, no UI effect |
| **PermissionRequest** | (handled in socket filter) | — | (handled in socket filter) | Add item based on tool_name routing | See §5 |
| **StatusLine** | Store cost, context-pct, model, tokens, duration, lines | — | — | — | — |

### 3.1 Additional Events (OpenCode, Daemon, Internal)

These events are handled by `handle-event` but originate from non-standard bridges or internal mechanisms.

| Event | Source | Effect |
|-------|--------|--------|
| **SessionStatus** | OpenCode | Map OC status to claude-status; store title, slug, branch |
| **SessionIdle** | OpenCode | Set claude-status → idle, finalize last prompt |
| **VcsBranchUpdate** | OpenCode | Update `:branch` on session |
| **MessagePart** | OpenCode | Create/complete tools from `tool` parts; append text from `text` parts |
| **AssistantMessage** | OpenCode | Full assistant message: extract tools, thinking, text; set model |
| **SessionUpdate** | OpenCode | Update title, slug, branch, claude-status |
| **PermissionAutoApproved** | Bridge | Annotate existing tool with `auto_approved` flag |
| **StreamDelta** | Daemon | Append text/thinking to `:streaming-text`, set responding |
| **AssistantComplete** | Daemon | Clear `:streaming-text` |
| **DaemonResult** | Daemon | Clear streaming text, set idle, store token usage, finalize prompt |
| **DaemonError** | Daemon | Clear streaming text, set idle, log error |
| **ToolProgress** | OpenCode | Update tool with progress metadata (percentage, message) |

---

## 4. Turn Management

### 4.1 Turn Boundaries

Four paths advance turns (increment `current-turn`, freeze previous turn, create new turn-node):

1. **UserPromptSubmit** — User sends a new prompt → normal prompt (`❯`)
2. **ExitPlanMode** (PostToolUse) — Plan approved → phase-boundary prompt (`→`) with text `"[Plan approved]"`
3. **AskUserQuestion** (PreToolUse) — Question for user → question prompt (`?`) with the question text
4. **soft-rekey-session** — Clear-and-proceed re-key → phase-boundary prompt (`→`) with text `"Context cleared — implementing plan"`

Each tool, agent, and task captures the current turn number at creation time. The renderer uses these to group items under their turn.

### 4.2 Turn 0 — Pre-prompt Activity

Turn 0 is always pre-allocated at session creation. It has no prompt (`prompt` = nil). Any tools that execute before the first UserPromptSubmit belong to turn 0 and are displayed as "Pre-prompt activity" in the UI.

### 4.3 Step Grouping

Within a turn, tools are grouped into **response steps**. Each step represents one assistant message (thinking + text) followed by its tool calls.

**New step boundary** is created when (in `tree-add-tool`):
- No current step exists (first tool in turn/agent)
- New `assistant_text` is non-empty AND differs from current step's text AND is not a subsumption (paragraph-boundary prefix match)
- New `assistant_thinking` is non-empty AND differs from current step's thinking

**Dedup for parallel tool calls:** When a tool's `assistant_text` matches the current step's text exactly, the tool's `assistant_text` is cleared to nil (it would be redundant since all parallel tools share the same preceding text). Same for `assistant_thinking`.

**Subsumption check** (`text-subsumes-p`): Text A subsumes B if they are equal, or if one is a paragraph-boundary prefix of the other (`B + "\n\n"` is a prefix of A, or vice versa). This handles streaming text that grows across tool calls without creating spurious step breaks.

### 4.4 Agent Routing

Tools with a `parent_agent_id` are routed to the agent's own step tree (not the turn's root steps). The agent has its own `steps` tlist with the same step-node structure, enabling the same boundary logic.

- `parent_agent_id` = nil → root steps
- `parent_agent_id` = `"ambiguous"` → root steps (with `ambiguous` flag set on tool)
- `parent_agent_id` = valid agent ID → agent's steps (via `agent-index` lookup)

Agent steps use `agent-ensure-step` which applies the same subsumption and dedup rules as root steps.

### 4.5 Agent–Task Linking

When a new agent is added to the turn tree, `link-agent-to-task-tool` scans recent steps in reverse for an unlinked Task tool whose `subagent_type` matches the agent's `type`.

On match, a bidirectional link is established:
- Tool gets `agent` key → pointer to agent alist
- Agent gets `task-tool` key → pointer to Task tool alist

This allows the renderer to show agent branches nested under their spawning Task tool.

---

## 5. Bidirectional Flows

### 5.1 Protocol

Bidirectional hooks keep the bridge process alive until Emacs responds.

```
Claude Code                Bridge (one-shot)              Emacs
    │                          │                            │
    ├── hook invocation ──────▶│                            │
    │                          ├── JSON over socket ───────▶│
    │                          │   (PermissionRequest)      │
    │                          │                            │
    │                          │   socket stays open        │
    │                          │                            │
    │                          │◀── JSON response ─────────┤
    │                          │   {hookSpecificOutput:     │
    │◀── stdout ──────────────┤     {hookEventName:        │
    │   (bridge exits)         │      "PermissionRequest", │
    │                          │      decision: {...}}}    │
```

All hooks are fire-and-forget except `PermissionRequest` which waits for a response.

### 5.2 PermissionRequest Routing

The socket filter dispatches PermissionRequest based on `tool_name`:

| `tool_name` | Inbox Type | Handler |
|-------------|-----------|---------|
| `"AskUserQuestion"` | `question` | Opens question action buffer |
| `"ExitPlanMode"` | `plan-review` | Opens plan review buffer |
| (anything else) | `permission` | Opens permission action buffer |

All three paths add an inbox item with the socket `proc` stored for later response.

**Ignored sessions:** If the session has `:ignored` = t, an empty JSON response is sent immediately (passing control to the TUI).

### 5.3 Plan Review

Plan review is triggered by `ExitPlanMode` PermissionRequest.

**Buffer lifecycle:**
1. `handle-plan-review` creates `*Claude Plan Review: <label>*` buffer
2. Plan content is inserted, `plan-review-mode` minor mode activated
3. Original content stored for diff computation
4. `allowedPrompts` converted to `toolAlwaysAllow` permission entries

**State transitions:**

```
[buffer open] ──── C-c C-c (approve) ──────▶ [closed]
     │                                           │
     │         no feedback? → send {allow}       │
     │         has feedback? → auto-deny          │
     │                        with structured     │
     │                        feedback message    │
     │                                            │
     ├──── C-c C-k (deny) ────────────────▶ [closed]
     │         prompt for comment                 │
     │         bundle all feedback                │
     │         send {deny, message}               │
     │                                            │
     ├──── C-c C-l (approve+clear) ────────▶ [closed]
     │         send {allow}                       │
     │         set :awaiting-clear on session     │
     │                                            │
     └──── buffer killed ──────────────────▶ [closed]
              auto-deny: "buffer closed"
```

**Feedback detection** (triggers auto-deny on approve):
- Buffer text differs from original (edits)
- Inline comments exist (added via `C-c ;`)
- `@claude:` markers found in text

**Deny-as-approve workaround:** Claude Code ignores `allow` responses for ExitPlanMode (#15755). The bridge converts `{allow}` → `{deny, message: "User approved the plan. Proceed with implementation."}`. Emacs sends the correct `allow`; the bridge intercepts in `index.ts`.

### 5.4 Turn Auto-Approve

`claude-gravity--turn-auto-approve` is an alist of `(session-id . turn-number)` pairs.

When a permission request arrives for a session whose current turn matches a turn-auto-approve entry, the request is automatically approved without user interaction (send `allow` immediately).

**Set:** By the permission action buffer's "Allow for turn" action.

**Cleared:** On turn boundaries — `UserPromptSubmit`, `Stop`, and `SessionEnd` all remove the session's entry via `assoc-delete-all`.

---

## 6. Inbox System

### 6.1 Lifecycle

**Add:** PermissionRequest dispatch (§5.2) and Stop event (idle notification).

**Remove:** User acts on the item (approve/deny/dismiss), or session ends (`inbox-remove-for-session`).

**Dismiss stale:** On turn/session boundaries (UserPromptSubmit, Stop, SessionEnd), `dismiss-stale-inbox-items` removes items whose socket proc is dead.

### 6.2 Staleness Rule

An inbox item is stale **only when** its socket proc is dead:
- Bridge already exited (user handled in TUI, bridge crashed, or Claude Code killed the hook)
- Safe to dismiss — no one is waiting for a response

If the socket proc is **alive**, the bridge is still waiting. Do NOT dismiss:
- A late-arriving Stop can race with a plan-review item just added for the next tool call
- Killing live procs causes the bridge to hang

### 6.3 Action Buffers

Each inbox item type has an associated action buffer:

| Type | Buffer | Tracking |
|------|--------|----------|
| `permission` | `*Claude Permissions*` | `claude-gravity--inbox-action-buffers` hash table |
| `question` | `*Claude Question*` | Same hash table |
| `plan-review` | `*Claude Plan Review: <label>*` | Found by buffer name pattern |
| `idle` | (no buffer) | — |

On dismiss, `dismiss-single-inbox-item`:
1. Closes the bridge socket proc (if alive)
2. Kills the associated action buffer (via hash table lookup or name pattern)
3. For plan-review: removes `kill-buffer-hook` first to prevent double-send of deny

---

## 7. Refresh & Rendering Contract

### 7.1 Debounce

Two timer pools handle UI updates:

- **Overview timer** (`claude-gravity--refresh-timer`): Single timer for the `*Structured Claude Sessions*` buffer. Fires after `claude-gravity-refresh-interval` (0.1s) idle.
- **Per-session timers** (`claude-gravity--session-refresh-timers`): One timer per session-id for session detail buffers. Same idle interval.

Only visible buffers are re-rendered (`get-buffer-window buf t` check). Follow-mode buffers auto-tail after refresh.

### 7.2 Invariant

**Renderers are read-only.** They read from the session plist and turn tree, producing magit-section UI. They never mutate session state.

**Model functions are the only mutation path.** All state changes go through `claude-gravity-model-*` functions (in `claude-gravity-state.el`) or direct `plist-put` on the session. Model functions do NOT trigger UI refresh — that is the caller's (adapter's) responsibility via `schedule-refresh` / `schedule-session-refresh`.

This separation ensures:
- Both adapters (hooks and JSON-output) can coexist without conflicts
- The renderer can be tested in isolation
- No impedance mismatch: the model stores exactly what the UI needs to display
