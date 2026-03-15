# System Architecture: emacs-gravity

This document describes the overall system design, component interactions, and module structure.

## System Overview

```
Claude Code
    ↓ (hooks: PreToolUse, PostToolUse, PostToolUseFailure, Stop,
    │         UserPromptSubmit, SubagentStart, SubagentStop,
    │         SessionStart, SessionEnd, Notification, PermissionRequest)
emacs-bridge (Node.js, one-shot shim)
    ↓ hook socket (~/.local/state/gravity-hooks.sock)
gravity-server (TypeScript/Effect, long-running)
    ├── enrichment (transcript parsing, agent attribution)
    ├── state manager (sessions, turn tree, indexes, inbox)
    ├── event handler (hook → state mutations → semantic patches)
    ↓ terminal socket (~/.local/state/gravity-terminal.sock)
Emacs client (claude-gravity-client.el)
    ↓ read-replica plist tree
magit-section renderer
```

### Three-Tier Architecture

1. **Bridge shim** (one-shot): Receives hook data from Claude Code via stdin, forwards raw event to gravity-server's hook socket, exits. For bidirectional events (PermissionRequest, AskUserQuestion), keeps the socket open and waits for the server's response.

2. **gravity-server** (long-running): The stateful backend. Enriches events (transcript parsing, agent attribution), manages session state (turn tree, tool/agent indexes, inbox), and broadcasts semantic patches to all connected terminals.

3. **Emacs client** (long-lived connection): Connects to the terminal socket, receives session snapshots and incremental patches, maintains a local read-replica as plists, and renders via magit-section. Sends user actions (permission responses, plan review feedback) back to the server.

### Two-Socket Design

**Hook socket** (`~/.local/state/gravity-hooks.sock`):
- Bridge shims connect here, one connection per hook event
- Newline-delimited JSON: `{ event, session_id, cwd, pid, data, needs_response }`
- Fire-and-forget for most events; bidirectional for PermissionRequest/AskUserQuestion
- Override: `GRAVITY_HOOK_SOCK` environment variable

**Terminal socket** (`~/.local/state/gravity-terminal.sock`):
- Emacs (and future web/native clients) connect here, persistent connection
- Server → terminal: snapshots, patches, inbox events, overview refreshes
- Terminal → server: permission/question/plan-review actions, session/overview requests
- Override: `GRAVITY_TERMINAL_SOCK` environment variable

### Auto-Start

Hook scripts source `_ensure-server` which:
1. Checks if hook socket exists (fast path)
2. Spawns gravity-server if missing (atomic lock prevents duplicate spawns)
3. Waits up to 2s for socket to appear

## Monorepo Structure

```
package.json                     -- npm workspace root
packages/
  shared/                        -- Shared types and utilities
    src/
      types.ts                   -- Session, TurnNode, Tool, Agent, Patch, messages
      index.ts                   -- Re-exports
    package.json
  emacs-bridge/                  -- Claude Code plugin (thin shim)
    src/
      index.ts                   -- stdin → hook socket → stdout
      services/                  -- Effect services (ProcessIO, Fs, HookSocket)
    hooks/                       -- Shell scripts (one per hook event)
      _ensure-server             -- Auto-start gravity-server
    package.json
  gravity-server/                -- Stateful backend
    src/
      server.ts                  -- Entry, two sockets, message routing
      state/
        session-store.ts         -- Map<sessionId, Session>, project grouping
        session.ts               -- Session factory, mutation methods (emit patches)
        inbox.ts                 -- InboxManager, PendingResponse
      enrichment/
        enrich.ts                -- Event enrichment (transcript parsing)
        enrichment.ts            -- Pure extraction functions
        agent-state.ts           -- In-memory agent state (no file I/O)
      protocol/
        messages.ts              -- Protocol message types
        terminal-server.ts       -- Terminal connections, broadcast
        patch.ts                 -- Patch types, collector
      handlers/
        event-handler.ts         -- Hook event → enrichment → state mutations
        bidirectional.ts         -- Permission/question/plan-review flow
      util/
        log.ts                   -- Logging
    build.mjs                    -- esbuild → dist/server.mjs
    package.json
Makefile                         -- Build orchestration
```

## Terminal Protocol

### Server → Terminal Messages

```typescript
{ type: "session.snapshot", sessionId: string, session: Session }
{ type: "session.update", sessionId: string, patches: Patch[] }
{ type: "session.removed", sessionId: string }
{ type: "inbox.added", item: InboxItem }
{ type: "inbox.removed", itemId: number }
{ type: "overview.snapshot", projects: ProjectSummary[] }
```

### Terminal → Server Messages

```typescript
{ type: "action.permission", itemId: number, decision: "allow"|"deny", message?: string }
{ type: "action.question", itemId: number, answers: string[] }
{ type: "action.plan-review", itemId: number, decision: "allow"|"deny", feedback?: PlanFeedback }
{ type: "action.turn-auto-approve", sessionId: string }
{ type: "request.session", sessionId: string }
{ type: "request.overview" }
```

### Semantic Patches

Instead of JSON Patch (RFC 6902), the server emits **typed semantic operations** that map 1:1 to model mutations:

```typescript
type Patch =
  | { op: "set_status", status: "active" | "ended" }
  | { op: "set_claude_status", claudeStatus: "idle" | "responding" }
  | { op: "set_token_usage", usage: TokenUsage }
  | { op: "set_plan", plan: Plan | null }
  | { op: "set_streaming_text", text: string | null }
  | { op: "set_meta", slug?: string, branch?: string, pid?: number }
  | { op: "add_turn", turn: TurnNode }
  | { op: "freeze_turn", turnNumber: number }
  | { op: "set_turn_stop", turnNumber, stopText?, stopThinking? }
  | { op: "set_turn_tokens", turnNumber, tokenIn, tokenOut }
  | { op: "add_step", turnNumber, agentId?, step: StepNode }
  | { op: "add_tool", turnNumber, stepIndex, agentId?, tool: Tool }
  | { op: "complete_tool", toolUseId, result, status, duration?, postText? }
  | { op: "add_agent", agent: Agent }
  | { op: "complete_agent", agentId, stopText?, stopThinking?, duration? }
  | { op: "update_task", taskId, task: Task }
  | { op: "track_file", path, fileOp }
  | { op: "add_prompt", turnNumber, prompt: PromptEntry }
  | { op: "set_prompt_answer", turnNumber, toolUseId, answer }
```

**Why semantic over JSON Patch:**
- Terminals can render incrementally (e.g., `add_tool` → insert one magit section)
- Typed — terminals validate, unknown ops trigger full refresh
- Maps directly to both Emacs plist mutations and React state updates

## Hook System

Hook scripts in `packages/emacs-bridge/hooks/` are registered via `hooks.json`. Each hook sources `_ensure-server` (to auto-start gravity-server) then invokes `tsx src/index.ts <EventName>`. The system handles 11 event types:

### Session Lifecycle
- **SessionStart**: User starts a new Claude Code conversation or runs `/clear`
- **SessionEnd**: Claude Code conversation terminates (user exit, crash, timeout)

### Tool Lifecycle
- **PreToolUse**: Called before tool execution (can validate/block)
- **PostToolUse**: Called after successful tool completion
- **PostToolUseFailure**: Called when tool execution fails

### Agent Lifecycle
- **SubagentStart**: User or Claude launches a specialized agent
- **SubagentStop**: Agent completes (success or failure)

### User Interaction
- **UserPromptSubmit**: User sends a message to Claude (captured for display)
- **Stop**: Claude's generation completes or is interrupted
- **Notification**: Informational messages (e.g., permission granted)

### Bidirectional
- **PermissionRequest**: Bridge keeps socket open, waits for server response
  - Matcher: `ExitPlanMode` (when Claude Code exits plan mode)
  - Timeout: 96 hours
  - Response: routed through gravity-server inbox → terminal → user action → server → bridge

## Emacs Module Structure

The Emacs package is split into 15 modular files loaded via `claude-gravity.el` (thin loader):

| Module | Lines | Purpose | Key Functions |
|--------|-------|---------|---|
| `claude-gravity-core.el` | ~270 | defgroup, defcustom, logging, tlist | `claude-gravity-log`, `claude-gravity--tlist-*` |
| `claude-gravity-faces.el` | ~270 | 37 defface declarations + fringe bitmaps | `claude-gravity-face-*` |
| `claude-gravity-session.el` | ~285 | Session hash table, CRUD operations | `claude-gravity--session-get`, `claude-gravity--session-set` |
| `claude-gravity-discovery.el` | ~470 | Plugin/skill/agent/MCP capability discovery | `claude-gravity--discover-capabilities` |
| `claude-gravity-state.el` | ~845 | Model API, mutation functions (read-replica) | `claude-gravity--add-tool`, `claude-gravity--update-task` |
| `claude-gravity-events.el` | ~845 | Event dispatcher, hook handlers | `claude-gravity--handle-event` (all 11 hook types) |
| `claude-gravity-text.el` | ~480 | Text utilities: dividers, tables, markdown, wrapping | `claude-gravity--wrap-text`, `claude-gravity--render-plan` |
| `claude-gravity-diff.el` | ~690 | Inline diffs, tool display, plan revision diff | `claude-gravity--render-tool-diff` |
| `claude-gravity-render.el` | ~860 | Section renderers, turn grouping, agent/tool/task UI | `claude-gravity--insert-turn-section`, `claude-gravity--insert-tool` |
| `claude-gravity-ui.el` | ~2210 | Overview/session buffers, modes, keymaps, transient | `claude-gravity-status` (main entry point) |
| `claude-gravity-plan-review.el` | ~550 | Plan review buffer, comment overlays, feedback flow | `claude-gravity-plan-review-approve`, `claude-gravity-plan-review-deny` |
| `claude-gravity-client.el` | ~1010 | Terminal socket client to gravity-server | `claude-gravity-server-start`, `claude-gravity--apply-patch` |
| `claude-gravity-actions.el` | ~920 | Permission/question action buffers, inbox handling | `claude-gravity--show-permission-buffer` |
| `claude-gravity-tmux.el` | ~1320 | Tmux session management, compose buffer | `claude-gravity--tmux-start-session` |
| `claude-gravity.el` | ~35 | Thin loader: requires all modules | Entry point |

### Load Order (Dependency DAG)

```
core → {faces, session, discovery} → state → events → {text, diff} → render → ui
                                                                        ↓
                                                                    plan-review
                                                                        ↓
                                                                      client
                                                                        ↓
                                                                  {actions, tmux}
```

- `core` defines utilities, custom vars, tlist (no dependencies)
- `faces`, `session`, `discovery` depend on `core` only
- `state` depends on `core` and `session` (stores data in session hash table)
- `events` depends on `core` and `state` (updates state based on events)
- `text` and `diff` depend on `core` (pure text transformation)
- `render` depends on all above (renders turns, tools, tasks, agents)
- `ui` depends on `render` (main UI buffers and keymaps)
- `plan-review` depends on `ui` (plan review buffer)
- `client` depends on `plan-review` (terminal socket, patch application, server lifecycle)
- `actions` and `tmux` depend on `client` and `ui` (interactive buffers, server actions)

**Cross-module forward references:**
- Use `declare-function` for functions called before they're defined
- Use bare `defvar` for variables (will be defined elsewhere)

## State Management

### Server-Side (Authoritative)

gravity-server maintains the authoritative session state. Each mutation method returns `Patch[]`:

```typescript
interface Session {
  sessionId: string; cwd: string; project: string;
  status: "active" | "ended";
  claudeStatus: "idle" | "responding";
  turns: TurnNode[];
  toolIndex: Map<string, Tool>;      // O(1) by tool_use_id
  agentIndex: Map<string, Agent>;    // O(1) by agent_id
  tasks: Map<string, Task>;
  files: Map<string, FileEntry>;
  plan: Plan | null;
  tokenUsage: TokenUsage | null;
  totalToolCount: number;
  // ... more fields
}
```

### Client-Side (Read-Replica in Emacs)

Emacs maintains a read-replica as plists in `claude-gravity--sessions` hash table. The shape mirrors the server's Session object:

```
Session (plist)
├── :session-id, :cwd, :project, :slug, :status, :claude-status
├── :turns (tlist of turn-node alists)
├── :tool-index (hash-table: tool_use_id → tool alist)
├── :agent-index (hash-table: agent_id → agent alist)
├── :files, :tasks, :plan, :token-usage
└── ...
```

**Patch application** (`claude-gravity--apply-patch`): Each patch op maps to a `pcase` branch that mutates the local plist tree. The renderer reads the same plist structure as before — it doesn't know the data comes from patches.

**Model API** in `claude-gravity-state.el` still exists — the tree mutation functions are called by `apply-patch` instead of by event handlers directly.

See @docs/session-data-model.md for the complete plist reference.

## Design Patterns

### Bidirectional Flow (v3)

```
Bridge shim →(hook socket)→ gravity-server creates PendingResponse + InboxItem
gravity-server →(terminal socket)→ all terminals see inbox.added
Terminal (Emacs) →(terminal socket)→ server receives action (first responder wins)
Server writes response →(hook socket)→ bridge shim reads → stdout → Claude Code
```

Multiple terminals can view the same inbox item. First terminal to respond wins.

### Known Bug: ExitPlanMode Allow Ignored (Deny-as-Approve Workaround)

Claude Code silently ignores `allow` responses from PermissionRequest hooks for `ExitPlanMode`. This is a regression of [#15755](https://github.com/anthropics/claude-code/issues/15755), still broken as of v2.1.50.

**Workaround:** gravity-server intercepts `allow` responses for ExitPlanMode and converts them to `deny` with the message "User approved the plan. Proceed with implementation." Claude reads the message content, sees approval, and proceeds.

**Data flow:**
1. Emacs sends `action.plan-review` with `decision: "allow"` to server
2. Server converts to `{"decision":{"behavior":"deny","message":"User approved the plan. Proceed with implementation."}}`
3. Server writes response to bridge's hook socket
4. Bridge reads and writes to stdout for Claude Code

**Deny with feedback** (user annotated the plan) is already a deny — passes through unchanged.

**Removal:** When Claude Code fixes #15755, delete the workaround in gravity-server's bidirectional handler.

### Tool Attribution

Enrichment (now in gravity-server) handles tool-to-agent attribution:
1. Scans the transcript backward from each tool to find the most recent agent context
2. For single-agent cases, uses optimized path (no scanning)
3. Attributes tool to the correct agent's nested step

### PostToolUse Content Extraction

PostToolUse events extract both preceding and following assistant text:
1. **Preceding content** (`extractPrecedingContent`): text before the tool in the same turn
2. **Following content** (`extractFollowingContent`): text after the tool but before the next turn

### Agent Transcript Handling

Agent transcripts are stored in "sidechain" format (separate files):
1. Claude Code creates `agent_transcript_path` file during agent execution
2. SubagentStop event provides path but no `stop_text`/`stop_thinking`
3. gravity-server's enrichment layer detects sidechain format, extracts trailing content
4. Falls back to main transcript extraction if sidechain is empty
5. Emacs displays agent completion at turn level AND in nested agent section

## Migration History

**Phase 1 (Completed):** Extract model API
- Split monolithic Emacs code into modular files
- Define clean session state model in `claude-gravity-state.el`
- Adapt all renderers to read from model state

**Phase 2 (Completed):** Managed Claude Code subprocess
- `docs/emacs-driven-sessions.md`: spawning `claude -p` as managed subprocess
- Hooks adapter + managed process coexist

**Phase 3 (Completed — v3):** gravity-server backend
- Moved state management to long-running TypeScript backend
- Emacs becomes thin terminal client (read-replica + patch application)
- Bridge thinned to raw hook forwarder
- Enrichment moved from bridge to server (benefits from in-memory state)
- Two-socket architecture: hook socket + terminal socket
- Inbox manager for bidirectional flows
- `claude-gravity-socket.el` split into `claude-gravity-plan-review.el` + `claude-gravity-client.el`
- See @docs/refactor-implementation.md for full design rationale

**Daemon Bridge (ON HOLD — 2026-02):** Agent SDK daemon (`daemon.ts`, `daemon-session.ts`)
- **BLOCKED**: Agent SDK requires pay-per-use API key. See [#6536](https://github.com/anthropics/claude-code/issues/6536).
- Code exists but is not usable. Revisit if Anthropic changes policy.

## Related Documentation

- @DEVELOPMENT.md — Build commands, dependencies, debugging, testing
- @UI-SPEC.md — Visual specification for all UI states and keybindings
- @docs/refactor-implementation.md — v3 design: gravity-server architecture and terminal protocol
- @docs/session-data-model.md — Session plist structure and turn tree reference
- @docs/emacs-driven-sessions.md — Managed sessions research (historical)
- @docs/tmux-interactive-sessions.md — Tmux integration approach
