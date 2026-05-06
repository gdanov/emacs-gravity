# System Architecture: emacs-gravity

This document describes the overall system design, component interactions, and module structure.

## System Overview

```
Claude Code
    ↓ (hooks: PreToolUse, PostToolUse, PostToolUseFailure, Stop,
    │         UserPromptSubmit, SubagentStart, SubagentStop,
    │         SessionStart, SessionEnd, Notification,
    │         PermissionRequest, AskUserQuestionIntercept)
emacs-bridge (Node.js, one-shot shim)
    ↓ hook socket (~/.local/state/gravity-hooks.sock)
gravity-server (TypeScript/Effect, long-running)
    ├── state manager (sessions, turn tree, indexes, inbox)
    ├── event handler (hook → enrichment → state mutations → semantic patches)
    ↓ terminal socket (~/.local/state/gravity-terminal.sock)
Terminal clients
    ├── Emacs client (claude-gravity-client.el)
    │       ↓ read-replica plist tree → magit-section renderer
    └── macOS menu bar (gravity-menubar, Swift)
            ↓ colored status dots + session dropdown
```

### Three-Tier Architecture

1. **Bridge shim** (one-shot): Receives hook data from Claude Code via stdin, forwards raw event to gravity-server's hook socket, exits. For bidirectional events (PermissionRequest, AskUserQuestion), keeps the socket open and waits for the server's response.

2. **gravity-server** (long-running): The stateful backend. Enriches events (transcript parsing, agent attribution), manages session state (turn tree, tool/agent indexes, inbox), and broadcasts semantic patches to all connected terminals.

3. **Terminal clients** (long-lived connections): Connect to the terminal socket, receive session snapshots and incremental patches.
   - **Emacs client**: Maintains a read-replica as plists, renders via magit-section. Sends user actions (permission responses, plan review feedback) back to the server.
   - **macOS menu bar** (`gravity-menubar`): Lightweight Swift app showing colored status dots per active session (green=idle, yellow=responding, orange=waiting) and a dropdown with session/inbox details. Read-only — no actions sent back to the server.

### Two-Socket Design

**Hook socket** (`~/.local/state/gravity-hooks.sock`):
- Bridge shims connect here, one connection per hook event
- Newline-delimited JSON: `{ event, session_id, cwd, pid, data, needs_response }`
- Fire-and-forget for most events; bidirectional for PermissionRequest/AskUserQuestion
- Override: `GRAVITY_HOOK_SOCK` environment variable

**Terminal socket** (`~/.local/state/gravity-terminal.sock`):
- Emacs and gravity-menubar (macOS menu bar) connect here, persistent connection
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
      gravity-server.ts           -- Entry, two sockets, message routing
      state/
        session-store.ts         -- Map<sessionId, Session>, project grouping
        session.ts               -- Session factory, mutation methods (emit patches)
        inbox.ts                 -- InboxManager, PendingResponse
      protocol/
        messages.ts              -- Protocol message types
        terminal-server.ts       -- Terminal connections, broadcast
        patch.ts                 -- Patch types, collector
      handlers/
        event-handler.ts         -- Hook event → enrichment → state mutations
        bidirectional.ts         -- Permission/question/plan-review flow
      util/
        log.ts                   -- Logging
    build.mjs                    -- esbuild → dist/gravity-server.mjs
    package.json
  gravity-menubar/               -- macOS menu bar app (Swift)
    GravityMenuBar/
      GravityMenuBarApp.swift    -- SwiftUI app, MenuBarLabel (status dots)
      GravityMonitor.swift       -- Terminal socket client, NDJSON parser
      Models.swift               -- View models, JSON protocol types
    Package.swift
Makefile                         -- Build orchestration
```

## Terminal Protocol

The terminal protocol supports two modes:
- **Pull mode** (default): Server sends lightweight signals, client fetches data on demand
- **Push mode**: Server broadcasts full payloads immediately

Enable push mode with `GRAVITY_PUSH_MODE=true` (legacy).

### Pull Mode Flow

```
Hook event → Server → state-changed signal (lightweight) → Terminal
                                     ↓
                    Client polls when idle → poll request
                                     ↓
                    Server → overview.snapshot + session-patches (full data)
```

This prevents UI freezes during heavy editing: the server only sends a small
signal while the client fetches data when it's idle.

### Server → Terminal Messages (Push mode)

```typescript
{ type: "session.snapshot", sessionId: string, session: Session }
{ type: "session.update", sessionId: string, patches: Patch[] }
{ type: "session.removed", sessionId: string }
{ type: "inbox.added", item: InboxItem }
{ type: "inbox.removed", itemId: number }
{ type: "inbox.snapshot", items: InboxItem[] }
{ type: "overview.snapshot", projects: ProjectSummary[] }
```

### Server → Terminal Messages (Pull mode)

```typescript
// Signals (lightweight, no payload)
{ type: "state-changed", what: "session"|"inbox"|"overview", sessionId?: string, seq: number }

// Responses to poll (full payload)
{ type: "session-patches", sessionId: string, seq: number, patches: Patch[] }
{ type: "inbox-items", items: InboxItem[] }
{ type: "overview.snapshot", projects: ProjectSummary[] }
```

### Terminal → Server Messages

```typescript
{ type: "hello", capabilities: string[] }
{ type: "action.permission", itemId: number, decision: "allow"|"deny", message?: string, updatedPermissions?: unknown[] }
{ type: "action.question", itemId: number, answers: string[] }
{ type: "action.plan-review", itemId: number, decision: "allow"|"deny", feedback?: PlanFeedback }
{ type: "action.turn-auto-approve", sessionId: string }
{ type: "request.session", sessionId: string }
{ type: "request.overview" }
{ type: "request.resync" }
{ type: "hint.session-dead", sessionId: string }
{ type: "poll" }  // Pull mode: request current state
```

### Patch Store (Pull Mode)

In pull mode, the server stores patches with sequence numbers per session:

```typescript
interface StoredPatch {
  seq: number;        // Global sequence number
  patch: Patch;         // The actual patch
  timestamp: number;   // When received
}
```

Clients can fetch patches since a given sequence number.

### Semantic Patches

Instead of JSON Patch (RFC 6902), the server emits **typed semantic operations** that map 1:1 to model mutations:

```typescript
type Patch =
  | { op: "set_status"; status: "active" | "ended" }
  | { op: "set_claude_status"; claudeStatus: "idle" | "responding" }
  | { op: "set_token_usage"; usage: TokenUsage }
  | { op: "set_plan"; plan: Plan | null }
  | { op: "set_streaming_text"; text: string | null }
  | { op: "set_permission_mode"; mode: string | null }
  | { op: "set_meta"; slug?: string; displayName?: string; branch?: string; pid?: number; modelName?: string; tmuxSession?: string }
  | { op: "add_turn"; turn: TurnNode }
  | { op: "freeze_turn"; turnNumber: number }
  | { op: "set_turn_stop"; turnNumber: number; stopText?: string; stopThinking?: string }
  | { op: "set_turn_tokens"; turnNumber: number; tokenIn: number; tokenOut: number }
  | { op: "add_step"; turnNumber: number; agentId?: string; step: StepNode }
  | { op: "add_tool"; turnNumber: number; stepIndex: number; agentId?: string; tool: Tool }
  | { op: "complete_tool"; toolUseId: string; result: unknown; status: "done" | "error"; duration?: number; postText?: string; postThinking?: string }
  | { op: "add_agent"; agent: Agent }
  | { op: "complete_agent"; agentId: string; stopText?: string; stopThinking?: string; duration?: number; transcriptPath?: string }
  | { op: "update_task"; taskId: string; task: Task }
  | { op: "track_file"; path: string; fileOp: string }
  | { op: "add_prompt"; turnNumber: number; prompt: PromptEntry }
  | { op: "set_prompt_answer"; turnNumber: number; toolUseId: string; answer: string }
```

**Why semantic over JSON Patch:**
- Terminals can render incrementally (e.g., `add_tool` → insert one magit section)
- Typed — terminals validate, unknown ops trigger full refresh
- Maps directly to both Emacs plist mutations and React state updates

## Hook System

Hook scripts in `packages/emacs-bridge/hooks/` are registered via `hooks.json`. Each hook sources `_ensure-server` (to auto-start gravity-server) then invokes `tsx src/index.ts <EventName>`. The system handles 12 event types:

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
- **AskUserQuestionIntercept**: Bridge keeps socket open, waits for user answer
  - Routes through inbox like PermissionRequest

## Emacs Module Structure

The Emacs package is split into 15 modular files loaded via `claude-gravity.el` (thin loader):

| Module | Lines | Purpose | Key Functions |
|--------|-------|---------|---|
| `claude-gravity-core.el` | ~280 | defgroup, defcustom, logging, tlist | `claude-gravity-log`, `claude-gravity--tlist-*` |
| `claude-gravity-faces.el` | ~270 | 37 defface declarations + fringe bitmaps | `claude-gravity-face-*` |
| `claude-gravity-session.el` | ~285 | Session hash table, CRUD operations | `claude-gravity--session-get`, `claude-gravity--session-set` |
| `claude-gravity-discovery.el` | ~1000 | Plugin/skill/agent/MCP capability discovery | `claude-gravity--discover-capabilities` |
| `claude-gravity-state.el` | ~475 | Session state helpers, inbox, tool/agent lookup | `claude-gravity--session-get`, `claude-gravity--apply-patch` |
| `claude-gravity-text.el` | ~480 | Text utilities: dividers, tables, markdown, wrapping | `claude-gravity--wrap-text`, `claude-gravity--render-plan` |
| `claude-gravity-diff.el` | ~685 | Inline diffs, tool display, plan revision diff | `claude-gravity--render-tool-diff` |
| `claude-gravity-render.el` | ~855 | Section renderers, turn grouping, agent/tool/task UI | `claude-gravity--insert-turn-section`, `claude-gravity--insert-tool` |
| `claude-gravity-ui.el` | ~2610 | Overview/session buffers, modes, keymaps, transient | `claude-gravity-status` (main entry point) |
| `claude-gravity-plan-review.el` | ~550 | Plan review buffer, comment overlays, feedback flow | `claude-gravity-plan-review-approve`, `claude-gravity-plan-review-deny` |
| `claude-gravity-actions.el` | ~920 | Permission/question action buffers, inbox handling | `claude-gravity--show-permission-buffer` |
| `claude-gravity-client.el` | ~1135 | Terminal socket client to gravity-server | `claude-gravity-server-start`, `claude-gravity--apply-patch` |
| `claude-gravity-tmux.el` | ~1295 | Tmux session management, compose buffer | `claude-gravity--tmux-start-session` |
| `claude-gravity-daemon.el` | ~695 | Agent SDK daemon bridge (ON HOLD) | `claude-gravity-daemon-start` |
| `claude-gravity-debug.el` | ~750 | Terminal protocol debug viewer | `claude-gravity-debug-open` |
| `claude-gravity.el` | ~35 | Thin loader: requires all modules | Entry point |

### Load Order (Dependency DAG)

```
core → {faces, session, discovery} → state → {text, diff} → render → ui
                                                                  ↓
                                                              plan-review
                                                                  ↓
                                                               actions
                                                                  ↓
                                                                client
                                                                  ↓
                                                            {tmux, daemon, debug}
```

- `core` defines utilities, custom vars, tlist (no dependencies)
- `faces`, `session`, `discovery` depend on `core` only
- `state` depends on `core` and `session` (stores data in session hash table)
- `text` and `diff` depend on `core` (pure text transformation)
- `render` depends on all above (renders turns, tools, tasks, agents)
- `ui` depends on `render` (main UI buffers and keymaps)
- `plan-review` depends on `ui` (plan review buffer)
- `actions` depends on `plan-review` and `ui` (permission/question action buffers)
- `client` depends on `actions` (terminal socket, patch application, server lifecycle)
- `tmux`, `daemon`, and `debug` depend on `client` (interactive buffers, server actions)

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

**Patch application** logic lives in `claude-gravity-client.el` (`claude-gravity--apply-patch`), with helper functions in `claude-gravity-state.el` for session state lookups, inbox management, and tool/agent index operations.

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
- Dead code cleanup: removed `claude-gravity-events.el` (event dispatcher, 846 lines) and ~270 lines of unused `claude-gravity-model-*` functions from `claude-gravity-state.el` — all event handling now lives in gravity-server

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
