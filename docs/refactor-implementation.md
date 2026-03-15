# Plan: Stateful TypeScript Backend for emacs-gravity

## Context

Currently, Emacs is the "brain" — it maintains all session state (turn tree, tool/agent indexes, inbox, tasks, files) in hash tables, while the Node.js bridge is a stateless one-shot forwarder. This tightly couples the state model to Emacs Lisp, making it impossible to build alternative UIs (web, native).

**Goal:** Move state management to a long-running TypeScript backend. Emacs becomes a thin renderer that receives view model updates and sends user actions back. The terminal protocol is designed so web and native UIs can be built trivially.

**Outcome:** Any client that speaks the protocol can render the full gravity UI — Emacs, React web app, SwiftUI, terminal TUI.

---

## Design Decisions

- **Effect library**: Keep Effect throughout (consistent with bridge, structured errors, DI)
- **Code sharing**: npm workspace monorepo (gravity-server + emacs-bridge + shared types)
- **Backward compat**: Clean break — no standalone Emacs mode, server is required
- **Transport**: Unix domain socket with newline-delimited JSON (WebSocket added later for web)

## Architecture Overview

```
Claude Code (11 hooks)
    ↓
Bridge Shim (one-shot, thin — sends raw hook data)
    ↓ (hook socket)
gravity-server (TypeScript/Effect, long-running)
    ├── Enrichment (transcript parsing, agent attribution)
    ├── State Manager (sessions, turn tree, indexes, inbox)
    ├── Event Handler (hook → state mutations → patches)
    └── Terminal Protocol Server (Unix socket)
         ├── Emacs (thin renderer)
         ├── Web dashboard (future, via WebSocket)
         └── Native app (future, via WebSocket)
```

### Two Sockets

1. **Hook socket** (`~/.local/state/gravity-hooks.sock`) — bridge shims connect here, one-shot per event
2. **Terminal socket** (`~/.local/state/gravity-terminal.sock`) — terminals connect here, long-lived

---

## Terminal Protocol

### Transport

Unix domain socket with newline-delimited JSON (same as today's bridge→Emacs protocol). WebSocket upgrade path for web terminals later. Emacs connects via `make-network-process` (no new dependency).

### Message Types: Server → Terminal

```typescript
// Full session state (on connect, on session create)
{ type: "session.snapshot", sessionId: string, session: Session }

// Incremental update (on state change)
{ type: "session.update", sessionId: string, patches: Patch[] }

// Session removed
{ type: "session.removed", sessionId: string }

// Inbox item lifecycle
{ type: "inbox.added", item: InboxItem }
{ type: "inbox.removed", itemId: number }

// Overview refresh
{ type: "overview.snapshot", projects: ProjectSummary[] }
```

### Message Types: Terminal → Server

```typescript
// Permission response
{ type: "action.permission", itemId: number, decision: "allow"|"deny", message?: string }

// Question response
{ type: "action.question", itemId: number, answers: string[] }

// Plan review response
{ type: "action.plan-review", itemId: number, decision: "allow"|"deny", feedback?: PlanFeedback }

// Turn auto-approve
{ type: "action.turn-auto-approve", sessionId: string }

// Subscribe to session detail
{ type: "request.session", sessionId: string }

// Request overview
{ type: "request.overview" }
```

### Semantic Patches (the key design)

Instead of JSON Patch (RFC 6902) which is path-based and untyped, we use **semantic patches** — typed operations that map 1:1 to model mutations:

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
  | { op: "set_turn_stop", turnNumber: number, stopText?: string, stopThinking?: string }
  | { op: "set_turn_tokens", turnNumber: number, tokenIn: number, tokenOut: number }
  | { op: "add_step", turnNumber: number, agentId?: string, step: StepNode }
  | { op: "add_tool", turnNumber: number, stepIndex: number, agentId?: string, tool: Tool }
  | { op: "complete_tool", toolUseId: string, result: unknown, status: "done"|"error", duration?: number, postText?: string }
  | { op: "add_agent", agent: Agent }
  | { op: "complete_agent", agentId: string, stopText?: string, stopThinking?: string, duration?: number }
  | { op: "update_task", taskId: string, task: Task }
  | { op: "track_file", path: string, fileOp: string }
  | { op: "add_prompt", turnNumber: number, prompt: PromptEntry }
  | { op: "set_prompt_answer", turnNumber: number, toolUseId: string, answer: string }
```

**Why semantic over JSON Patch:**
- Terminals can render incrementally (e.g., `add_tool` → insert one magit section)
- Typed — terminals validate, unknown ops trigger full refresh
- Maps directly to both Emacs plist mutations and React state updates
- Multiple patches from one hook event batched into single `session.update`

---

## State Model (TypeScript port of `claude-gravity-state.el`)

```typescript
interface Session {
  sessionId: string;
  cwd: string; project: string;
  status: "active" | "ended";
  claudeStatus: "idle" | "responding";
  slug: string | null; branch: string | null;
  pid: number | null; modelName: string | null;
  startTime: number; lastEventTime: number;
  tokenUsage: TokenUsage | null;
  plan: Plan | null;
  streamingText: string | null;

  turns: TurnNode[];           // tree: turns → steps → tools
  currentTurn: number;
  toolIndex: Map<string, Tool>;     // O(1) by tool_use_id
  agentIndex: Map<string, Agent>;   // O(1) by agent_id
  tasks: Map<string, Task>;
  files: Map<string, FileEntry>;
  totalToolCount: number;
}

interface TurnNode {
  turnNumber: number;
  prompt: PromptEntry | null;
  steps: StepNode[];
  agents: Agent[];
  tasks: Task[];
  toolCount: number; agentCount: number;
  frozen: boolean;
  stopText: string | null; stopThinking: string | null;
  tokenIn: number | null; tokenOut: number | null;
}

interface StepNode { thinking: string | null; text: string | null; tools: Tool[] }
interface Tool { toolUseId: string; name: string; input: Record<string, unknown>; status: "running"|"done"|"error"; result: unknown; duration: number | null; assistantText: string | null; assistantThinking: string | null; postText: string | null; postThinking: string | null; parentAgentId: string | null; agent: string | null; /* agentId ref */ }
interface Agent { agentId: string; type: string; status: "running"|"done"; steps: StepNode[]; toolCount: number; stopText: string | null; stopThinking: string | null; duration: number | null; taskToolId: string | null; /* tool_use_id ref */ }
```

Direct 1:1 port of the Emacs plist model. Bidirectional links (Tool↔Agent) use ID references instead of object pointers.

---

## Bidirectional Flows

Bridge shim keeps its hook socket connection open for PermissionRequest/AskUserQuestion. The server holds a `PendingResponse` object mapping inbox item → hook socket connection. When a terminal responds, the server writes the response back to the shim's connection:

```
Shim →(hook socket)→ Server creates PendingResponse + InboxItem
Server →(terminal socket)→ Terminal shows permission/plan-review UI
Terminal →(terminal socket)→ Server receives action
Server writes response → Shim's hook socket connection
Shim reads response → stdout → Claude Code
```

Multiple terminals: all see the inbox item, first responder wins. DENY-AS-APPROVE workaround for ExitPlanMode (#15755) lives in the server.

---

## Monorepo Structure

```
package.json                     -- npm workspace root
packages/
  shared/                        -- Shared types and utilities
    src/
      types.ts                   -- Session, TurnNode, Tool, Agent, Patch, messages
      safe-bash.ts               -- (moved from emacs-bridge)
    package.json
  emacs-bridge/                  -- (existing, thinned out)
    src/
      index.ts                   -- Stripped to shim: stdin → hook socket → stdout
      services/                  -- Effect services (ProcessIO, Fs, HookSocket)
    package.json
  gravity-server/                -- NEW: stateful backend
    src/
      server.ts                  -- Entry, lifecycle, two sockets
      state/
        session-store.ts         -- Map<sessionId, Session>, project grouping
        session.ts               -- Session class, model mutation methods (emit patches)
        inbox.ts                 -- InboxManager, PendingResponse
      enrichment/
        enrich.ts                -- (moved from emacs-bridge, refactored for in-memory agent state)
        enrichment.ts            -- (moved from emacs-bridge, pure functions)
        agent-state.ts           -- (refactored: file I/O → in-memory Map)
      protocol/
        messages.ts              -- Protocol message type defs
        terminal-server.ts       -- Accept terminal connections, dispatch
        patch.ts                 -- Patch types, emission helpers
      handlers/
        event-handler.ts         -- Hook event → enrichment → state mutations
        bidirectional.ts         -- Permission/question/plan-review flow
      util/
        log.ts                   -- (reuse pattern from emacs-bridge)
    package.json
    tsconfig.json
```

---

## What Changes in Emacs

### Removed (clean break — no standalone mode)
- `claude-gravity-events.el` — event handling is server-side
- `claude-gravity-socket.el` socket server — replaced by client connection
- `claude-gravity-state.el` model mutation API — replaced by patch application
- `claude-gravity-session.el` session creation/CRUD — server creates sessions

### Added
- `claude-gravity-client.el` — Unix socket client connecting to gravity-server
  - Receives snapshots → deserialize JSON to plist tree (same shape renderer already reads)
  - Receives patches → apply to local plist tree (read replica)
  - Sends actions → serialize to JSON, write to socket
  - Inbox events → wire to existing action buffers
  - Server lifecycle: start/stop gravity-server as subprocess

### Unchanged
- `claude-gravity-render.el` — reads same plist structure, no changes
- `claude-gravity-ui.el` — buffers, keymaps, transient stay the same
- `claude-gravity-text.el`, `claude-gravity-diff.el` — text processing stays
- `claude-gravity-faces.el` — visual styling stays
- `claude-gravity-actions.el` — permission/plan-review UI stays (input source changes)
- `claude-gravity-core.el` — utilities, logging, tlist
- `claude-gravity-tmux.el` — tmux management (sends actions to server instead of direct socket)

### Patch Application (replaces model mutations)

```elisp
(defun claude-gravity--apply-patch (session-id patch)
  (let ((session (gethash session-id claude-gravity--sessions)))
    (pcase (alist-get 'op patch)
      ("add_tool" (claude-gravity--apply-add-tool session patch))
      ("complete_tool" (claude-gravity--apply-complete-tool session patch))
      ("set_claude_status" (plist-put session :claude-status (intern ...)))
      ;; ... one case per patch type
      )))
```

Same plist/hash-table structures the renderer already reads. The renderer doesn't know the difference.

---

## Bridge Shim Changes

The one-shot bridge stays (Claude Code requires it), but becomes thinner:

1. **Strip enrichment** — raw hook data forwarded to server
2. **Connect to hook socket** instead of Emacs terminal socket
3. **Bidirectional wait** — same as today, but on hook socket
4. **Fallback** — if server unavailable, write `{}` to stdout (same as today)

Enrichment code (enrich.ts, enrichment.ts, agent-state.ts) moves to the server where it benefits from in-memory agent state and cached transcript positions.

---

## Migration Phases

### Phase 1: Monorepo + Shared Types
- Set up npm workspace root with `packages/{shared,emacs-bridge,gravity-server}`
- Move shared types (Session, TurnNode, Tool, Agent, Patch, messages) to `packages/shared`
- Move `safe-bash.ts`, `types.ts` to shared
- Verify existing bridge still works with workspace imports

### Phase 2: Server Core — State + Event Handling
- Create `gravity-server/` with Effect-based architecture
- Port session store, turn tree, indexes from `claude-gravity-state.el` + `claude-gravity-session.el`
- Port event handler from `claude-gravity-events.el` (~680 lines → TypeScript)
- Move enrichment from shim to server (enrich.ts, enrichment.ts, agent-state.ts → in-memory)
- Server accepts hook connections on hook socket
- **Validation**: Bridge dual-writes to both server and Emacs. Compare server state vs Emacs state via MCP eval.

### Phase 3: Terminal Protocol + Inbox
- Implement terminal socket server (Unix domain socket)
- Implement snapshot serialization (Session → JSON)
- Implement patch emission: each model mutation emits typed patches
- Implement inbox manager with PendingResponse objects for bidirectional flows
- Build test terminal (CLI tool that connects and prints state changes)
- **Validation**: test terminal receives correct snapshots and patches for all 11 event types

### Phase 4: Emacs Client (Clean Break)
- Create `claude-gravity-client.el` — Unix socket client
- Implement snapshot → plist/hash-table deserialization
- Implement patch application (one `pcase` branch per patch op)
- Wire `inbox.added` → existing `claude-gravity-actions.el` buffers
- Wire action responses → server
- Emacs spawns gravity-server as subprocess on `claude-gravity-server-start`
- **Remove**: `claude-gravity-events.el`, socket server from `claude-gravity-socket.el`, model mutations from `claude-gravity-state.el`
- Strip bridge shim to raw forwarding only (no enrichment, no Emacs socket)
- **Validation**: full Claude Code session renders identically to old standalone mode

### Phase 5: Web Terminal (future, separate project)
- React app connecting via WebSocket (server adds WS endpoint)
- Receives same protocol, renders with whatever framework

---

## Reusable Existing Code

| File | Destination | Strategy |
|------|-------------|----------|
| `emacs-bridge/src/enrichment.ts` | `gravity-server/src/enrichment/` | Move — pure functions, no changes |
| `emacs-bridge/src/enrich.ts` | `gravity-server/src/enrichment/` | Move — pure functions, no changes |
| `emacs-bridge/src/agent-state.ts` | `gravity-server/src/enrichment/` | Move + refactor file I/O → in-memory Map |
| `emacs-bridge/src/safe-bash.ts` | `packages/shared/` | Move — pure function |
| `emacs-bridge/src/types.ts` | `packages/shared/` | Move + extend with protocol types |
| `emacs-bridge/src/services/` | `gravity-server/src/` | Reuse Effect service patterns for hook/terminal sockets |

## Server Lifecycle

1. **Start:** Emacs spawns via `start-process` (or user starts manually / systemd)
2. **Listen:** Hook socket + terminal socket
3. **Persist:** Periodic state dump to `~/.local/state/gravity-server/state.json` (30s)
4. **Health:** Periodic PID checks, staleness timeout → mark dead sessions ended
5. **Shutdown:** Final state dump, close sockets

---

## Verification

1. **Phase 1**: Run both server and standalone Emacs simultaneously. After N hook events, dump server state as JSON, compare against Emacs state (`claude-gravity--sessions` exported as JSON via MCP eval). States should match.

2. **Phase 2**: Connect test CLI terminal to server. Verify snapshots and patches arrive for all event types. Replay a recorded hook sequence, verify final state matches expected.

3. **Phase 3**: Run Emacs in client mode. Full UI should work identically — same rendering, same keybindings, same plan review workflow. Test: run a real Claude Code session, verify all turns/tools/agents/tasks render correctly.

4. **Bidirectional**: Test PermissionRequest, AskUserQuestion, plan review end-to-end with server in the middle. Verify DENY-AS-APPROVE workaround works.

---

## Critical Files

| File | Role in Migration |
|------|-------------------|
| `claude-gravity-state.el` | Port 17+ model mutation functions to TypeScript |
| `claude-gravity-events.el` | Port ~680 lines of event handling to TypeScript |
| `claude-gravity-session.el` | Session creation/cleanup logic moves to server |
| `claude-gravity-socket.el` | Server becomes client; bidirectional flow moves to server |
| `emacs-bridge/src/index.ts` | Strip enrichment, forward raw to server's hook socket |
| `emacs-bridge/src/enrich.ts` | Move to gravity-server, reuse as-is |
| `emacs-bridge/src/enrichment.ts` | Move to gravity-server, reuse as-is |
| `emacs-bridge/src/agent-state.ts` | Refactor file I/O → in-memory in server |
