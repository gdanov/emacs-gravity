# Pi Adapter for Gravity-Server

Design and implementation plan for integrating pi coding agent as a gravity-server driver.

## Overview

gravity-server manages session state and broadcasts patches to terminals (Emacs, macOS menu bar). Currently it receives events from Claude Code via a bridge shim. This design adds a **pi adapter** — gravity-server spawns and drives a pi subprocess, translating pi events into the same internal state mutations.

**Why pi?**
Because we want to support it.

**Key insight:** gravity-server's `handleEvent()` is event-format agnostic. The adapter translates pi events into `HookData` and calls `handleEvent()` directly — no new protocol needed.

## Architecture

```
┌─────────────────────────────────────────────────────────────────────┐
│                        gravity-server                                │
│                                                                      │
│  ┌──────────────┐    ┌─────────────────┐    ┌───────────────────┐  │
│  │ pi-driver/   │───▶│ hook-translator │───▶│ handleEvent()     │  │
│  │              │    │                 │    │ (existing)        │  │
│  │  spawn.ts    │    │ pi events       │    │                   │  │
│  │  protocol.ts │    │ → HookData       │    │ returns Patch[]   │  │
│  │  session.ts  │    │                 │    │                   │  │
│  └──────────────┘    └─────────────────┘    └─────────┬─────────┘  │
│                                                        │            │
│                         ┌──────────────────────────────▼─────────┐  │
│                         │           SessionStore                │  │
│                         │  (authoritative state + patch history) │  │
│                         └──────────────────────────────┬─────────┘  │
│                                                        │            │
│  ┌──────────────────┐                    ┌────────────▼─────────┐  │
│  │ pi session        │                    │    TerminalService    │  │
│  │ management        │                    │  (broadcasts patches) │  │
│  └──────────────────┘                    └─────────────────────┘  │
└─────────────────────────────────────────────────────────────────────┘
                            │
                            ▼
┌─────────────────────────────────────────────────────────────────────┐
│                    pi subprocess (RPC mode)                          │
│                                                                      │
│  pi --mode rpc --no-session  # uses configured defaults             │
│                                                                      │
│  stdin: JSONL commands (prompt, steer, abort, etc.)                  │
│  stdout: JSONL events (turn_start, tool_execution_end, etc.)        │
└─────────────────────────────────────────────────────────────────────┘
```

## Event Mapping

The adapter translates pi events into gravity state mutations. Pi gives us **explicit, unambiguous boundary events** at three levels — the mapping uses each one directly. There is no boundary reconstruction from heuristics, no cross-event dependency, no "the next event will close this".

### Pi's three boundary levels (per `pi-coding-agent/docs/json.md`)

| Pi level | Pi events | Meaning |
|---|---|---|
| Process | (subprocess spawn / exit) | One pi binary runs, can serve N user prompts |
| Agent run | `agent_start` / `agent_end` | One user-prompt cycle. Begins when pi starts processing a prompt, ends when the model stops calling tools |
| Inner LLM call | `turn_start` / `turn_end` | One model invocation inside an agent run. An agent run loops through N inner turns until the model returns no more `tool_calls` |

**`agent_start` is NOT a session lifetime event.** A long-lived pi process emits `agent_start` once per user prompt. This is the boundary signal we map to gravity's "turn".

### Boundary mapping (the spine)

| Pi boundary | Gravity action | Owned by |
|---|---|---|
| Subprocess spawn | `SessionStart` (synthesized eagerly by `startPiSession` in `gravity-server.ts`) | gravity-server |
| `agent_start` | **Open new turn** (calls `state/session.openTurn(session)` — creates an empty TurnNode, freezes the previous one if any). Emits `add_turn` + (if applicable) `freeze_turn` patches. | translator |
| `message_start` role=user (after `agent_start`) | **Attach prompt text** to the current turn (`state/session.attachPrompt(session, text)`). Emits `add_prompt` patch. Degrades cleanly: if the message never arrives or its text can't be parsed, the turn just shows up unlabeled — we don't lose the boundary. | translator |
| `agent_end` | **Close current turn**: set `stop_text`/`stop_thinking`, mark `frozen=true`, set token usage. Emits `set_turn_stop` + `freeze_turn` + `set_turn_tokens` patches. Token usage is **summed across all `AssistantMessage` entries** in `agent_end.messages[]` — each inner LLM call carries its own `usage`, and the gravity turn = the whole agent run, so the turn total is the sum. Read `stopReason` from the trailing `AssistantMessage`. There is **no** `agent_end.result.usage` field on the wire (legacy shape, kept defensively). | translator |
| Subprocess exit | `SessionEnd` | mod.ts |

### Tool & content events (within a turn)

| Gravity action                                                                                                                                                                                | Pi event                                                                                     | Notes                                                                                                                                                                                                                                                |
|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|----------------------------------------------------------------------------------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| snapshot pending text/thinking into per-tool slot                                                                                                                                             | `tool_execution_start`                                                                       | no patch yet                                                                                                                                                                                                                                         |
| emit `PreToolUse` + `PostToolUse` (or `PostToolUseFailure`) with the snapshotted assistant text and the post-tool text/thinking                                                               | `tool_execution_end`                                                                         | both events emitted on END so post-text is captured                                                                                                                                                                                                  |
| accumulate into `pendingAssistantText`                                                                                                                                                        | `message_update` text_delta                                                                  | flushed by next `tool_execution_start` or by `agent_end`                                                                                                                                                                                             |
| accumulate into `pendingAssistantThinking`                                                                                                                                                    | `message_update` thinking_delta                                                              | flushed by next `tool_execution_start` or by `agent_end`                                                                                                                                                                                             |
| (no-op)                                                                                                                                                                                       | `message_start` role=assistant                                                               | content arrives via `message_update` deltas                                                                                                                                                                                                          |
| (no-op for now)                                                                                                                                                                               | `message_end` role=assistant                                                                 | per `docs/rpc.md`, this carries the *final* assistant message including `usage` and `stopReason`. The current spine derives turn token usage from `agent_end.messages[]` instead; if/when we want per-step token accounting, this is where it lives. |
| (no-op)                                                                                                                                                                                       | `turn_start` / `turn_end`                                                                    | pi's inner LLM-call boundary has no gravity equivalent                                                                                                                                                                                               |
| emit `set_meta { modelName }` patch | `model_select` | **not in pi's documented event vocabulary** (`docs/json.md`, `docs/rpc.md`). Either an RPC-private/undocumented event or dead code. TODO: verify with a pi smoke run before relying on it. |
| update accumulator state; next turn carries the branch | `branch_update` | **not in pi's documented event vocabulary**. Same caveat as `model_select`.                                                                                                                                                                          |
| log; no patch                                                                                                                                                                                 | `error`                                                                                      |                                                                                                                                                                                                                                                      |
| emit `Compaction` → `addCompaction` primitive → `add_compaction` patch; marker pushed to `session.compactions[]`.                                                                             | `compaction_end`                                                                             | Captures reason/turnNumber/tokensBefore/summary/aborted. Does NOT move turn boundaries — marker attaches to whichever turn is current when compaction completes.                                                                                     |
| (log to stderr; no patches)                                                                                                                                                                   | `queue_update`, `compaction_start`, `auto_retry_start` / `auto_retry_end`, `extension_error` | Documented in `docs/rpc.md`. `compaction_start` is informational — the marker is recorded only on `_end`.                                                                                                                                            |
| emit `ToolPartial` → `update_tool_partial` patch; latest partial lands on `tool.partial`. Also stashed on accumulator so `accToolEnd` can fall back to it if pi's `_end` carries no `result`. | `tool_execution_update`                                                                      | Cumulative-snapshot model: each `_update` replaces (does not append). Terminals choose how to render `tool.partial`; CC tools always have `partial: null`.                                                                                           |

### Why this mapping (vs the old one)

Pi already tells us, at zero ambiguity, where every gravity boundary lives. The old design routed `agent_start` → `SessionStart` and reconstructed the turn boundary downstream from `message_start` content shape. That had two structural problems:

1. **`agent_start`'s information was discarded.** `SessionStart` is conceptually wrong (pi can emit `agent_start` N times in one process lifetime), and the server had to special-case "skip reset for pi" to keep it benign. The actual turn boundary was reconstructed in a different code path entirely.
2. **`agent_end` didn't close the turn.** Stop set `stop_text` but never marked the turn frozen. Freezing happened lazily in `addPrompt` of the *next* prompt — meaning a pi process that emitted `agent_end` and waited for the user (or never received another prompt) would leave the turn open indefinitely. Subsequent activity on the same un-frozen turn (e.g. a second prompt whose `message_start` parsing failed) would silently extend the previous turn instead of starting a new one. This was observed in practice: pi emitted Stop, the user sent a second prompt whose user-message content arrived as a string (translator only handled array form), no `UserPromptSubmit` fired, `agent_start` was a no-op `SessionStart`, and 46 tools from the second prompt landed in the first prompt's still-open turn. The first prompt's `stop_text` was overwritten by the second's.

The new mapping makes each gravity state mutation owned by exactly one pi event, with no cross-event dependency:

- `agent_start` always opens a turn, even before any prompt text arrives.
- `agent_end` always closes the turn, regardless of what comes next.
- `message_start` only *enriches* the turn with a label; missing prompt text degrades to "untitled turn", not "lost turn".

Result: the state machine is monotonically correct. Malformed or missing pi events degrade locally instead of corrupting structure.

### Content Extraction

Pi exposes token-by-token streaming via `message_update` events:

```typescript
// message_update: text_delta
{ assistantMessageEvent: { type: "text_delta", delta: "Hello " } }

// message_update: thinking_delta
{ assistantMessageEvent: { type: "thinking_delta", delta: "Let me check" } }
```

The adapter accumulates deltas and attaches them to the next tool as `assistant_text` / `assistant_thinking`. Tool results may also carry post-tool thinking, extracted as `post_tool_text` / `post_tool_thinking`. Any deltas left in the accumulator at `agent_end` become the turn's `stop_text` / `stop_thinking`.

### User message text extraction

Pi's `UserMessage.content` is `string | (TextContent | ImageContent)[]` (per `pi-coding-agent/docs/session.md`). The translator must accept **both** forms:

```typescript
case "message_start": {
  const msg = e.message;
  if (msg?.role === "user") {
    const text =
      typeof msg.content === "string"
        ? msg.content
        : Array.isArray(msg.content)
          ? msg.content.filter(c => c?.type === "text" && typeof c.text === "string")
                       .map(c => c.text as string)
                       .join("")
          : "";
    if (text) attachPromptText(state, text);   // attach to current turn (already opened by agent_start)
  }
  return { kind: "noop" };
}
```

If `text` is empty, the turn stays unlabeled — the boundary is still in place because `agent_start` already opened it.

### Turn lifecycle: the full picture

```
┌─ pi process spawn ─────────────────────────────┐  → SessionStart (eager, server-side)
│                                                 │
│  ┌─ agent_start ──────────────────────────────┐ │  → openTurn(s)               [add_turn, freeze_turn(prev)]
│  │   message_start role=user content=…        │ │  → attachPromptText(s, text) [add_prompt]
│  │   turn_start                                │ │  → (no-op)
│  │     message_start role=assistant            │ │  → (no-op)
│  │     message_update text_delta "Let me…"    │ │  → accumulate
│  │     tool_execution_start  Read /foo        │ │  → snapshot pending text
│  │     tool_execution_end    {…}              │ │  → emit PreToolUse + PostToolUse
│  │     message_update text_delta "Now I…"     │ │  → accumulate
│  │     tool_execution_start  Edit /foo        │ │
│  │     tool_execution_end    {…}              │ │  → emit PreToolUse + PostToolUse
│  │     …                                       │ │
│  │   turn_end                                  │ │  → (no-op)
│  │   turn_start                                │ │  → (no-op — pi's inner loop iteration)
│  │     …more tools…                            │ │
│  │   turn_end                                  │ │
│  └─ agent_end messages=[…AssistantMessage]    ─┘ │  → closeTurn(s, stopText, stopThinking, usage)
│                                                  │     usage = trailing AssistantMessage.usage
│                                                  │     [set_turn_stop, freeze_turn, set_turn_tokens]
│                                                  │
│  ┌─ agent_start (next prompt) ──────────────── ┐ │  → openTurn(s)               [add_turn]  (prev already frozen)
│  │   …                                          │ │
│  └─ agent_end ────────────────────────────────  ┘ │  → closeTurn
└─ pi process exit ──────────────────────────────── ┘  → SessionEnd
```

Three crisp boundaries (process / agent / tool), one event per boundary, no reconstruction.

## Effort Level Control

pi exposes thinking levels: `off`, `minimal`, `low`, `medium`, `high`, `xhigh`.

gravity-server tracks effort via `effort_level` in HookData.

### Mapping

```typescript
const EFFORT_FROM_THINKING: Record<string, string> = {
  off: "low",
  minimal: "low",
  low: "medium",
  medium: "medium", 
  high: "high",
  xhigh: "high",  // cap at "high"
};
```

### Control Surface

1. **Spawn flag**: `--thinking <level>` passed to pi subprocess
2. **Runtime control**: `set_thinking_level` RPC command
3. **Terminal request**: Emacs sends effort change via `pi.set-thinking` message

## Session Coordination

### Mode: Adapter-Owns Session

The adapter creates its own session ID and pi runs in `--no-session` mode (ephemeral):

```typescript
// spawn.ts
const sessionId = generateSessionId();

const pi = spawn("pi", [
  "--mode", "rpc",
  "--no-session",
  "--thinking", thinkingLevel,
  "--cwd", cwd,
]);
```

**No provider or model specified by default.** pi uses its own configured defaults from `settings.json`, `auth.json`, or environment variables. This keeps the adapter configuration-minimal and lets users control models via pi's normal configuration flow.

If explicit override is needed, pass via adapter options:
```typescript
startPiDriver({ model: "claude-opus-4-5", provider: "anthropic" })
```

Benefits of adapter-owns-session:
- No session file format compatibility needed
- Full control over session lifecycle
- Clean separation of concerns

## File Structure

```
packages/gravity-server/src/
├── pi-driver/                    # pi integration
│   ├── mod.ts                   # Main entry, exports startPiDriver()
│   ├── index.ts                 # Re-export startPiDriver
│   ├── spawn.ts                 # Spawn and manage pi subprocess
│   ├── protocol.ts              # Parse pi JSONL events, send RPC commands
│   ├── session.ts               # Session sync, effort level, metadata
│   ├── hook-translator.ts       # pi events → HookData translation
│   ├── turn-accumulator.ts      # Batch pi turns into gravity turns
│   └── types.ts                 # PiEvent types, translation result
│
└── handlers/
    ├── event-handler.ts         # (existing)
    └── bidirectional.ts         # (existing)

packages/gravity-server/test/
└── pi-driver.test.ts           # Unit tests (40 tests)
```

## Environment Variables

| Variable | Description | Default |
|----------|-------------|---------|
| `PI_BINARY_PATH` | Path to pi binary | `pi` (PATH lookup) |
| `PI_CWD` | Working directory | Inherited from parent |

## Usage

### From Emacs Client

```elisp
;; Start a new pi session
(claude-gravity--pi-start "/path/to/project")
(claude-gravity--pi-start "/path/to/project" "high")

;; Send a prompt
(claude-gravity--pi-prompt "Hello, help me with this task")

;; Send steering message
(claude-gravity--pi-steering "Try a different approach")

;; Abort current execution
(claude-gravity--pi-abort)

;; Change thinking level
(claude-gravity--pi-set-thinking "low")

;; Check status
(claude-gravity--pi-status)
```

### From Command Line

```bash
# Auto-start pi session with gravity-server
gravity-server --pi --pi-cwd /path/to/project --pi-thinking high

# Or via environment variables
GRAVITY_HOOK_SOCK=/tmp/hooks.sock gravity-server --pi
```

## Terminal Messages

Emacs client communicates with gravity-server via terminal socket messages:

```typescript
// Start a new pi session
{ type: "pi.start", cwd?: string, thinkingLevel?: string }

// Send a prompt
{ type: "pi.prompt", sessionId: string, text: string, images?: string[] }

// Send steering message
{ type: "pi.steer", sessionId: string, text: string }

// Abort execution
{ type: "pi.abort", sessionId: string }

// Change thinking level
{ type: "pi.set-thinking", sessionId: string, level: string }
```

## Status

Implemented:

- [x] **pi-driver/types.ts** — PiEvent union type, TranslationResult, AccState
- [x] **pi-driver/protocol.ts** — JSONL parser and RPC command formatter
- [x] **pi-driver/spawn.ts** — Subprocess spawning with lifecycle management
- [x] **pi-driver/session.ts** — Effort level mapping and metadata sync
- [x] **pi-driver/mod.ts** — startPiDriver() composition
- [x] **pi-driver/index.ts** — Re-export startPiDriver
- [x] **Wire into gravity-server.ts** — `--pi` flag, terminal message handlers, multi-driver `Map<sessionId, Driver>`
- [x] **Emacs client functions** — `claude-gravity--pi-start`, `claude-gravity--pi-prompt`, etc., per-session
- [x] **Multi-session pi** — N concurrent pi processes, one per gravity session
- [x] **Extension UI bridge** — `confirm`/`select` routed through gravity inbox

### Spine primitives (implemented)

Three primitives in `state/session.ts` own all turn-boundary mutations:

- `openTurn(session)` — creates an empty `TurnNode`, freezes the previous turn if any (the defensive freeze guards against the restart hazard). Emits `freeze_turn` (prev, if applicable) + `add_turn`.
- `attachPrompt(session, entry)` — attaches a prompt to the current (last) turn. Does NOT create a new turn. Idempotent: if the current turn already has a prompt, no-op (boundary survives, no relabel).
- `closeTurn(session, { stopText, stopThinking, tokenIn, tokenOut })` — stamps stop text/thinking, records tokens, sets `frozen=true`. Emits `set_turn_stop` + `set_turn_tokens` (if usage) + `freeze_turn`.

`addPrompt` is now `openTurn + attachPrompt` (used by Claude Code's UserPromptSubmit where boundary + label arrive atomically). `finalizeLastPrompt` is now `closeTurn` (used by CC's Stop), so CC's Stop path freezes the turn too — the latent bug masked by CC's tight prompt cadence is fixed.

Done (boundary mapping wired end-to-end):

- [x] **Spine primitives in `state/session.ts`** — `openTurn`, `attachPrompt`, `closeTurn` introduced.
- [x] **`agent_start` opens a turn directly** — `accAgentStart` emits `TurnOpen` (new internal hook event), handler calls `openTurn`. `SessionStart` for pi is synthesized exactly once by `startPiSession`, never per prompt.
- [x] **`agent_end` freezes the turn** — `accAgentEnd` emits `TurnClose`, handler calls `closeTurn(stopText, stopThinking, { tokenIn, tokenOut })`. Token usage extracted from `agent_end.messages[]` trailing AssistantMessage (camelCase→snake_case mapping); legacy `result.usage` kept as defensive fallback.
- [x] **`message_start` accepts `content: string`** — translator handles both string and array forms per pi's `UserMessage` shape.
- [x] **Removed the `isPi` special case** in `event-handler.ts:handleSessionStart`.
- [x] **Freeze on Stop for Claude Code** — `finalizeLastPrompt` routed through `closeTurn`, so CC's Stop freezes the turn.
- [x] **Restart guard via defensive freeze** — `openTurn` freezes any prior unfrozen turn. If a pi process dies mid-turn and is respawned, the next `agent_start` cannot stack a new turn on top of a stale unfrozen one.
- [x] **Pending UI dialog cleanup on pi exit** — already implemented in `gravity-server.ts`'s `onLifecycle` "stop"/"error" branch: iterates `pendingPiUIResponses`, filters by `sessionId`, removes each inbox item and broadcasts `inbox.removed`.

### Remaining gaps and parity TODOs

- [x] **Unmapped documented events** — explicit no-op handlers added for `queue_update`, `compaction_start`, `auto_retry_start`/`auto_retry_end`, `extension_error`. They log to stderr today; no patches emitted.
- [x] **`compaction_end` recorded as session marker.** Emits new internal `Compaction` event → `addCompaction` primitive → `add_compaction` patch → `session.compactions[]`. Marker captures `{reason, turnNumber, timestamp, tokensBefore, summary, aborted}` and does not move turn boundaries (mid-stream compaction during `threshold`/`overflow` keeps the same turn open). Terminals can group markers by `turnNumber` for inline rendering, or surface as a session-level sidebar. Aborted compactions are recorded with `aborted: true` for debug visibility. Turn 0 compactions get `turnNumber: -1` (no user turn opened yet). Token totals untouched — `get_session_stats` already reflects post-compaction reality, the marker only records the `tokensBefore` snapshot.
- [x] **`agent_end.messages[]` `stopReason` surface.** The trailing `AssistantMessage`'s `stopReason` (one of `"stop"`, `"length"`, `"toolUse"`, `"error"`, `"aborted"`) is now plumbed through `TurnClose` hookData (`stop_reason` field) into `closeTurn`'s `opts.stopReason`, which writes it to `turn.stopReason` and emits an extended `set_turn_stop` patch. Terminals can read `stopReason` off the turn to distinguish "model stopped" from "budget exhausted" from "abort" — UI rendering is the terminal's call.
- [ ] **`message_end role=assistant` → per-step token accounting.** Pi emits `message_end` with full `usage` and `stopReason` for every inner LLM call. If we ever want per-step (not just per-turn) token attribution, this is the hook. Not needed for spine.
- [ ] **`model_select` / `branch_update` provenance.** Annotated in code as extension-channel events (pi's source emits them via `_extensionRunner.emit`, not as core session events). Handlers retained defensively. Verify by running with verbose stdout whether these actually appear in pi 0.74's RPC-mode output; delete if not.
- [x] **`tool_execution_update` streaming output.** Each `_update` now flows through a new internal `ToolPartial` hook event → `updateToolPartial` primitive → `update_tool_partial` patch. The latest partial lands on `tool.partial` (cumulative-snapshot model, replaced on each update). Terminals can render live progress or ignore the patches; CC tools simply leave `partial: null`. As a defensive fallback, the translator also stashes the latest partial on `AccState.currentToolPartial` so `accToolEnd` can synthesize a result if pi's `tool_execution_end` carries an empty `result` (tool-dependent behavior). Late `_update`s arriving after `_end` are dropped.

## Feature Parity with Claude Code

Pi differs from Claude Code in *how* it surfaces interactive review:

| gravity feature (Claude Code) | pi equivalent |
|---|---|
| `ExitPlanMode` / plan review buffer | none — pi has no plan-mode concept |
| `PermissionRequest` (tool gating) | **via extensions** — pi extensions use `ctx.ui.confirm` to gate; routed through gravity's permission inbox (see Extension UI bridge below) |
| `AskUserQuestion` tool | **via extensions** — pi extensions use `ctx.ui.select`; routed through gravity's question inbox |
| Free-text input dialogs | pi `ctx.ui.input` / `ctx.ui.editor` — auto-cancelled by the adapter for now (no matching gravity surface yet) |
| Allow-pattern management / `settings.local.json` | n/a — pi extensions decide on a per-call basis; no equivalent of CC's pattern protocol |
| `SubagentStart` / `SubagentStop` | none — pi 0.74 has no sub-agent event vocabulary |

### Extension UI bridge

Pi extensions that need user input emit `extension_ui_request` events on stdout with a `method` and a unique `id`; for dialog methods the client must reply with `extension_ui_response` on stdin keyed by the same `id`. The adapter routes the dialog methods into gravity's existing inbox:

- **`confirm`** (`{title, message}`) → `permission` inbox item. Allow/deny → `{confirmed: true|false}` to pi.
- **`select`** (`{title, options[]}`) → `question` inbox item. First answer → `{value: <choice>}` to pi.
- **`input` / `editor`** → auto-cancelled today (TODO — needs a text-entry surface in Emacs).
- Fire-and-forget methods (**`notify`**, **`setStatus`**, **`setWidget`**, **`setTitle`**, **`set_editor_text`**) are logged on the server for now; UI integration (status bar, transient notifications, window title) is a follow-up.

When pi exits, any pending UI dialogs are dropped from the inbox (pi is gone, there's no one to respond to).

This means **pi extensions that use `ctx.ui.confirm` / `ctx.ui.select` get the same gravity allow/deny + multiple-choice flow as Claude Code's `PermissionRequest` / `AskUserQuestion`.** What's "inert by capability" is narrower than before — primarily plan review and the pattern-allowlist machinery.

## Non-Goals (Deferred)

- pi session file compatibility — adapter owns session
- Explicit provider/model override — pi uses its configured defaults

## No longer non-goals (now supported)

- **Multiple concurrent pi sessions.** N pi processes can run in parallel, one per gravity session. Server tracks them in `piDrivers: Map<sessionId, Driver>`; every pi RPC takes a `sessionId`. See the multi-session refactor for details.

## Alternatives Considered

### Option A: pi-as-driver (chosen)
- gravity-server spawns pi subprocess
- Adapter translates events → handleEvent()
- Clean separation, reuses existing state management

### Option B: Managed pi session
- pi maintains session file
- Bridge reads session file, forwards to gravity-server
- Requires session file format compatibility

### Option C: Extension mode
- pi loaded as gravity-server extension
- Shares process memory
- Requires pi SDK integration into Node/Efect context
