# Pi Adapter for Gravity-Server

Design and implementation plan for integrating pi coding agent as a gravity-server driver.

## Overview

gravity-server manages session state and broadcasts patches to terminals (Emacs, macOS menu bar). Currently it receives events from Claude Code via a bridge shim. This design adds a **pi adapter** — gravity-server spawns and drives a pi subprocess, translating pi events into the same internal state mutations.

**Why pi?**
- Model-agnostic: works with Anthropic, OpenAI, Google, DeepSeek, etc.
- Managed context: built-in compaction, session management
- Extensible: skills, prompt templates, extensions
- Active development with solid SDK

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
│  │  protocol.ts │    │ → HookData      │    │ returns Patch[]   │  │
│  │  session.ts  │    │                 │    │                   │  │
│  └──────────────┘    └─────────────────┘    └─────────┬─────────┘  │
│                                                        │            │
│                         ┌──────────────────────────────▼─────────┐  │
│                         │           SessionStore                │  │
│                         │  (authoritative state + patch history) │  │
│                         └──────────────────────────────┬─────────┘  │
│                                                        │            │
│  ┌──────────────────┐                    ┌────────────▼─────────┐  │
│  │ bidirectional.ts │                    │    TerminalService    │  │
│  │ (extends for pi) │                    │  (broadcasts patches) │  │
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

The adapter translates pi events to gravity-server's `HookData` format, then calls `handleEvent()`.

### Event Translation Table

| Pi Event | Gravity Event | Notes |
|----------|--------------|-------|
| `agent_start` | `UserPromptSubmit` | On first pi turn after user sends prompt |
| `tool_execution_start` | `PreToolUse` | Extracts assistant_text, assistant_thinking from streaming |
| `tool_execution_end` | `PostToolUse` | Extracts post_tool_text, post_tool_thinking from result |
| `tool_execution_end` (error) | `PostToolUseFailure` | Sets error flag |
| `agent_end` | `Stop` | Finalizes turn, emits token usage |
| — | `SessionStart` | On adapter startup (if not resuming) |
| — | `SessionEnd` | On adapter shutdown |

### Content Extraction

pi exposes token-by-token streaming via `message_update` events:

```typescript
// message_update: text_delta
{ assistantMessageEvent: { type: "text_delta", delta: "Hello " } }

// message_update: thinking_delta  
{ assistantMessageEvent: { type: "thinking_delta", delta: "Let me check" } }
```

The adapter accumulates these deltas and attaches them to the next tool as `assistant_text` / `assistant_thinking`.

Similarly, tool results may include thinking content that the adapter extracts as `post_tool_text` / `post_tool_thinking`.

### Turn Alignment

| Concept | Pi | Gravity |
|---------|-----|---------|
| Turn boundary | `turn_end` (one assistant response) | `addPrompt` → `Stop` (user prompt → response cycle) |
| Steps | N/A | `StepNode` = one assistant response |

**One gravity turn** = one user prompt + zero or more pi turns (tool calls within that response cycle).

```
pi: agent_start (user prompt)
    ↓
pi: turn_start (pi turn 1)
    ↓
pi: tool_execution_start (bash ls)
    ↓
pi: tool_execution_end
    ↓
pi: turn_end
    ↓
pi: turn_start (pi turn 2 — only if more tool calls)
    ↓
... (more pi turns)
    ↓
pi: agent_end (done)
    ↓
adapter: emit gravity turn (combines all pi turns since agent_start)
```

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
3. **Terminal request**: Emacs sends effort change via terminal socket → adapter applies

## Bidirectional Flow (Permission/Question)

When pi encounters a permission request or question tool:

```
pi: tool_execution_start (with needs_response=true)
    ↓
adapter: detects special tool (PermissionRequest, AskUserQuestion)
    ↓
adapter: sends pause signal to pi (via RPC)
    ↓
adapter: calls handleEvent() with appropriate event type
    ↓
gravity-server: inbox.add() → broadcasts to terminals
    ↓
user responds via Emacs (action.permission / action.question)
    ↓
gravity-server: inbox.respond() returns response
    ↓
adapter: formats response as tool result, sends to pi
    ↓
pi: resumes, tool result available
```

### Permission Tool Detection

Standard pi tools don't have permission requests — the adapter can inject permission-gate behavior via extension, or the user can configure pi's built-in safety settings.

For this implementation, we assume no permission flow needed (pi handles permissions natively).

## Session Coordination

### Mode: Adapter-Owns Session

The adapter creates its own session ID and pi runs in `--no-session` mode (ephemeral):

```typescript
// spawn.ts
const sessionId = generateSessionId();


const pi = spawn("pi", [
  "--mode", "rpc",
  "--no-session",
  `--thinking`, thinkingLevel,
  "--cwd", cwd,
]);
```

**No provider or model specified by default.** pi uses its own configured defaults from `settings.json`, `auth.json`, or environment variables. This keeps the adapter configuration-minimal and lets users control models via pi's normal configuration flow.


If explicit override is needed, pass via environment variables:
```bash
PI_PROVIDER=anthropic PI_MODEL=claude-sonnet-4-5 pi --mode rpc ...
```

Or via adapter options (optional):
```typescript
startPiDriver({ model: "claude-opus-4-5", provider: "anthropic" })
```

Benefits of adapter-owns-session:
- No session file format compatibility needed
- Full control over session lifecycle
- Clean separation of concerns

### Session Metadata

The adapter syncs metadata from pi events:

- `model`: From pi's `model_select` events or initial config
- `effort_level`: From thinking level changes
- `cwd`: Passed on spawn, reflected in session

## File Structure

```
packages/gravity-server/src/
├── pi-driver/                    # NEW: pi integration
│   ├── mod.ts                   # Main entry, exports startPiDriver()
│   ├── index.ts                 # Re-export startPiDriver
│   ├── spawn.ts                 # Spawn and manage pi subprocess (no provider/model)
│   ├── protocol.ts              # Parse pi JSONL events, send RPC commands
│   ├── session.ts               # Session sync, effort level, metadata
│   ├── hook-translator.ts       # pi events → HookData translation
│   ├── turn-accumulator.ts      # Batch pi turns into gravity turns
│   └── types.ts                 # PiEvent types, translation result
│
└── handlers/
    ├── event-handler.ts         # (existing — no changes needed)
    └── bidirectional.ts         # (existing — no changes needed)
```

## Environment Variables

| Variable | Description | Default |
|----------|-------------|---------|
| `PI_BINARY` | Path to pi binary | `pi` (PATH lookup) |
| `PI_CWD` | Working directory | Inherited from parent |

No `PI_PROVIDER` or `PI_MODEL` — pi uses its configured defaults.

## Non-Goals

- pi-as-extension (managed mode where pi is an extension of gravity-server) — out of scope
- Bidirectional flows beyond basic tool execution — deferred
- Multiple concurrent pi sessions — single session per adapter instance
- pi session file compatibility — adapter owns session
- Explicit provider/model override — pi uses its configured defaults

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

## TODO

- [ ] **pi-driver/types.ts** — Define `PiEvent` union type matching pi's event stream. Document translation result types (`HookData`, `TurnAccumulator` state).

- [ ] **pi-driver/hook-translator.ts** — Pure function: `translatePiEvent(event: PiEvent, state: AccState): { hookEvent: HookEventName, hookData: HookData }`. Handle content accumulation from message_update events.

- [ ] **pi-driver/turn-accumulator.ts** — State machine for batching pi turns into gravity turns. Track pending assistant text/thinking, tool queue, turn boundaries.

- [ ] **pi-driver/protocol.ts** — JSONL parser for pi stdout, typed event emitter. Implement RPC command formatter (prompt, steer, abort, set_thinking_level).

- [ ] **pi-driver/spawn.ts** — Subprocess spawning with stderr logging, process cleanup on shutdown. Handle process exit codes and signals.

- [ ] **pi-driver/session.ts** — Effort level mapping, session metadata sync. `syncEffortLevel(level: ThinkingLevel)`, `syncModel(model: Model)`.

- [ ] **pi-driver/mod.ts** — Compose all modules. Export `startPiDriver(options: PiDriverOptions)` returning `{ stop(): Promise<void>, setEffortLevel(level: string): void }`.

- [ ] **pi-driver/index.ts** — Re-export `startPiDriver` from mod.ts for ergonomic imports.

- [ ] **Wire into main.ts** — Add `--pi` flag to gravity-server. When set, spawn pi driver instead of listening on hook socket. Test basic spawn + prompt + event flow.

- [ ] **Test: Basic event flow** — Verify UserPromptSubmit → PreToolUse → PostToolUse → Stop events generate correct patches. Compare output with equivalent Claude Code session.

- [ ] **Test: Tool content extraction** — Verify assistant_text, post_tool_text, thinking content extracted from message_update events and attached to tool.

- [ ] **Test: Turn alignment** — Verify multiple pi tool calls within one prompt result in single gravity turn with multiple steps.

- [ ] **Test: Effort level** — Verify thinking level changes are reflected in HookData effort_level field.

- [ ] **Test: Shutdown cleanup** — Verify SessionEnd emitted and resources cleaned on adapter stop().

- [ ] **Error handling** — Add graceful degradation when pi subprocess crashes. Log errors without crashing gravity-server. Emit session.removed to terminals.

- [ ] **Documentation** — Document the `--pi` flag, environment variables (PI_BINARY_PATH, PI_PROVIDER, PI_MODEL), and usage patterns.
