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

## Status: Complete

All core functionality is implemented and tested:

- [x] **pi-driver/types.ts** — PiEvent union type, TranslationResult, AccState
- [x] **pi-driver/hook-translator.ts** — translatePiEvent() with content accumulation
- [x] **pi-driver/turn-accumulator.ts** — State machine for batching pi turns
- [x] **pi-driver/protocol.ts** — JSONL parser and RPC command formatter
- [x] **pi-driver/spawn.ts** — Subprocess spawning with lifecycle management
- [x] **pi-driver/session.ts** — Effort level mapping and metadata sync
- [x] **pi-driver/mod.ts** — startPiDriver() composition
- [x] **pi-driver/index.ts** — Re-export startPiDriver
- [x] **Wire into gravity-server.ts** — `--pi` flag and terminal message handlers
- [x] **Emacs client functions** — claude-gravity--pi-start, claude-gravity--pi-prompt, etc.
- [x] **Unit tests** — 40 tests covering all modules

## Non-Goals (Deferred)

- Bidirectional flows (permission/question) — pi handles natively
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