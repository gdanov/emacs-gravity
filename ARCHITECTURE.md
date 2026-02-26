# System Architecture: emacs-gravity

This document describes the overall system design, component interactions, and module structure.

## System Overview

```
Claude Code
    ↓ (hooks: PreToolUse, PostToolUse, PostToolUseFailure, Stop,
    │         UserPromptSubmit, SubagentStart, SubagentStop,
    │         SessionStart, SessionEnd, Notification, PermissionRequest)
emacs-bridge (Node.js)
    ↕ (JSON over Unix domain socket — bidirectional for PermissionRequest)
claude-gravity.el (Emacs)
```

### One-Shot Architecture

**emacs-bridge** is a stateless one-shot process:
1. Claude Code hook script invokes `tsx src/index.ts` with hook event name as argument
2. Bridge reads event JSON from stdin
3. Bridge forwards event over Unix domain socket to Emacs
4. Bridge exits (process lifecycle is hook duration)
5. Emacs receives and processes event asynchronously

**State persistence** between bridge invocations:
- Agent tracking state: `.claude/emacs-bridge-agents.json` (gitignored)
- Emacs maintains all session state in hash tables
- Socket communication is stateless from bridge perspective

### Socket Communication

The socket path is `claude-gravity.sock` in the package directory, resolved via:
1. `${CLAUDE_PLUGIN_ROOT}` environment variable (set by Claude Code)
2. Fallback: relative to `load-file-name` (Emacs package directory)

**Bidirectional flow:**
- All hooks are **fire-and-forget** (bridge sends event, exits)
- Except `PermissionRequest` and `AskUserQuestion`: bridge keeps socket open, waits for Emacs response
- Emacs sends structured JSON response back over the socket
- Bridge reads response and writes it to stdout for Claude Code to consume

**Error handling:**
- Bridge always returns valid JSON to stdout, even on socket errors
- Prevents breaking Claude Code if Emacs is unavailable
- Emacs buffers messages and renders them when next event arrives

## Bridge Design: Stable View Model

The bridge layer translates different session sources into a **stable, unified view model** that Emacs consumes. This ensures the UI renders identically regardless of how the session was started.

### Supported Bridges

1. **One-Shot Bridge** (`emacs-bridge/src/index.ts`)
   - Stateless process invoked by Claude Code hooks
   - Extracts `stop_text` from transcript files
   - Runs via tsx: `node_modules/.bin/tsx src/index.ts <event>`

2. **PI Agent Bridge** (`emacs-bridge/src/pi-session.ts`)
   - Stateful adapter for `@mariozechner/pi-agent-core`
   - Translates pi-agent events to view model format
   - Extracts `stop_text`/`stop_thinking` from agent message content
   - Runs via tsx: `node_modules/.bin/tsx src/daemon.ts`

3. **Daemon Bridge** (`emacs-bridge/src/daemon.ts`)
   - Long-running process for SDK-based sessions (ON HOLD — API key restrictions)
   - Manages sessions via `@anthropic-ai/claude-agent-sdk`

### View Model Invariants

All bridges MUST produce events conforming to this schema:

```
SessionStart: { session_id, cwd, model }
UserPromptSubmit: { prompt }
Stop: { stop_text?, stop_thinking?, token_usage? }
SubagentStart: { agent_id, agent_type }
SubagentStop: { agent_id, agent_stop_text?, agent_stop_thinking? }
PreToolUse: { tool_name, tool_call_id }
PostToolUse: { tool_name, tool_call_id, result, is_error? }
PostToolUseFailure: { tool_name, tool_call_id, result }
```

**Key invariants:**
- `stop_text` is always present on Stop events (empty string if no content)
- `agent_stop_text` is always present on SubagentStop events
- Turn numbers are sequential integers starting from 1
- Tool attribution is deterministic regardless of bridge source

### Why This Matters

The Emacs view model should not know which bridge generated the event. This:
- Allows testing bridges in isolation
- Enables future bridges (e.g., CLI-only, MCP) without UI changes
- Keeps the rendering layer simple and predictable

## Hook System

Hook scripts in `emacs-bridge/hooks/` are registered via `hooks.json`. Each hook is a shell script that pipes stdin to the Node.js bridge with the event name as an argument. The system handles 11 event types:

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
- **Stop**: User stops Claude's generation (Ctrl+C)
- **Notification**: Informational messages (e.g., permission granted)

### Bidirectional
- **PermissionRequest**: Bridge sends request, waits for Emacs response (Approve/Deny)
  - Matcher: `ExitPlanMode` (when Claude Code exits plan mode)
  - Timeout: 96 hours
  - Response: User clicks button in `*Claude Permissions*` buffer

## Module Structure

The Emacs package is split into 13 modular files loaded via `claude-gravity.el` (thin loader):

| Module | Lines | Purpose | Key Functions |
|--------|-------|---------|---|
| `claude-gravity-core.el` | ~150 | defgroup, defcustom, logging | `claude-gravity-log`, `claude-gravity--session-path` |
| `claude-gravity-faces.el` | ~250 | 37 defface declarations + fringe bitmaps | `claude-gravity-face-*` (37 faces) |
| `claude-gravity-session.el` | ~190 | Session hash table, CRUD operations | `claude-gravity--session-get`, `claude-gravity--session-set` |
| `claude-gravity-state.el` | ~530 | Inbox queue, file/task/agent tracking, model API | `claude-gravity--inbox-push`, `claude-gravity--add-tool`, `claude-gravity--update-task` |
| `claude-gravity-events.el` | ~340 | Event dispatcher, hook handlers | `claude-gravity--handle-event` (all 11 hook types) |
| `claude-gravity-text.el` | ~370 | Text utilities: dividers, tables, markdown, wrapping | `claude-gravity--wrap-text`, `claude-gravity--render-plan` |
| `claude-gravity-diff.el` | ~650 | Inline diffs, tool display, plan revision diff | `claude-gravity--render-tool-diff`, `claude-gravity--plan-revision-diff` |
| `claude-gravity-render.el` | ~990 | Section renderers, turn grouping, agent/tool/task UI | `claude-gravity--insert-turn-section`, `claude-gravity--insert-tool` |
| `claude-gravity-ui.el` | ~920 | Overview/session buffers, modes, keymaps, transient | `claude-gravity-status` (main entry point), keymaps |
| `claude-gravity-socket.el` | ~700 | Socket server, plan review, permissions | `claude-gravity--server-start`, `claude-gravity--handle-permission-request` |
| `claude-gravity-actions.el` | ~480 | Permission/question action buffers | `claude-gravity--show-permission-buffer`, `claude-gravity--show-question-buffer` |
| `claude-gravity-tmux.el` | ~610 | Tmux session management, compose buffer | `claude-gravity--tmux-start-session`, `claude-gravity--tmux-heartbeat` |
| `claude-gravity.el` | ~30 | Thin loader: requires all modules | `claude-gravity` (main entry point) |

### Load Order (Dependency DAG)

```
core → {faces, session} → state → events → {text, diff} → render → ui
                           ↓                                ↓
                         socket ← ← ← ← ← ← ← ← ← ← ← ← ← ←
                           ↓
                      {actions, tmux}
```

Explanation:
- `core` defines utilities and custom vars (no dependencies)
- `faces` and `session` depend on `core` only
- `state` depends on `core` and `session` (stores data in session hash table)
- `events` depends on `core` and `state` (updates state based on events)
- `text` and `diff` depend on `core` (pure text transformation)
- `render` depends on all above (renders turns, tools, tasks, agents)
- `ui` depends on `render` (main UI buffers and keymaps)
- `socket` depends on `render` (needs rendering functions to send back to bridge)
- `actions` and `tmux` depend on `socket` and `ui` (interactive buffers)

**Cross-module forward references:**
- Use `declare-function` for functions you call before they're defined
- Use bare `defvar` for variables (will be defined elsewhere)
- This pattern allows modular loading without circular dependencies

## Model API and State Management

The state model is defined in `claude-gravity-state.el` as a set of mutation functions:

```
Session (plist)
├── :slug (string) — unique session identifier
├── :project (string) — working directory
├── :status (symbol) — idle, responding, ended
├── :turns (vector) — turn objects
├── :tools (vector) — tool execution history
├── :tasks (hash-table) — task tracking
├── :files (hash-table) — file operation tracking
├── :agents (hash-table) — subagent state
├── :plan (plist) — current plan (if any)
└── :inbox (list) — queued events (for deferred processing)
```

**Model API (mutation functions in `claude-gravity-state.el`):**
- `claude-gravity--session-get(slug key)` — read session property
- `claude-gravity--session-set(slug key value)` — write session property
- `claude-gravity--inbox-push(slug event)` — queue event for processing
- `claude-gravity--add-turn(slug turn-object)` — append turn
- `claude-gravity--add-tool(slug turn-idx tool)` — append tool to turn
- `claude-gravity--update-task(slug task-id fields)` — update task
- `claude-gravity--add-file(slug file-path operation)` — track file op
- `claude-gravity--add-agent(slug agent-id agent-object)` — track agent
- `claude-gravity--remove-agent(slug agent-id)` — remove agent
- `claude-gravity--update-agent(slug agent-id fields)` — update agent
- `claude-gravity--set-plan(slug plan)` — set plan
- (and ~17 more model functions)

**Rendering adapts to model state:**
- `claude-gravity-render.el` reads model state and produces magit-section UI
- No impedance mismatch: model stores exactly what UI needs to display
- Deferred rendering: UI refreshes on demand (`g` key) or after events

## Design Patterns

### Tool Attribution with Multiple Agents

When multiple agents are active, tools can be ambiguous. The system:
1. Scans the transcript backward from each tool to find the most recent agent context
2. For single-agent cases, uses optimized path (no scanning)
3. Attributes tool to the correct agent's nested section

### Bidirectional PermissionRequest Flow

PermissionRequest and AskUserQuestion keep the socket open:
1. Bridge sends request JSON to Emacs
2. Emacs renders interactive buffer (e.g., plan review, permission dialog)
3. User interacts and submits response
4. Emacs sends response JSON back over socket
5. Bridge reads response and writes to stdout
6. Claude Code receives response and continues

### Known Bug: ExitPlanMode Allow Ignored (Deny-as-Approve Workaround)

Claude Code silently ignores `allow` responses from PermissionRequest hooks for `ExitPlanMode`. This is a regression of [#15755](https://github.com/anthropics/claude-code/issues/15755), still broken as of v2.1.50. The TUI prompt appears simultaneously with the hook — if the hook responds `allow`, Claude Code drops it. `deny` responses are always processed.

**Workaround (commit `62b24d8`):** The bridge intercepts `allow` responses for ExitPlanMode and converts them to `deny` with the message "User approved the plan. Proceed with implementation." Claude reads the message content, sees approval, and proceeds.

**What this means for the data flow:**
1. Emacs `claude-gravity-plan-review-approve` sends `{"decision":{"behavior":"allow"}}`
2. Bridge (`index.ts`, PermissionRequest block) intercepts and rewrites to `{"decision":{"behavior":"deny","message":"User approved the plan. Proceed with implementation."}}`
3. Claude Code processes the deny, model reads the message, continues implementing

**Deny with feedback** (user annotated the plan) is already a deny — it passes through unchanged.

**Removal:** When Claude Code fixes #15755, delete the `if (toolName === "ExitPlanMode" && response?.decision?.behavior === "allow")` block in `index.ts`. Emacs already sends the correct allow response.

### PostToolUse Content Extraction

PostToolUse events extract both preceding and following assistant text:
1. **Preceding content** (`extractPrecedingContent`): text before the tool in the same turn
2. **Following content** (`extractFollowingContent`): text after the tool but before the next turn

This allows displaying assistant commentary around tool execution.

### Agent Transcript Handling

Agent transcripts are stored in "sidechain" format (separate files):
1. Claude Code creates `agent_transcript_path` file during agent execution
2. SubagentStop event provides path but no `stop_text`/`stop_thinking`
3. Bridge detects sidechain format and extracts trailing content from agent transcript
4. Emacs displays agent completion at turn level AND in nested agent section

## Migration History

**Phase 1 (Completed):** Extract model API
- Split monolithic Emacs code into modular files
- Define clean session state model in `claude-gravity-state.el`
- Adapt all renderers to read from model state

**Phase 2 (Completed):** Add managed process + JSON-output adapter
- `docs/emacs-driven-sessions.md`: experiment with spawning Claude Code as managed subprocess
- JSON-output adapter: reads stdout and feeds into model API
- Hooks adapter: existing hook system forwards events to model API
- Both adapters write to same model, allowing dual operation

**Daemon Bridge (ON HOLD — 2026-02):** Agent SDK daemon (`daemon.ts`, `daemon-session.ts`)
- Long-running Node.js process managing SDK sessions via `@anthropic-ai/claude-agent-sdk`
- **BLOCKED**: The Agent SDK requires a pay-per-use API key. Using Claude Max/Pro subscription with the SDK violates Anthropic's TOS and risks account ban. See [#6536](https://github.com/anthropics/claude-code/issues/6536), [#5891](https://github.com/anthropics/claude-code/issues/5891).
- Code exists but is not usable without separate API billing. Revisit if Anthropic changes policy.

**Phase 3 (Backlog):** Hooks-free mode
- Remove dependency on Claude Code hooks
- Run Claude Code entirely via managed process (eat-terminal)
- All events sourced from JSON-output + subprocess exit handling

## Related Documentation

See the following for detailed information on specific topics:
- @DEVELOPMENT.md — Build commands, dependencies, debugging, testing
- @UI-SPEC.md — Visual specification for all UI states and keybindings
- @docs/emacs-driven-sessions.md — Research on managed session architecture
- @docs/tmux-interactive-sessions.md — Tmux integration approach
