# Development Guide: emacs-gravity

This guide covers building, testing, and debugging the emacs-gravity project.

## Dependencies

### Emacs
- `emacs >= 27.1`
- `magit-section >= 3.0.0` (hierarchical collapsible sections)
- `transient >= 0.3.0` (menu UI)

### Node.js & npm
- Node.js >= 18.0.0
- npm >= 8.0.0
- TypeScript (dev dependency)
- Vitest (testing framework)

## Build Commands

### Node.js Bridge

The `emacs-bridge/` directory contains the Node.js plugin that forwards Claude Code hook events to Emacs over a Unix domain socket.

**Install dependencies (first time):**
```bash
cd emacs-bridge && npm install
```

**Build the bridge (required after editing `emacs-bridge/src/index.ts`):**
```bash
cd emacs-bridge && npm run build
```

**Run tests:**
```bash
cd emacs-bridge && npm test
```

**Type check without building:**
```bash
cd emacs-bridge && npx tsc --noEmit
```

### Emacs Lisp

For the Emacs Lisp code, use the `emacs` MCP to re-evaluate code after making changes:

```elisp
;; After editing a defun in claude-gravity-*.el:
M-x eval-defun   ;; Evaluate the changed function
M-x eval-region  ;; Evaluate a region of code
```

**Important:** When editing `claude-gravity.el` modules, eval all changed/new defuns AND any callers that reference them. The server-filter is registered via `:filter 'claude-gravity--server-filter` (symbol reference), so re-eval of the defun is sufficient — but if it was byte-compiled at load time, the stale compiled version runs until re-eval'd.

## Debugging

### Bridge Logs

The Node.js bridge logs all activity to a file:
```
/tmp/emacs-bridge.log
```

View logs in real-time:
```bash
tail -f /tmp/emacs-bridge.log
```

### Agent State Persistence

Agent state is persisted across bridge invocations:
```
{cwd}/.claude/emacs-bridge-agents.json
```

This file tracks active agents and is gitignored. Check it to understand state transitions.

### Emacs Debugging

Use the `emacs` MCP to inspect buffers and debug running Emacs:

```elisp
;; Get buffer contents
M-x emacs-buffer-contents

;; Eval arbitrary Elisp
M-x emacs-eval-region
```

Check the session buffer (e.g., `*Structured Claude Session <slug>*`) for live rendering feedback.

## Testing Interactive Sessions

### Testing with tmux

When testing Claude Code sessions that run in tmux:

**Important:** Use `tmux send-keys -t <session> -l "/command"` with the `-l` (literal) flag, then send Enter **separately** after a 2-second delay:

```bash
tmux send-keys -t $SESS -l "/clear" && sleep 2 && tmux send-keys -t $SESS Enter
```

Without `-l` and the delay, Enter gets consumed by Claude Code's autocomplete dropdown instead of confirming the command.

### /clear Behavior

The `/clear` command in Claude Code (v2.1.37+) fires both `SessionEnd` and `SessionStart` hooks with a new session ID:
- The session ID **changes** (does not reuse the old one)
- Hooks fire **after** `/clear` finishes processing (compaction may take 10+ seconds)
- This triggers buffer cleanup and creation of a new session buffer in Emacs

## Known Pitfalls

### Transcript Race Conditions

When the Stop hook fires, the next turn may have already started writing `tool_use` entries to the transcript. The `extractTrailingText` function in the bridge uses a retry-with-delay pattern intentionally — **do not remove retry loops**.

**Further optimization:** Capture `statSync(transcriptPath).size` at Stop invocation time and pass as `maxBytes` to `readHead()`, limiting the read to only data from the completed turn.

### Process Filter Context

Interactive minibuffer functions cannot run inside a network process filter (`claude-gravity--server-filter`). They silently error. Use `(run-at-time 0 nil #'my-func args...)` to defer interactive calls to the Emacs command loop:

```elisp
;; Wrong: This silently fails inside process-filter
(completing-read "Choose: " '("a" "b"))

;; Right: Defer to Emacs command loop
(run-at-time 0 nil #'completing-read "Choose: " '("a" "b"))
```

The socket stays alive while waiting for the response, so the bridge receives the result correctly.

### Magit Section Macros

The `magit-insert-section` macros expand into complex `unwind-protect` blocks. Do not refactor them into higher-order functions — the macro expansion is critical for proper cleanup and navigation.

### Hook Configuration

The `hooks.json` file uses `${CLAUDE_PLUGIN_ROOT}` which is expanded by Claude Code at startup, not by the shell. Ensure all paths in `hooks.json` use this variable or absolute paths.

## Performance Considerations

See @MEMORY.md for detailed performance analysis (last updated 2026-02-09). Key findings:

- O(n²) append-in-loop patterns at 4 sites (use `push` + `nreverse` instead)
- `json-read-from-string` parsing (replace with `json-parse-string`)
- Linear tool search (add tool_use_id hash index)
- Full erase+rebuild on every event (cache tail section)

Quick win implementations will provide ~40% performance improvement on typical sessions.

## Module Testing

Each module in `claude-gravity-*.el` can be tested independently via the emacs MCP:

```elisp
;; Load a specific module
(require 'claude-gravity-render)

;; Call a test function
(claude-gravity-render-example-section)
```

Modules have no circular dependencies, making isolated testing straightforward.
