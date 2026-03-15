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

### Monorepo Setup

The project uses npm workspaces with three packages under `packages/`:
- `@gravity/shared` — Shared types and utilities
- `emacs-bridge` — Claude Code plugin (one-shot hook forwarder)
- `@gravity/server` — Stateful backend (session state, terminal protocol)

**Install all dependencies (first time):**
```bash
npm install
```

**Run all tests:**
```bash
make test
```

**Type check all packages:**
```bash
npx -w packages/shared tsc --noEmit
npx -w packages/emacs-bridge tsc --noEmit
npx -w packages/gravity-server tsc --noEmit
```

### Bridge (packages/emacs-bridge)

Uses tsx — no build step needed. Changes to TypeScript files picked up automatically.

**Run bridge tests:**
```bash
make test-bridge
```

### Gravity Server (packages/gravity-server)

Long-running TypeScript backend. Run with tsx.

**Run server tests:**
```bash
make test-server
```

**Start server manually:**
```bash
npx -w packages/gravity-server tsx src/server.ts
```

Sockets: `~/.local/state/gravity-hooks.sock` (bridge → server) and `~/.local/state/gravity-terminal.sock` (server → terminals). Override with `GRAVITY_HOOK_SOCK` / `GRAVITY_TERMINAL_SOCK`.

### Emacs Lisp

**CRITICAL: All CLI Emacs invocations MUST use `-nw`** (e.g., `/Applications/Emacs.app/Contents/MacOS/Emacs -nw --batch ...`). Without `-nw`, macOS tries to open a GUI window which hangs or fails in non-interactive contexts.

For the Emacs Lisp code, use the `emacs` MCP to re-evaluate code after making changes:

```elisp
;; After editing a defun in claude-gravity-*.el:
M-x eval-defun   ;; Evaluate the changed function
M-x eval-region  ;; Evaluate a region of code
```

**Important:** When editing `claude-gravity-*.el` modules, eval all changed/new defuns AND any callers that reference them. The client filter is registered via `:filter 'claude-gravity--client-filter` (symbol reference), so re-eval of the defun is sufficient — but if it was byte-compiled at load time, the stale compiled version runs until re-eval'd.

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

### Gravity Server Logs

gravity-server logs to stderr (visible in the Emacs `*gravity-server*` process buffer if started from Emacs).

### Agent State

In v3, agent state is managed in-memory by gravity-server (no file I/O). The legacy file `{cwd}/.claude/emacs-bridge-agents.json` is no longer used.

### Building and Deploying

```bash
make build-server     # esbuild gravity-server → dist/server.mjs
make sync-cache       # Sync bridge + server dist to plugin marketplace cache
```

**CRITICAL:** After any TypeScript change, run `make sync-cache` to copy dist files to `~/.claude/plugins/cache/local-emacs-marketplace/emacs-bridge/2.0.0/dist/`. Without this, stale cached code runs instead of the dev version.

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

Interactive minibuffer functions cannot run inside a network process filter (`claude-gravity--client-filter`). They silently error. Use `(run-at-time 0 nil #'my-func args...)` to defer interactive calls to the Emacs command loop:

```elisp
;; Wrong: This silently fails inside process-filter
(completing-read "Choose: " '("a" "b"))

;; Right: Defer to Emacs command loop
(run-at-time 0 nil #'completing-read "Choose: " '("a" "b"))
```

The socket stays alive while waiting for the response, so the bridge receives the result correctly.

### Magit Section Macros

The `magit-insert-section` macros expand into complex `unwind-protect` blocks. Do not refactor them into higher-order functions — the macro expansion is critical for proper cleanup and navigation.

### ExitPlanMode Allow Ignored (#15755)

Claude Code silently ignores `allow` responses from PermissionRequest hooks for ExitPlanMode. This is a regression of [#15755](https://github.com/anthropics/claude-code/issues/15755), still broken as of v2.1.50. `deny` responses always work.

**Workaround:** gravity-server intercepts ExitPlanMode `allow` responses and converts them to `deny` with message "User approved the plan. Proceed with implementation." See the "Known Bug" section in ARCHITECTURE.md.

**If plan approval via Emacs stops working:** Check that the server workaround is still in place. Check gravity-server logs for `converting ExitPlanMode allow → deny-as-approve`.

**To remove:** When #15755 is fixed upstream, delete the workaround in gravity-server's bidirectional handler and the related comments in `claude-gravity-plan-review.el`.

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
