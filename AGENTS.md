# Agent Instructions

This project uses **bd** (beads) for issue tracking. Run `bd onboard` to get started.

## Quick Reference

```bash
bd ready              # Find available work
bd show <id>          # View issue details
bd update <id> --status in_progress  # Claim work
bd close <id>         # Complete work
bd sync               # Sync with git
```

## Build/Lint/Test Commands

### Emacs Lisp

```elisp
M-x eval-defun        ; Evaluate changed function (after edit)
M-x eval-region       ; Evaluate selected region
M-x load-file         ; Reload entire file
M-x ert t             ; Run all ERT tests
```

```bash
# Run specific test file
M-x ert t /Users/gdanov/work/playground/emacs-gravity/test/claude-gravity-test.el

# Run specific test by name
M-x ert t cg-test-user-prompt-advances-turn
```

### Node.js Bridge (emacs-bridge/)

```bash
cd emacs-bridge
npm install           # Install dependencies (first time)
npm test              # Run vitest tests
npx tsc --noEmit      # Type check without building

# Run single test
npx vittest run --testNamePattern "extractTrailingText"
```

---

## Code Style Guidelines

### Emacs Lisp

**Naming Conventions:**
- Prefix public functions/variables with `claude-gravity-` or `cg-` (internal)
- Use kebab-case: `claude-gravity-session-get`
- Internal helpers use underscores: `claude-gravity--internal-func`
- Use double-hyphen for internal definitions
- Face names: `claude-gravity-face-name-face`

**Formatting:**
- Use 2 spaces for indentation, no tabs
- Max line length: ~80-100 characters
- Use `(require 'module)` at top, `(provide 'module-name)` at bottom
- Place `;;; Code:` marker after docstring header

**Types & Data:**
- Use `defcustom` with `:type` declarations for user-tweakable variables
- Use `plist-get`/`plist-put` for session data
- Use hash tables for indexed lookups: `(gethash key hash-table)`

**Error Handling:**
- Use `condition-case` for graceful error recovery
- Log errors with `claude-gravity--log`
- Return sensible defaults on error rather than throwing

**Critical Patterns:**
```elisp
;; Use push + nreverse instead of append in loops (O(n) vs O(n²))
(let ((result nil))
  (dolist (item items)
    (push (process item) result))
  (nreverse result))

;; Defer interactive calls from process filters
(run-at-time 0 nil #'completing-read "Choose: " '("a" "b"))
```

**Important Notes:**
- Use `lexical-binding: t` at top of all `.el` files
- Never use `append` in loops
- Use `magit-insert-section` macros as-is — don't refactor them

### TypeScript (emacs-bridge/)

**Naming & Formatting:**
- Kebab-case files: `extract-transcript.ts`
- PascalCase types/interfaces, camelCase functions
- Max line length: 100 characters
- Explicit return types for exported functions

**Types:**
- Strict TypeScript (`strict: true` in tsconfig.json)
- Avoid `any` — use `unknown` with type guards

**Error Handling:**
- Always return valid JSON to stdout, even on errors
- Log to `/tmp/emacs-bridge.log`

---

## Project Structure

```
emacs-gravity/
├── claude-gravity.el          ; Thin loader
├── claude-gravity-core.el     ; Utilities, logging, custom variables
├── claude-gravity-faces.el    ; Faces and fringe bitmaps
├── claude-gravity-session.el  ; Session state CRUD
├── claude-gravity-state.el    ; Model API, mutation functions
├── claude-gravity-events.el   ; Event dispatcher (11 hook types)
├── claude-gravity-text.el     ; Text rendering
├── claude-gravity-diff.el     ; Inline diffs, tool/plan display
├── claude-gravity-render.el   ; UI section rendering
├── claude-gravity-ui.el       ; Buffers, keymaps, transient menu
├── claude-gravity-socket.el   ; Socket server
├── claude-gravity-actions.el  ; Permission/question buffers
├── claude-gravity-tmux.el     ; Tmux session management
├── claude-gravity-debug.el    ; Debug helpers
├── emacs-bridge/              ; Node.js hook forwarder
│   ├── src/index.ts
│   └── test/*.test.ts
└── test/
    └── claude-gravity-test.el ; ERT tests
```

---

## Landing the Plane (Session Completion)

**When ending a work session**, you MUST complete ALL steps below. Work is NOT complete until `git push` succeeds.

**MANDATORY WORKFLOW:**

1. **File issues for remaining work** - Create issues for anything that needs follow-up
2. **Run quality gates** (if code changed) - Tests, linters, builds
3. **Update issue status** - Close finished work, update in-progress items
4. **PUSH TO REMOTE** - This is MANDATORY:
   ```bash
   git pull --rebase
   bd sync
   git push
   git status  # MUST show "up to date with origin"
   ```
5. **Clean up** - Clear stashes, prune remote branches
6. **Verify** - All changes committed AND pushed
7. **Hand off** - Provide context for next session

**CRITICAL RULES:**
- Work is NOT complete until `git push` succeeds
- NEVER stop before pushing - that leaves work stranded locally
- NEVER say "ready to push when you are" - YOU must push
- If push fails, resolve and retry until it succeeds

Use 'bd' for task tracking
