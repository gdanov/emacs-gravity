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

### Node.js (monorepo)

```bash
npm install               # Install all workspace dependencies (first time)
make test                 # Run all tests (elisp + bridge + server)
make test-bridge          # Bridge tests only
make test-server          # Server tests only
make build-server         # Build gravity-server → dist/server.mjs
make sync-cache           # Sync dist to plugin marketplace cache (REQUIRED after changes)
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

### TypeScript (packages/emacs-bridge/, packages/gravity-server/, packages/shared/)

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
- Log to `/tmp/emacs-bridge.log` (bridge) or stderr (gravity-server)

---

## Project Structure

```
emacs-gravity/
├── claude-gravity.el              ; Thin loader
├── claude-gravity-core.el         ; Utilities, logging, tlist
├── claude-gravity-faces.el        ; Faces and fringe bitmaps
├── claude-gravity-session.el      ; Session state CRUD
├── claude-gravity-discovery.el    ; Plugin/skill/MCP capability discovery
├── claude-gravity-state.el        ; Model API, mutation functions (read-replica)
├── claude-gravity-events.el       ; Event dispatcher (11 hook types)
├── claude-gravity-text.el         ; Text rendering
├── claude-gravity-diff.el         ; Inline diffs, tool/plan display
├── claude-gravity-render.el       ; UI section rendering
├── claude-gravity-ui.el           ; Buffers, keymaps, transient menu
├── claude-gravity-plan-review.el  ; Plan review buffer and feedback
├── claude-gravity-client.el       ; Terminal socket client to gravity-server
├── claude-gravity-actions.el      ; Permission/question action buffers
├── claude-gravity-tmux.el         ; Tmux session management
├── claude-gravity-debug.el        ; Debug helpers
├── packages/
│   ├── shared/                    ; Shared types (Session, Patch, messages)
│   ├── emacs-bridge/              ; Claude Code plugin (one-shot shim)
│   │   ├── src/index.ts
│   │   └── hooks/                 ; Shell scripts + _ensure-server
│   └── gravity-server/            ; Stateful backend (state, enrichment, protocol)
│       ├── src/server.ts
│       └── test/
├── Makefile
└── test/
    └── claude-gravity-test.el     ; ERT tests
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

<!-- BEGIN BEADS INTEGRATION -->
## Issue Tracking with bd (beads)

**IMPORTANT**: This project uses **bd (beads)** for ALL issue tracking. Do NOT use markdown TODOs, task lists, or other tracking methods.

### Why bd?

- Dependency-aware: Track blockers and relationships between issues
- Git-friendly: Dolt-powered version control with native sync
- Agent-optimized: JSON output, ready work detection, discovered-from links
- Prevents duplicate tracking systems and confusion

### Quick Start

**Check for ready work:**

```bash
bd ready --json
```

**Create new issues:**

```bash
bd create "Issue title" --description="Detailed context" -t bug|feature|task -p 0-4 --json
bd create "Issue title" --description="What this issue is about" -p 1 --deps discovered-from:bd-123 --json
```

**Claim and update:**

```bash
bd update <id> --claim --json
bd update bd-42 --priority 1 --json
```

**Complete work:**

```bash
bd close bd-42 --reason "Completed" --json
```

### Issue Types

- `bug` - Something broken
- `feature` - New functionality
- `task` - Work item (tests, docs, refactoring)
- `epic` - Large feature with subtasks
- `chore` - Maintenance (dependencies, tooling)

### Priorities

- `0` - Critical (security, data loss, broken builds)
- `1` - High (major features, important bugs)
- `2` - Medium (default, nice-to-have)
- `3` - Low (polish, optimization)
- `4` - Backlog (future ideas)

### Workflow for AI Agents

1. **Check ready work**: `bd ready` shows unblocked issues
2. **Claim your task atomically**: `bd update <id> --claim`
3. **Work on it**: Implement, test, document
4. **Discover new work?** Create linked issue:
   - `bd create "Found bug" --description="Details about what was found" -p 1 --deps discovered-from:<parent-id>`
5. **Complete**: `bd close <id> --reason "Done"`

### Auto-Sync

bd automatically syncs via Dolt:

- Each write auto-commits to Dolt history
- Use `bd dolt push`/`bd dolt pull` for remote sync
- No manual export/import needed!

### Important Rules

- ✅ Use bd for ALL task tracking
- ✅ Always use `--json` flag for programmatic use
- ✅ Link discovered work with `discovered-from` dependencies
- ✅ Check `bd ready` before asking "what should I work on?"
- ❌ Do NOT create markdown TODO lists
- ❌ Do NOT use external issue trackers
- ❌ Do NOT duplicate tracking systems

For more details, see README.md and docs/QUICKSTART.md.

## Landing the Plane (Session Completion)

**When ending a work session**, you MUST complete ALL steps below. Work is NOT complete until `git push` succeeds.

**MANDATORY WORKFLOW:**

1. **File issues for remaining work** - Create issues for anything that needs follow-up
2. **Run quality gates** (if code changed) - Tests, linters, builds
3. **Update issue status** - Close finished work, update in-progress items
4. **PUSH TO REMOTE** - This is MANDATORY:
   ```bash
   git pull --rebase
   bd dolt push
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

<!-- END BEADS INTEGRATION -->
