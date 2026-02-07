# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Emacs UI for Claude Code, inspired by Google's AntiGravity and Cursor. Provides a Magit-style working memory interface that displays Claude Code's plan, tasks, tool execution status, and supports commenting/annotations.

## Architecture

```
Claude Code
    ↓ (hooks: PreToolUse, PostToolUse, PostToolUseFailure, Stop,
    │         UserPromptSubmit, SubagentStart, SubagentStop,
    │         SessionStart, SessionEnd, Notification, PermissionRequest)
emacs-bridge (Node.js)
    ↕ (JSON over Unix domain socket — bidirectional for PermissionRequest)
claude-gravity.el (Emacs)
```

- **emacs-bridge/**: Claude Code plugin. Shell hook scripts invoke `dist/index.js` which reads event JSON from stdin and forwards it over a Unix socket to Emacs.
- **claude-gravity.el**: Emacs package. Runs a Unix socket server, receives tool events, maintains state, and renders a Magit-section based UI with transient menus.

The socket path is `claude-gravity.sock` in the package directory (resolved via `CLAUDE_PLUGIN_ROOT` or relative to `load-file-name`). The bridge always returns valid JSON to stdout to avoid breaking Claude Code, even on errors. Plugin registration is in `marketplace.json`.

## Build Commands

```bash
# Build the Node.js bridge (required after editing emacs-bridge/src/index.ts)
cd emacs-bridge && npm run build

# Install Node.js dependencies (first time setup)
cd emacs-bridge && npm install

# Run bridge tests
cd emacs-bridge && npm test
```

For the Emacs Lisp code we use the `emacs` MCP to re-evaluate code.

## Emacs Dependencies

- `emacs >= 27.1`
- `magit-section >= 3.0.0` (hierarchical collapsible sections)
- `transient >= 0.3.0` (menu UI)

## Hook System

Hook scripts in `emacs-bridge/hooks/` are registered via `hooks.json`. Each hook is a shell script that pipes stdin to the Node.js bridge with the event name as an argument. Handles 11 events:

- **Session lifecycle**: `SessionStart`, `SessionEnd`
- **Tool lifecycle**: `PreToolUse`, `PostToolUse`, `PostToolUseFailure`
- **Agent lifecycle**: `SubagentStart`, `SubagentStop`
- **Interaction**: `UserPromptSubmit`, `Stop`, `Notification`
- **Bidirectional**: `PermissionRequest` — Emacs sends approval/denial back over the socket (matcher: `ExitPlanMode`, timeout: 96h)

## UI Sections

The Emacs UI (magit-section based) displays:
- **Overview buffer**: All sessions grouped by project, with status (idle/responding/ended)
- **Session detail buffer**: Per-session view with header (project, status, elapsed, token usage)
- **Turn-based response cycles**: Tools and assistant text grouped by turn with collapse
- **Plan review**: Bidirectional PermissionRequest flow. Opens editable markdown buffer (`*Claude Plan Review: <slug>*`) with inline comments (`c`), `@claude` marker scanning, and diff view (`C-c C-d`). Approve (`C-c C-c`) auto-denies with structured feedback if any edits, comments, or `@claude` markers are detected
- **Task tracking**: TaskCreate/TaskUpdate/TaskList lifecycle with status indicators
- **File tracking**: Tracks read/edit/write operations per file
- **Agent tracking**: Subagent status with nested rendering, tool attribution, transcript viewing
- **Prompt history**: User prompts, questions (AskUserQuestion), plan approvals
- **Permission patterns**: Generate and write allow patterns to settings.local.json
- **Comment overlays**: Annotate tools/items with wave-underline comments
- **Transient menu** (`?`): Full command palette — Actions, Permissions, Sessions, Navigation

Multi-session support: each Claude Code session gets its own buffer, identified by session ID.

**Entry point**: `M-x claude-gravity-status` opens the overview buffer. Requires `(require 'claude-gravity)` and `(claude-gravity-server-start)` in init.el.

## Keybindings

### Main mode (`claude-gravity-mode-map`)

| Key | Command | Description |
|-----|---------|-------------|
| `g` | `claude-gravity-refresh` | Refresh buffer |
| `c` | `claude-gravity-comment-at-point` | Add comment overlay on section |
| `P` | `claude-gravity-show-plan` | Show plan in side buffer |
| `F` | `claude-gravity-open-plan-file` | Open plan file |
| `t` | `claude-gravity-tail` | Collapse all, focus latest turn |
| `T` | `claude-gravity-view-agent-transcript` | Parse agent transcript |
| `V` | `claude-gravity-open-agent-transcript` | Open agent transcript file |
| `A` | `claude-gravity-add-allow-pattern` | Copy allow pattern to kill ring |
| `a` | `claude-gravity-add-allow-pattern-to-settings` | Add pattern to settings.local.json |
| `D` | `claude-gravity-cleanup-sessions` | Remove ended sessions |
| `R` | `claude-gravity-reset-status` | Reset all status to idle |
| `X` | `claude-gravity-detect-dead-sessions` | Detect dead sessions |
| `d` | `claude-gravity-delete-session` | Delete session at point |
| `?` | `claude-gravity-menu` | Open transient menu |
| `TAB` | `magit-section-toggle` | Toggle section |
| `RET` | `claude-gravity-visit-or-toggle` | Visit session/agent or toggle |

### Transient menu experiment commands (`?` then `e`)

| Key | Command | Description |
|-----|---------|-------------|
| `e S` | `claude-gravity-experiment-eat-session` | Start hidden eat session with hooks |
| `e s` | `claude-gravity-experiment-send-prompt` | Send prompt to eat session |
| `e o` | `claude-gravity-experiment-show-output` | Display eat buffer output |
| `e k` | `claude-gravity-experiment-kill-session` | Kill eat experiment session |

### Plan review mode (`claude-gravity-plan-review-mode-map`)

| Key | Command | Description |
|-----|---------|-------------|
| `C-c C-c` | `claude-gravity-plan-review-approve` | Approve (auto-denies if feedback detected) |
| `C-c C-k` | `claude-gravity-plan-review-deny` | Deny with feedback |
| `C-c C-d` | `claude-gravity-plan-review-diff` | Show diff vs original |
| `c` | `claude-gravity-plan-review-comment` | Add inline comment on line |

## Related Documentation

- `UI-SPEC.md` — visual specification for all UI states and sections
- `plan.md` — project roadmap, current features, and open items

## Debugging & testing

The Node.js bridge logs all activity to `/tmp/emacs-bridge.log`.

Agent state is persisted to `{cwd}/.claude/emacs-bridge-agents.json` (gitignored) to track active agents across bridge invocations.

The emacs MCP can inspect buffers. Use that to feedback loop changes.
