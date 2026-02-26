# CLAUDE.md

**IMPORTANT:** This file is your entry point. For detailed information on specific topics, use @path references below to load the relevant documentation file.

## Project Overview

Emacs UI for Claude Code, inspired by Google's AntiGravity and Cursor. Provides a Magit-style working memory interface that displays Claude Code's plan, tasks, tool execution status, and supports commenting/annotations.

## Quick Architecture

```
Claude Code
    ↓ (11 hook events)
emacs-bridge (Node.js)
    ↕ (Unix domain socket)
claude-gravity.el (Emacs)
    ↓
magit-section UI
```

One-shot event forwarding: Claude Code hooks invoke the bridge with event JSON, bridge forwards over socket to Emacs, then exits. All state persists in Emacs hash tables and a JSON file (`.claude/emacs-bridge-agents.json`).

For detailed architecture including view model, state API, and module interactions, see @ARCHITECTURE.md.

## Module Structure (Summary)

The Emacs package is split into 13 modular files:

| Module | Purpose |
|--------|---------|
| `claude-gravity-core.el` | Utilities, logging, custom variables |
| `claude-gravity-faces.el` | 37 faces and fringe bitmaps |
| `claude-gravity-session.el` | Session state CRUD |
| `claude-gravity-state.el` | Model API, mutation functions |
| `claude-gravity-events.el` | Event dispatcher (11 hook types) |
| `claude-gravity-text.el` | Text rendering: dividers, markdown, wrapping |
| `claude-gravity-diff.el` | Inline diffs, tool/plan display |
| `claude-gravity-render.el` | UI section rendering |
| `claude-gravity-ui.el` | Buffers, keymaps, transient menu |
| `claude-gravity-socket.el` | Socket server, plan review |
| `claude-gravity-actions.el` | Permission and question buffers |
| `claude-gravity-tmux.el` | Tmux session management |
| `claude-gravity.el` | Thin loader |

**Load order:** `core → {faces,session} → state → events → {text,diff} → render → ui ↔ socket → {actions,tmux}`

For line counts, key functions, and dependency details, see @ARCHITECTURE.md.

## Hook System

11 events: `SessionStart`, `SessionEnd`, `PreToolUse`, `PostToolUse`, `PostToolUseFailure`, `SubagentStart`, `SubagentStop`, `UserPromptSubmit`, `Stop`, `Notification`, `PermissionRequest`.

Fire-and-forget except `PermissionRequest` (bidirectional, waits for user response in Emacs).

Registered in `hooks.json` and forwarded by shell scripts to the Node.js bridge.

## Key Features

- Multi-session tracking with per-session buffers
- Live turn-based response cycles with collapsible sections
- Agent tracking and transcript viewing
- Plan review with inline comments and diff
- Task and file operation tracking
- Permission management with pattern generation
- Comment overlays on tools and items
- Managed Claude Code subprocess (experimental)

For complete visual specification and keybindings, see @UI-SPEC.md.

## Development

For build commands, dependencies, debugging, and testing, see @DEVELOPMENT.md.

Use `npm install` in emacs-bridge and `M-x eval-defun` for Emacs Lisp changes.

## Related Documentation

Use @path to read detailed information on specific topics:

- @README.md — User-facing overview for GitHub
- @ARCHITECTURE.md — System design: modules, hooks, state API, migration history
- @DEVELOPMENT.md — Build, debug, test workflows and dependencies
- @UI-SPEC.md — Visual specification for all UI states and keybindings
- @AGENTS.md — Agent workflow and landing-the-plane protocol
- @plan.md — Project roadmap and feature backlog
- @docs/emacs-driven-sessions.md — Managed sessions research and implementation
- @docs/tmux-interactive-sessions.md — Tmux integration approach
