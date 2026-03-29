# CLAUDE.md

**IMPORTANT:** This file is your entry point. For detailed information on specific topics, use @path references below to load the relevant documentation file.

## Project Overview

Emacs UI for Claude Code, inspired by Google's AntiGravity and Cursor. Provides a Magit-style working memory interface that displays Claude Code's plan, tasks, tool execution status, and supports commenting/annotations.

## Quick Architecture

```
Claude Code (12 hooks)
    ↓
emacs-bridge (Node.js, one-shot shim)
    ↓ hook socket (~/.local/state/gravity-hooks.sock)
gravity-server (TypeScript, long-running)
    ├── enrichment, state management, inbox
    ↓ semantic patches over terminal socket (~/.local/state/gravity-terminal.sock)
Terminal clients
    ├── Emacs client (claude-gravity-client.el) → magit-section UI
    └── macOS menu bar (gravity-menubar, Swift) → status dots + dropdown
```

**Server-driven architecture:** gravity-server owns all session state. Terminal clients receive semantic patches and send user actions. Multiple terminals can connect simultaneously. Currently two clients: the Emacs package (full session UI) and a macOS menu bar app (at-a-glance status).

**Monorepo structure:** `packages/{shared, emacs-bridge, gravity-server, gravity-menubar}` with npm workspaces (menubar is a standalone Swift package).

For detailed architecture, see @ARCHITECTURE.md. For the v3 design rationale, see @docs/refactor-implementation.md.

## Module Structure (Summary)

The Emacs package is split into 15 modular files:

| Module | Purpose |
|--------|---------|
| `claude-gravity-core.el` | Utilities, logging, custom variables, tlist |
| `claude-gravity-faces.el` | 37 faces and fringe bitmaps |
| `claude-gravity-session.el` | Session state CRUD |
| `claude-gravity-discovery.el` | Plugin/skill/agent/MCP capability discovery |
| `claude-gravity-state.el` | Session state helpers, inbox, tool/agent lookup |
| `claude-gravity-text.el` | Text rendering: dividers, markdown, wrapping |
| `claude-gravity-diff.el` | Inline diffs, tool/plan display |
| `claude-gravity-render.el` | UI section rendering |
| `claude-gravity-ui.el` | Buffers, keymaps, transient menu |
| `claude-gravity-plan-review.el` | Plan review buffer and feedback flow |
| `claude-gravity-client.el` | Terminal socket client to gravity-server |
| `claude-gravity-actions.el` | Permission and question action buffers |
| `claude-gravity-tmux.el` | Tmux session management |
| `claude-gravity-daemon.el` | Agent SDK daemon bridge (ON HOLD) |
| `claude-gravity-debug.el` | Terminal protocol debug viewer |
| `claude-gravity.el` | Thin loader |

**Load order:** `core → {faces,session,discovery} → state → {text,diff} → render → ui → plan-review → actions → client → {tmux,daemon,debug}`

For line counts, key functions, and dependency details, see @ARCHITECTURE.md.

## Hook System

12 events: `SessionStart`, `SessionEnd`, `PreToolUse`, `PostToolUse`, `PostToolUseFailure`, `SubagentStart`, `SubagentStop`, `UserPromptSubmit`, `Stop`, `Notification`, `PermissionRequest`, `AskUserQuestionIntercept`.

Fire-and-forget except `PermissionRequest` and `AskUserQuestionIntercept` (bidirectional, wait for user response).

Registered in `hooks.json` and forwarded by shell scripts to the Node.js bridge.

## Key Features

- Multi-session tracking with per-session buffers
- Live turn-based response steps with collapsible sections
- Agent tracking and transcript viewing
- Plan review with inline comments and diff
- Task and file operation tracking
- Permission management with pattern generation
- Comment overlays on tools and items
- Managed Claude Code subprocess (experimental)

For complete visual specification and keybindings, see @UI-SPEC.md.

## Development

For build commands, dependencies, debugging, and testing, see @DEVELOPMENT.md.

Use `npm install` at the monorepo root, `make test` for all tests, and `M-x eval-defun` for Emacs Lisp changes.

## Related Documentation

Use @path to read detailed information on specific topics:

- @README.md — User-facing overview for GitHub
- @ARCHITECTURE.md — System design: modules, hooks, state API, migration history
- @DEVELOPMENT.md — Build, debug, test workflows and dependencies
- @UI-SPEC.md — Visual specification for all UI states and keybindings
- @AGENTS.md — Agent workflow and landing-the-plane protocol
- @plan.md — Project roadmap and feature backlog
- @docs/refactor-implementation.md — v3 design: gravity-server architecture and terminal protocol
- @docs/session-data-model.md — Session plist structure and turn tree reference
- @docs/emacs-driven-sessions.md — Managed sessions research (historical)
- @docs/tmux-interactive-sessions.md — Tmux integration approach
