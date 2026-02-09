# emacs-gravity

Emacs UI for Claude Code, inspired by Google's AntiGravity and Cursor. A Magit-style working memory interface that displays Claude Code's plan, tasks, tool execution status, and supports commenting/annotations.

## Features

- **Multi-session support** — Track multiple Claude Code sessions simultaneously, one buffer per session
- **Turn-based response cycles** — Tools and assistant text grouped by turn with collapsible sections
- **Live streaming** — See tool execution and assistant responses as they happen
- **Agent tracking** — Monitor subagents with nested rendering and transcript inspection
- **Plan review** — Interactive approval/denial with inline comments and diff viewing
- **Managed sessions** — Launch and manage Claude Code as a subprocess from Emacs
- **File tracking** — Monitor which files are read, edited, or written
- **Task tracking** — See TaskCreate/TaskUpdate/TaskList lifecycle with status indicators
- **Permission management** — Grant/deny tool permissions with pattern generation
- **Comment overlays** — Annotate tools and items with wave-underline comments

## Architecture

```
Claude Code Hook Events
    ↓
emacs-bridge (Node.js plugin)
    ↕ Unix domain socket
claude-gravity.el (Emacs UI server)
    ↓
magit-section (hierarchical collapsible UI)
```

The system is event-driven. Claude Code fires hooks on tool execution, agent transitions, and user actions. The bridge forwards these events over a local Unix socket to Emacs, which maintains session state and renders an interactive UI.

## Installation

### 1. Install Emacs Dependencies

You need Emacs 27.1+ with two packages:

```elisp
;; In your init.el or use-package
(require 'magit-section)  ; >= 3.0.0
(require 'transient)      ; >= 0.3.0
```

### 2. Build the Node.js Bridge

```bash
cd emacs-gravity/emacs-bridge
npm install
npm run build
```

### 3. Load in Emacs

Add to your `~/.emacs.d/init.el`:

```elisp
;; Load emacs-gravity and start the socket server
(add-to-list 'load-path "/path/to/emacs-gravity")
(require 'claude-gravity)
(claude-gravity-server-start)
```

### 4. Register the Plugin

Claude Code discovers plugins via `~/.claude/plugins/marketplace.json`. The emacs-gravity plugin is registered in the file at `emacs-bridge/marketplace.json`. Ensure Claude Code loads it:

```bash
# Verify plugin is listed
cat ~/.claude/plugins/marketplace.json | grep emacs-gravity
```

## Usage

### Open the Overview Buffer

```
M-x claude-gravity-status
```

This opens the `*Structured Claude Sessions*` buffer showing all active sessions grouped by project.

### Navigate and Interact

| Key | Action |
|-----|--------|
| `TAB` | Toggle section expansion |
| `RET` | Visit session or open file |
| `g` | Refresh buffer |
| `c` | Add comment overlay on section |
| `P` | Show full plan in side buffer |
| `t` | Collapse all, focus latest turn |
| `?` | Open transient menu (all commands) |

### Manage Sessions

From the overview buffer (or inside a session):

| Key | Action |
|-----|--------|
| `D` | Remove ended sessions |
| `K` | Stop/kill managed session |
| `R` | Reset all status to idle |
| `X` | Detect dead sessions |
| `d` | Delete session at point |

### Review Plans & Permissions

When Claude Code enters plan mode:
- Emacs opens `*Claude Plan Review*` buffer
- Add comments with `c` (wave-underline highlights)
- View diff with `C-c C-d`
- Approve with `C-c C-c` (auto-denies if feedback found)
- Deny with `C-c C-k` (with structured feedback)

## Documentation

- **@CLAUDE.md** — Guide for Claude Code when working on this project
- **@ARCHITECTURE.md** — System design: modules, hooks, state management, migration history
- **@DEVELOPMENT.md** — Build commands, dependencies, debugging, testing procedures
- **@UI-SPEC.md** — Visual specification for all UI states and keybindings
- **@AGENTS.md** — Agent workflow and landing-the-plane protocol
- **@plan.md** — Project roadmap and feature backlog
- **@docs/emacs-driven-sessions.md** — Research on managing Claude Code as subprocess
- **@docs/tmux-interactive-sessions.md** — Tmux integration approach

## Development

To contribute:

1. Read @ARCHITECTURE.md to understand the module structure
2. Read @DEVELOPMENT.md for build and test procedures
3. Edit Emacs Lisp files in `claude-gravity-*.el`
4. Use `M-x eval-defun` to reload after changes
5. Test with `npm test` for bridge changes

## License

[License here]
