# emacs-gravity

An idiomatic Emacs UI for Claude Code — structured, navigatable, fully native.

emacs-gravity gives you a magit-style working memory interface for Claude Code. Instead of watching a scrolling terminal, you get a structured, collapsible view of every session: plans, tool executions, agent trees, file changes, and tasks — all keyboard-navigatable, all in Emacs.

Claude Code's TUI is hidden by default. You see the conversation as data, not as a chat log.

---

## v3: Server-Driven Architecture

> **Breaking change.** v3 is a ground-up rewrite of the backend. The v2 standalone mode (where Emacs held all state directly) is gone. **You must remove v2 completely before upgrading** — gravity-server is now required.

### What changed

1. **Server-driven architecture** — All session state moved from Emacs Lisp hash tables to gravity-server, a long-running TypeScript/Effect backend. Emacs is now a thin terminal client that applies semantic patches and renders via magit-section.
2. **Multi-client support** — Multiple terminals connect to the same server simultaneously. The macOS menu bar app and Emacs share the same live state. First responder wins for inbox actions.
3. **Semantic patch protocol** — Typed incremental updates (`add_tool`, `complete_agent`, `set_plan`) replace full state rebuilds. Any client that speaks the protocol can render the full UI — future web or native dashboards included.
4. **Centralized enrichment** — Transcript parsing, agent attribution, and event enrichment run server-side with full in-memory state. No more file I/O for agent tracking.
5. **Inbox system** — Permissions, plan reviews, and questions from all sessions funnel into a centralized inbox. Any connected terminal can respond.
6. **macOS menu bar client** — New lightweight Swift app with colored status dots per session, project-grouped dropdown, health monitoring, and auto-reconnect.
7. **Two-socket architecture** — Hook socket (bridge shim, one-shot per event) and terminal socket (persistent client connections) are cleanly separated.
8. **Monorepo with shared types** — `packages/{shared, emacs-bridge, gravity-server}` with npm workspaces and type-safe protocol definitions shared across all components.
9. **Beads issue tracking integration** — Projects with a `.beads/` directory get a live Issues section in the overview buffer. Open, in-progress, and blocked issues displayed per project with priority labels, status indicators, and async refresh.

![Live session detail with inline diffs](<Screenshot 2026-03-29 at 19.54.59.png>)

![Permission management with menubar status](<Screenshot 2026-03-29 at 19.53.16.png>) ![Plan review with menubar status](<Screenshot 2026-03-29 at 20.03.53.png>)

See [ARCHITECTURE.md](ARCHITECTURE.md) for the full v3 design and [docs/refactor-implementation.md](docs/refactor-implementation.md) for the design rationale.

### Migrating from v2

- **Remove old socket config** — v2 connected the bridge directly to Emacs. Delete any custom socket paths pointing at Emacs.
- **Delete agent state files** — Remove `~/.claude/emacs-bridge-agents.json` (agent state is now in-memory in gravity-server).
- **Clean install** — Run `npm install` at the monorepo root.
- **Re-register the plugin** — Update `marketplace.json` if needed (the `source` path is unchanged).
- **Update your init.el** — Replace `(claude-gravity-start)` with `(claude-gravity-server-start)`.
- **Delete stale byte-compiled files** — Run `rm -f *.elc` in the project root. Emacs loads `.elc` over `.el`, so stale compiled files silently override your changes.

---

## Why emacs-gravity?

**You already live in Emacs.** Claude Code is powerful, but its terminal UI is a firehose — scrolling text, interleaved tool calls, agent output mixed with thinking. emacs-gravity restructures all of that into something you can actually navigate.

- **Idiomatic Emacs UI** — Built on magit-section. Collapsible trees, TAB to expand, RET to visit. If you know magit, you know emacs-gravity.
- **Structured session transcripts** — Every tool call, agent dispatch, thinking block, and assistant message organized into turns. Expand what matters, collapse the rest.
- **Deep agent visibility** — Subagents render as nested trees. See their tool calls, thinking, and results. Inspect transcripts with a keystroke.
- **Multi-session message center** — Track multiple Claude sessions simultaneously, grouped by project. Switch between them like buffers.
- **Plan review with feedback** — Review Claude's plans in a dedicated buffer. Add inline comments, edit the text, write `@claude:` markers. Smart approve: clean plans pass through, annotated plans auto-deny with structured feedback.
- **Runs anywhere Emacs runs** — Terminal, TTY, SSH, tmux. No Electron, no browser, no GUI required.
- **Turn-based YOLO mode** — Go YOLO for just one turn when you know what to expect. Useful in planning mode.

## Architecture

emacs-gravity is a **server-driven, multi-client system**. A long-running TypeScript backend owns all session state and broadcasts semantic patches to any number of connected terminal clients over a typed protocol.

```
Claude Code (12 hook events)
    |
emacs-bridge (Node.js one-shot shim)
    | hook socket
gravity-server (TypeScript, long-running backend)
    |-- enrichment, state management, inbox
    | semantic patches over terminal socket
Terminal clients
    |-- Emacs client (15 modules, ~11.5k lines) -- magit-section UI
    '-- macOS menu bar (gravity-menubar, Swift) -- status dots + dropdown
```

**How it works:** Claude Code fires hook events (PreToolUse, PostToolUse, Stop, SubagentStart, etc.) which the bridge shim forwards to gravity-server. The server enriches events (transcript parsing, agent attribution), manages session state (turn tree, tool/agent indexes, inbox), and emits **semantic patches** — typed operations like `add_tool`, `complete_agent`, `set_plan` — to all connected terminals.

Terminals maintain a read-replica of the session state. Any client that speaks the protocol can render the full gravity UI — Emacs, a native app, or a future web dashboard.

See [ARCHITECTURE.md](ARCHITECTURE.md) for the full system design.

### Monorepo Structure

```
packages/
  shared/          -- Shared types (Session, Patch, protocol messages)
  emacs-bridge/    -- Claude Code plugin (one-shot hook forwarder)
  gravity-server/  -- Stateful backend (state, enrichment, terminal protocol)
  gravity-menubar/ -- macOS menu bar app (Swift, standalone)
```

## Terminal Clients

gravity-server supports multiple simultaneous terminals. Currently two production clients ship with the project.

### Emacs Client

The full interactive UI. 15 Emacs Lisp modules (~11.5k lines) built on magit-section, providing:

- **Session overview** — All active sessions grouped by project with status indicators (idle/responding/ended)
- **Session detail** — Turn-based conversation view with collapsible response steps, inline diffs, agent trees
- **Plan review** — Dedicated buffer with inline comments, `@claude:` markers, diff view, smart approve/deny with structured feedback
- **Permission management** — Review permission requests, generate allow-patterns, write to `settings.local.json`
- **Capabilities browser** — All plugins, skills, agents, commands, and MCP servers in a navigatable tree
- **Tmux integration** — Launch and manage Claude Code sessions in tmux from Emacs; external tmux sessions integrate via hooks
- **Inbox** — Centralized queue of pending approvals, plan reviews, and questions across all sessions

### macOS Menu Bar

`gravity-menubar` is a lightweight Swift app that connects to gravity-server alongside Emacs. It provides at-a-glance session status without switching to your terminal.

- **Status dots** — One colored circle per active session: green (idle), yellow (responding), orange (needs attention)
- **Session dropdown** — Sessions grouped by project with status labels ("idle 3m", "responding", "ended") and pending inbox items
- **Health monitoring** — 10-second heartbeat with 30-second timeout detection, auto-reconnect on disconnect
- **Icon state machine** — Menu bar icon reflects aggregate state: disconnected, attention (inbox items waiting), responding, neutral

The menu bar is read-only — it observes session state but doesn't send actions back to the server. Think of it as a dashboard you glance at while working in another app.

Build and run: `cd packages/gravity-menubar && swift build && swift run`

Requires macOS 13+ and Swift 5.9+.

## Key Features

### Inbox
See all pending approvals, plan reviews and other actions in the inbox. React when you want on what you want.

![Inbox](<Screenshot 2026-02-18 at 15.36.30.png>)

### Overview Buffer
All active sessions at a glance, grouped by project. Status indicators show idle/responding/ended. Session counts, tool counts, elapsed time.

![Overview](<Screenshot 2026-02-21 at 23.40.45.png>)

### Session Detail
Turn-based conversation view. Each user prompt starts a turn; tools are grouped into response steps with collapsible headings. Assistant thinking (purple) and text (orange) rendered with margin indicators.

![Session Detail](<Screenshot 2026-02-20 at 13.35.56.png>)

### Plan Review
Dedicated buffer for reviewing Claude's plans. Add inline comments with `c` (orange wave-underline overlays). Edit the plan text directly. View diffs with `C-c C-d`. Smart approve: clean plans pass through, annotated plans auto-deny with structured feedback.

![Plan Review](<Screenshot 2026-02-22 at 0.30.47.png>)

### Agent Tracking
Subagents render as nested trees inside the turn that spawned them. See each agent's tool calls, thinking, and completion text. Running agents highlighted with gold background. Completed agents show duration and summary.

### Inline and Expanded Diffs
File edits displayed as unified diffs directly in the session buffer, alongside the tool that made them. Read, Edit, and Write operations tracked per file with aggregated diffs available via SPC.

![Diffs](<Screenshot 2026-02-22 at 0.22.44.png>) ![Expanded Diff](<Screenshot 2026-02-22 at 0.22.50.png>)

### Permission Management
When Claude requests permissions, review in a dedicated buffer. Generate allow-patterns from tool signatures with `A` (copy) or `a` (write to settings). Pattern suggestions based on tool name and arguments.

### File & Task Tracking
Files section shows all read/edit/write operations with per-file aggregation. Tasks section tracks TaskCreate/TaskUpdate lifecycle with status indicators ([x] done, [/] in progress, [ ] pending).

### Capabilities Browser
All plugins, skills, agents, commands, and MCP servers rendered in a collapsible tree. See what's available across global, project, and plugin scopes. Navigate to source files with RET.

![Capabilities](<Screenshot 2026-02-17 at 10.29.53.png>)

### Tmux Integration
Launch and manage Claude Code sessions in tmux directly from Emacs. Compose prompts in a dedicated buffer, send via `C-c C-c`. Heartbeat monitoring detects dead sessions. External sessions running inside tmux are fully integrated via hooks.

## How it compares

|                      | Google AntiGravity      | Cursor                  | emacs-gravity                              |
|----------------------|-------------------------|-------------------------|--------------------------------------------|
| **UI paradigm**      | Web panels in IDE       | VS Code sidebar         | magit-section (terminal-native)            |
| **Plan review**      | View + comment          | —                       | View + comment + diff + structured feedback|
| **Agent visibility** | Flat list               | Minimal                 | Full nested tree with transcript access    |
| **Multi-session**    | No                      | No                      | Yes — per-project grouping                 |
| **Multi-client**     | No                      | No                      | Yes — Emacs + menu bar (same server)       |
| **Capabilities**     | Hidden                  | Hidden                  | Browsable tree (plugins, skills, MCP)      |
| **Permission mgmt**  | —                       | —                       | Pattern generation + settings integration  |
| **Inline diffs**     | No                      | In editor               | In session buffer alongside tool context   |
| **Extensibility**    | Closed                  | Closed (extensions API) | Elisp — fully hackable                     |
| **Runs in**          | Chrome / Electron       | Electron                | Terminal / TTY / SSH                        |
| **Open source**      | No                      | No                      | Yes                                        |

## Getting Started

### Prerequisites
- Emacs 27.1+ with `magit-section` (>= 3.0) and `transient` (>= 0.3)
- Node.js 18+ and npm
- Claude Code CLI installed

### 1. Install dependencies

```bash
cd emacs-gravity
npm install
```

### 2. Register the Claude Code plugin

The bridge is a Claude Code plugin that hooks into lifecycle events. Register it via a marketplace file.

Create or edit `~/.claude/plugins/marketplace.json`:

```json
{
  "name": "local-emacs-marketplace",
  "owner": {
    "name": "your-name",
    "email": "your-email@example.com"
  },
  "plugins": [
    {
      "name": "emacs-bridge",
      "description": "Bridge to Emacs via Unix Socket",
      "source": "/absolute/path/to/emacs-gravity/packages/emacs-bridge"
    }
  ]
}
```

> **Important:** The `source` path must be absolute. Claude Code resolves plugins at startup from this file.

After saving, restart Claude Code. You should see hook status messages (e.g., "gravity: session start") in the Claude Code status line, confirming the plugin is active.

### 3. Load in Emacs

Add to your `~/.emacs.d/init.el`:

```elisp
(add-to-list 'load-path "/path/to/emacs-gravity")
(require 'claude-gravity)
(claude-gravity-server-start)
```

This starts gravity-server (if not already running) and connects Emacs as a terminal client.

### Quick Start

```
M-x claude-gravity-status    — open the overview buffer
TAB                          — expand/collapse sections
RET                          — visit session or file
?                            — transient menu (all commands)
```

## Emacs Modules

The Emacs package is split into 15 modular files with a thin loader:

| Module | Purpose |
|--------|---------|
| `claude-gravity.el` | Thin loader — requires all modules |
| `claude-gravity-core.el` | Utilities, logging, custom variables, tlist |
| `claude-gravity-faces.el` | 37 faces and fringe bitmaps |
| `claude-gravity-session.el` | Session state CRUD |
| `claude-gravity-discovery.el` | Plugin/skill/agent/MCP capability discovery |
| `claude-gravity-state.el` | Session state helpers, inbox, tool/agent lookup |
| `claude-gravity-text.el` | Text rendering: dividers, markdown, wrapping |
| `claude-gravity-diff.el` | Inline diffs, tool/plan display |
| `claude-gravity-render.el` | UI section rendering (turns, tools, agents, tasks) |
| `claude-gravity-ui.el` | Overview/session buffers, keymaps, transient menu |
| `claude-gravity-plan-review.el` | Plan review buffer, comment overlays, feedback flow |
| `claude-gravity-client.el` | Terminal socket client to gravity-server |
| `claude-gravity-actions.el` | Permission/question action buffers, inbox handling |
| `claude-gravity-tmux.el` | Tmux session management, compose buffer |
| `claude-gravity-daemon.el` | Agent SDK daemon bridge (ON HOLD) |
| `claude-gravity-debug.el` | Terminal protocol debug viewer |

**Load order:** `core -> {faces, session, discovery} -> state -> {text, diff} -> render -> ui -> plan-review -> actions -> client -> {tmux, daemon, debug}`

Each module has no circular dependencies, making isolated testing and customization straightforward. Every face, keybinding, and rendering function is yours to override.

## Documentation

- [ARCHITECTURE.md](ARCHITECTURE.md) — System design, modules, hooks, state API
- [DEVELOPMENT.md](DEVELOPMENT.md) — Build, debug, test workflows
- [UI-SPEC.md](UI-SPEC.md) — Visual specification for all UI states
- [docs/refactor-implementation.md](docs/refactor-implementation.md) — v3 design: gravity-server and terminal protocol
- [docs/session-data-model.md](docs/session-data-model.md) — Session plist structure reference
- [docs/tmux-interactive-sessions.md](docs/tmux-interactive-sessions.md) — Tmux integration

## On Hold

- **OpenCode support** — Basic infrastructure exists for [OpenCode](https://github.com/opencode-ai/opencode) integration, but it's on hold pending further development on their side.
- **Anthropic Agent SDK bridge** — A near-complete bridge using `@anthropic-ai/claude-agent-sdk` for a long-running daemon mode (no hooks needed). On hold because the SDK requires a pay-per-use API key — using it with a Claude Max/Pro subscription violates Anthropic's TOS. See [#6536](https://github.com/anthropics/claude-code/issues/6536). Code lives in `daemon.ts` / `daemon-session.ts` / `claude-gravity-daemon.el`.

## License

GPL-3.0. See [LICENSE](LICENSE).
