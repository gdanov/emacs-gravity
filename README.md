# emacs-gravity

An idiomatic Emacs UI for Claude Code — structured, navigatable, fully native.

emacs-gravity gives you a magit-style working memory interface for Claude Code. Instead of watching a scrolling terminal, you get a structured, collapsible view of every session: plans, tool executions, agent trees, file changes, and tasks — all keyboard-navigatable, all in Emacs.

Claude Code's TUI is hidden by default. You see the conversation as data, not as a chat log.

<!-- screenshot: hero.png — overview buffer with a couple of sessions expanded -->

## Why emacs-gravity?

**You already live in Emacs.** Claude Code is powerful, but its terminal UI is a firehose — scrolling text, interleaved tool calls, agent output mixed with thinking. emacs-gravity restructures all of that into something you can actually navigate.

- **Idiomatic Emacs UI** — Built on magit-section. Collapsible trees, TAB to expand, RET to visit. If you know magit, you know emacs-gravity.
- **Structured session transcripts** — Every tool call, agent dispatch, thinking block, and assistant message organized into turns. Expand what matters, collapse the rest.
- **Deep agent visibility** — Subagents render as nested trees. See their tool calls, thinking, and results. Inspect transcripts with a keystroke.
- **Multi-session message center** — Track multiple Claude sessions simultaneously, grouped by project. Switch between them like buffers.
- **Inline diffs** — File edits shown as unified diffs directly in the session buffer. No context switching.
- **Capabilities browser** — Skills, agents, commands, and MCP servers from all plugins rendered in a navigatable tree.
- **Plan review with feedback** — When Claude proposes a plan, review it in a dedicated buffer. Add inline comments (wave-underline overlays), edit the text, write `@claude:` markers. Approve sends clean; any feedback auto-converts to a structured deny with your annotations, diffs, and comments bundled.
- **Permission management** — Generate allow-patterns from tool signatures, copy to clipboard, or write directly to `settings.local.json`.
- **Runs anywhere Emacs runs** — Terminal, TTY, SSH, tmux. No Electron, no browser, no GUI required.
- **Turn-based YOLO mode** - go YOLO just for one turn when you know what to expect. Useful in planning mode.

## How it compares

|                      | Google AntiGravity      | Cursor                  | emacs-gravity                              |
|----------------------|-------------------------|-------------------------|--------------------------------------------|
| **UI paradigm**      | Web panels in IDE       | VS Code sidebar         | magit-section (terminal-native)            |
| **Plan review**      | View + comment          | —                       | View + comment + diff + structured feedback|
| **Agent visibility** | Flat list               | Minimal                 | Full nested tree with transcript access    |
| **Multi-session**    | No                      | No                      | Yes — per-project grouping                 |
| **Capabilities**     | Hidden                  | Hidden                  | Browsable tree (plugins, skills, MCP)      |
| **Permission mgmt**  | —                       | —                       | Pattern generation + settings integration  |
| **Inline diffs**     | No                      | In editor               | In session buffer alongside tool context   |
| **Extensibility**    | Closed                  | Closed (extensions API) | Elisp — fully hackable                     |
| **Runs in**          | Chrome / Electron       | Electron                | Terminal / TTY / SSH                        |
| **Open source**      | No                      | No                      | Yes                                        |

**Philosophy:** emacs-gravity is text-based, keyboard-driven, and fully extensible. It treats the AI conversation as structured data, not a chat window. You own your tools — every face, keybinding, and rendering function is yours to customize.

## Key Features

### Inbox 
See all pending approvals, plan reviews and other actions in the inbox. React when you want on what you want. 

![alt text](<Screenshot 2026-02-18 at 15.36.30.png>)

### Overview Buffer
All active sessions at a glance, grouped by project. Status indicators show idle/responding/ended. Session counts, tool counts, elapsed time.

![alt text](<Screenshot 2026-02-21 at 23.40.45.png>)

### Session Detail
Turn-based conversation view. Each user prompt starts a turn; tools are grouped into response cycles with collapsible headings. Assistant thinking (purple) and text (orange) rendered with margin indicators.

![alt text](<Screenshot 2026-02-20 at 13.35.56.png>)

### Plan Review
Dedicated buffer for reviewing Claude's plans. Add inline comments with `c` (orange wave-underline overlays). Edit the plan text directly. View diffs with `C-c C-d`. Smart approve: clean plans pass through, annotated plans auto-deny with structured feedback.

![alt text](<Screenshot 2026-02-22 at 0.30.47.png>)

### Agent Tracking
Subagents render as nested trees inside the turn that spawned them. See each agent's tool calls, thinking, and completion text. Running agents highlighted with gold background. Completed agents show duration and summary.

### Inline and expanded Diffs
File edits displayed as unified diffs directly in the session buffer, alongside the tool that made them. Read, Edit, and Write operations tracked per file with aggregated diffs available via SPC.


![alt text](<Screenshot 2026-02-22 at 0.22.44.png>) ![alt text](<Screenshot 2026-02-22 at 0.22.50.png>)

### Permission Management
When Claude requests permissions, review in a dedicated buffer. Generate allow-patterns from tool signatures with `A` (copy) or `a` (write to settings). Pattern suggestions based on tool name and arguments.

<!-- screenshot: permissions.png -->


### File & Task Tracking
Files section shows all read/edit/write operations with per-file aggregation. Tasks section tracks TaskCreate/TaskUpdate lifecycle with status indicators ([x] done, [/] in progress, [ ] pending).

### Capabilities Browser
All plugins, skills, agents, commands, and MCP servers rendered in a collapsible tree. See what's available across global, project, and plugin scopes. Navigate to source files with RET.

![alt text](<Screenshot 2026-02-17 at 10.29.53.png>)

### Tmux Integration
Launch and manage Claude Code sessions in tmux directly from Emacs. Compose prompts in a dedicated buffer, send via `C-c C-c`. Heartbeat monitoring detects dead sessions.

Sessions started outside of emacs are integrated as well via hooks (but no prompting). External sessions running inside tmux are fully integrated.

## Architecture

```
Claude Code (11 hook events)
    ↓
emacs-bridge (Node.js one-shot plugin)
    ↕ JSON over Unix domain socket
claude-gravity.el (Emacs — 13 modules, ~5500 lines)
    ↓
magit-section UI
```

Event-driven: Claude Code fires hooks → bridge forwards over socket → Emacs maintains state in hash tables → renders via magit-section. All hooks are fire-and-forget except PermissionRequest (bidirectional).

See [ARCHITECTURE.md](ARCHITECTURE.md) for the full system design.

## Getting Started

### Prerequisites
- Emacs 29.1+ with `magit-section` (≥ 3.0) and `transient` (≥ 0.3)
- Node.js 18+ and npm
- Claude Code CLI installed

### 1. Build the Node.js bridge

```bash
cd emacs-gravity/emacs-bridge
npm install
```

### 2. Register the Claude Code plugin

The bridge is a Claude Code plugin that hooks into lifecycle events. You need to register it via a marketplace file.

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
      "source": "/absolute/path/to/emacs-gravity/emacs-bridge"
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

This starts the Unix domain socket server that receives events from the bridge.

### Quick Start

```
M-x claude-gravity-status    — open the overview buffer
TAB                          — expand/collapse sections
RET                          — visit session or file
?                            — transient menu (all commands)
```

## Documentation

- [ARCHITECTURE.md](ARCHITECTURE.md) — System design, modules, hooks, state API
- [DEVELOPMENT.md](DEVELOPMENT.md) — Build, debug, test workflows
- [UI-SPEC.md](UI-SPEC.md) — Visual specification for all UI states
- [docs/emacs-driven-sessions.md](docs/emacs-driven-sessions.md) — Managed session architecture
- [docs/tmux-interactive-sessions.md](docs/tmux-interactive-sessions.md) — Tmux integration

## On Hold

- **OpenCode support** — Basic infrastructure exists for [OpenCode](https://github.com/opencode-ai/opencode) integration, but it's on hold pending further development on their side.
- **Anthropic Agent SDK bridge** — A near-complete bridge using `@anthropic-ai/claude-agent-sdk` for a long-running daemon mode (no hooks needed). On hold because the SDK requires a pay-per-use API key — using it with a Claude Max/Pro subscription violates Anthropic's TOS. See [#6536](https://github.com/anthropics/claude-code/issues/6536). Code lives in `daemon.ts` / `daemon-session.ts` / `claude-gravity-daemon.el`.

## License

GPL-3.0. See [LICENSE](LICENSE).
