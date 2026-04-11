# emacs-gravity

An idiomatic Emacs UI for Claude Code — structured, navigatable, fully native.

emacs-gravity gives you a magit-style working memory interface for Claude Code. Instead of watching a scrolling terminal, you get a structured, collapsible view of every session: plans, tool executions, agent trees, file changes, and tasks — all keyboard-navigatable, all in Emacs.

Claude Code's TUI is hidden by default. You see the conversation as data, not as a chat log.

---
# Current state


I'm using it as my daily driver for several projects with Claude Code TUI. While stability is imploving, we're at beta quality, mostly.

- unmanaged sessions are the most frequently used and have the least amount of bugs
  * the most confusing "feature" is that in 90% of the cases you see permission request or question displayed both in TUI and Gravity. The remaining 10% Claude may appear "stuck" because the request/question is not shown in the TUI, but only in Gravity. The menubar indicator helps a lot with this case.
* managed sessions (tmux) are not well tested yet. They are my next priority.
  * known issue is you have to have running tmux before Gravity can start managed session.
* performance is OK. With many sessions and frequent updates editors freeze too often. I regularly have performance focused sessions, but I'm not sure we can ever get to state where Gravity buffers are receiving frequent updates and regular editors feel as smooth as usual.
  
This project has been vibecoded on purpose as experiment, so desipite my best effort to keep complete test suite, regressions happen.

Project is being used & developed exclusively on MacOS, so windows & linux users may hit obstacles I'm unaware of.

---

## v4: GitHub-marketplace distribution

> **No user-facing behaviour change.** v4 is strictly about how the plugin is packaged and delivered. If you were already running v3 from a local checkout, everything you see inside Emacs still works the same. What changed is installation and updates.

### What changed

1. **Self-contained plugin bundle** — Both the Node bridge and gravity-server are now bundled with esbuild into `dist/emacs-bridge.mjs` and `dist/gravity-server.mjs`. No `npm install` at the install site, no `tsx`/`node_modules` shipping with the plugin, no `${CLAUDE_PLUGIN_ROOT}` hard-coded paths.
2. **GitHub-sourced marketplace** — `.claude-plugin/marketplace.json` is now hosted in this repo as a `github` source. End users install with `/plugin marketplace add gdanov/emacs-gravity` instead of hand-editing an absolute-path local marketplace file.
3. **Auto-updating releases** — A GitHub Actions workflow on every push to `master` bumps the semver version (derived from Conventional Commits), rebuilds the bundles, commits them back, tags, and publishes a GitHub Release. Clients with auto-update enabled for the marketplace pick up new versions on Claude Code startup. See the [Releases](#releases) and [Commit conventions](#commit-conventions) sections below.
4. **Contributor/user paths cleanly separated** — [INSTALL.md](INSTALL.md) now has a **Quick install** section for end users (marketplace command), and contributor-only sections for running from a local checkout via `make sync-cache` + a local marketplace entry.

### Migrating from v3

- **Drop the old local marketplace entry** — If your `~/.claude/plugins/marketplace.json` has an absolute-path `source` pointing at this repo, remove it (or uninstall via `claude plugin uninstall 'emacs-bridge@local-emacs-marketplace'` and `claude plugin marketplace remove local-emacs-marketplace`).
- **Install from the GitHub marketplace**:
  ```
  /plugin marketplace add gdanov/emacs-gravity
  /plugin install emacs-bridge@emacs-gravity-marketplace
  ```
- **Enable auto-update** (optional) — `/plugin` → **Marketplaces** → `emacs-gravity-marketplace` → **Enable auto-update**. Off by default for third-party marketplaces.
- **No `init.el` change needed** — `(claude-gravity-server-start)` still works as before.

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
- **Drop the old local marketplace entry** — If your `~/.claude/plugins/marketplace.json` has an absolute-path `source` pointing at this repo, remove it. The plugin is now installed from the GitHub marketplace (see below).
- **Install from the GitHub marketplace** — `/plugin marketplace add gdanov/emacs-gravity` then `/plugin install emacs-bridge@emacs-gravity-marketplace`.
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
Launch and manage Claude Code sessions in tmux directly from Emacs. Compose prompts in a dedicated buffer, send via `C-c C-c`. External sessions running inside tmux are fully integrated via hooks. Session liveness is monitored server-side.

## How it compares

### vs other Emacs integrations

|                          | emacs-gravity                                | [claude-code-ide.el](https://github.com/manzaltu/claude-code-ide.el)  | [claude-code.el](https://github.com/stevemolitor/claude-code.el) + [monet](https://github.com/stevemolitor/monet) | [gptel](https://github.com/karthink/gptel)     | [aider.el](https://github.com/tninja/aider.el) / [aidermacs](https://github.com/MatthewZMD/aidermacs) |
|--------------------------|----------------------------------------------|------------------------------------------------------------------------|------------------------------------------------------------------|-------------------------------------------------|--------------------------------------------------------------------------------------------------------|
| **Approach**             | Hook-based server + semantic patches         | MCP bridge (Claude calls Emacs)                                        | Terminal emulator (eat/vterm) + IDE protocol via WebSocket (monet) | Direct API client                               | Aider CLI wrapper                                                                                      |
| **What you see**         | Structured turn tree (magit-section)         | Terminal output + MCP-enhanced context                                 | Raw terminal output; monet adds inline diffs and diagnostics     | Chat buffer                                     | Terminal output + ediff                                                                                |
| **Multi-session**        | Yes — per-project grouping, overview buffer  | No                                                                     | Yes — project-aware instances                                    | Multiple buffers                                | Multiple aider sessions                                                                                |
| **Agent tracking**       | Full nested tree with transcript access      | No                                                                     | No                                                               | N/A                                             | No                                                                                                     |
| **Plan review**          | Dedicated buffer, inline comments, diff, structured feedback | No                                              | No                                                               | N/A                                             | No                                                                                                     |
| **Permission mgmt**      | Pattern generation + write to settings       | No                                                                     | No                                                               | N/A                                             | No                                                                                                     |
| **Inline diffs**         | In session buffer alongside tool context     | No                                                                     | Yes (monet — ediff or simple diff)                               | No                                              | Yes (ediff)                                                                                            |
| **Multi-client**         | Yes — Emacs + macOS menu bar (same server)   | No                                                                     | No                                                               | No                                              | No                                                                                                     |
| **Capabilities browser** | Plugins, skills, agents, MCP — navigatable   | No                                                                     | No                                                               | No                                              | No                                                                                                     |
| **Needs Claude Code CLI**| Yes                                          | Yes                                                                    | Yes                                                              | No (API only)                                   | No (uses Aider)                                                                                        |
| **Extra dependencies**   | Node.js (gravity-server)                     | None                                                                   | eat or vterm; monet needs websocket.el                           | None                                            | Aider (Python)                                                                                         |

**Different philosophies:** claude-code.el and claude-code-ide.el embed the Claude Code terminal inside Emacs — you interact with Claude's TUI directly. monet enhances claude-code.el with Claude Code's IDE protocol (diffs, diagnostics, go-to-definition) over WebSocket. emacs-gravity hides the TUI entirely and reconstructs the conversation as structured, navigatable data via hooks and a server-driven architecture. gptel and aider.el are general-purpose LLM tools that happen to support Claude models but don't integrate with Claude Code's hook system, agent framework, or plan workflow.

### vs IDE tools

|                      | Google AntiGravity      | Cursor                  | emacs-gravity                              |
|----------------------|-------------------------|-------------------------|--------------------------------------------|
| **UI paradigm**      | Web panels in IDE       | VS Code sidebar         | magit-section (terminal-native)            |
| **Plan review**      | View + comment          | —                       | View + comment + diff + structured feedback|
| **Agent visibility** | Flat list               | Minimal                 | Full nested tree with transcript access    |
| **Multi-session**    | No                      | No                      | Yes — per-project grouping                 |
| **Multi-client**     | No                      | No                      | Yes — Emacs + menu bar (same server)       |
| **Extensibility**    | Closed                  | Closed (extensions API) | Elisp — fully hackable                     |
| **Runs in**          | Chrome / Electron       | Electron                | Terminal / TTY / SSH                        |
| **Open source**      | No                      | No                      | Yes                                        |

## Getting Started

### Prerequisites
- Emacs 27.1+ with `magit-section` (>= 3.0) and `transient` (>= 0.3)
- Node.js 18+ (only required if you want to build from source; the published plugin ships prebuilt bundles)
- Claude Code CLI installed

### 1. Install the Claude Code plugin

emacs-gravity is published as a Claude Code plugin in a GitHub marketplace. Inside Claude Code, run:

```
/plugin marketplace add gdanov/emacs-gravity
/plugin install emacs-bridge@emacs-gravity-marketplace
```

That pulls the plugin (bundled bridge + gravity-server) directly from this repo. No `npm install`, no path juggling.

> **Auto-update:** Third-party marketplaces have auto-update **disabled by default**. To enable it, open `/plugin` → **Marketplaces** → select `emacs-gravity-marketplace` → toggle **Enable auto-update**. New releases will then be applied on Claude Code startup.

After installing, restart Claude Code. You should see hook status messages (e.g., "gravity: session start") in the status line, confirming the plugin is active.

### 2. Load in Emacs

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

## Releases

The plugin ships via GitHub Releases. Every push to `master` runs a GitHub Actions workflow that tests the bridge + server, rebuilds the bundles, bumps the semver version in `packages/emacs-bridge/.claude-plugin/plugin.json`, commits the bumped manifest and fresh `dist/*.mjs` artifacts back to `master`, tags `v<x.y.z>`, and publishes a GitHub Release with auto-generated notes.

**Version scheme** — derived from the commit history since the previous `v*` tag:

| Commit shape | Bump |
|---|---|
| `fix:` / `chore:` / `docs:` / `refactor:` / `ci:` / `perf:` / `test:` | patch |
| `feat:` or `feat(scope):` (any commit in the batch) | minor |
| `type!:` in subject or `BREAKING CHANGE:` on a line by itself in the body | major |

The workflow skips itself on its own bump commits via `if: !startsWith(github.event.head_commit.message, 'chore(release):')`, so the release loop can't re-enter. The bump logic is a ~80-line Node script at [.github/scripts/bump-version.mjs](.github/scripts/bump-version.mjs); the workflow is [.github/workflows/release.yml](.github/workflows/release.yml).

**Clients** installed via `/plugin marketplace add gdanov/emacs-gravity` automatically pick up new releases on Claude Code startup — provided auto-update is enabled for the marketplace (`/plugin` → **Marketplaces** → `emacs-gravity-marketplace` → **Enable auto-update**; disabled by default for third-party marketplaces).

## Commit conventions

Since the release workflow derives the next version from commit messages, **every commit that lands on `master` must follow [Conventional Commits](https://www.conventionalcommits.org/)**:

```
<type>[optional scope][!]: <subject>

[optional body]

[optional footer(s)]
```

**Types used by this project:**

| Type | Meaning | Bump |
|---|---|---|
| `feat` | New user-visible feature | minor |
| `fix` | Bug fix | patch |
| `perf` | Performance improvement | patch |
| `refactor` | Internal refactor, no behavioural change | patch |
| `docs` | Documentation only | patch |
| `test` | Test changes only | patch |
| `ci` | CI/workflow changes | patch |
| `chore` | Tooling, dependencies, or other housekeeping | patch |

**Breaking changes** are signalled in one of two ways:

1. `!` after the type/scope: `feat!: drop v2 standalone mode` → major
2. A `BREAKING CHANGE:` footer at the start of a line:

   ```
   feat: new permission protocol

   BREAKING CHANGE: permission responses now require an `updatedPermissions`
   field. Older terminal clients will need to be updated.
   ```

Prose mentions inside a commit body that *happen* to contain the literal words "BREAKING CHANGE" or "[skip release]" are safe — both detectors are line-anchored / subject-anchored.

**Scopes** (optional but encouraged): `bridge`, `server`, `emacs`, `menubar`, `tmux`, `docs`, `ci`, `tests`.

## Development

Prerequisites: Node.js 18+, npm, and optionally Emacs 29+ if you want to run the elisp test suite locally.

```bash
git clone git@github.com:gdanov/emacs-gravity.git
cd emacs-gravity
npm install
make build            # esbuild both bundles
make test             # elisp + bridge + server + menubar tests
```

**Build targets** (`Makefile`):

| Target | Description |
|---|---|
| `make build` | Build both plugin bundles (`packages/emacs-bridge/dist/emacs-bridge.mjs`, `packages/gravity-server/dist/gravity-server.mjs`) |
| `make build-bridge` | Bridge bundle only |
| `make build-server` | Server bundle only |
| `make test` | Run everything: elisp + bridge + server + macOS menu bar |
| `make test-bridge` | vitest in `packages/emacs-bridge/` |
| `make test-server` | vitest in `packages/gravity-server/` |
| `make test-elisp` | ERT tests via batch Emacs |
| `make sync-cache` | **Contributor dev loop:** builds both bundles and copies them into `~/.claude/plugins/cache/local-emacs-marketplace/emacs-bridge/<version>/`, where `<version>` is read from `plugin.json` via `jq`. Pair with a `~/.claude/plugins/marketplace.json` pointing at your checkout (see [INSTALL.md](INSTALL.md#register-the-claude-code-plugin-contributors-only)) so Claude Code loads the local version instead of the published release. |
| `make kill-server` / `make restart-server` | Manage the running gravity-server during development |
| `make menubar` | Build and run the macOS menu bar app |

**Contributor install:** See [INSTALL.md](INSTALL.md) for the full contributor setup, including the Nix dev shell and the local-marketplace registration needed for Claude Code to load your in-repo checkout instead of the published plugin.

**PR workflow:** Branch off `master`, open a PR targeting `master`. CI on merge will run the release workflow — if your PR contains a `feat:` commit it will bump minor, otherwise patch. There is no separate release action to take.

**Further docs:**

- [DEVELOPMENT.md](DEVELOPMENT.md) — full build, debug, and testing guide (pitfalls, performance notes)
- [ARCHITECTURE.md](ARCHITECTURE.md) — system design, modules, hooks, state API
- [AGENTS.md](AGENTS.md) — agent workflow and "landing the plane" protocol
- [UI-SPEC.md](UI-SPEC.md) — visual specification for every UI state

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
