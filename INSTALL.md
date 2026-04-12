# Installation Guide

Three installation paths, depending on who you are:

- **End users** — Install the plugin from the GitHub marketplace; no clone, no build. Jump to [Quick install](#quick-install).
- **Contributors** — Clone the repo and run from source. Use **Nix** (reproducible) or **manual** (bring your own Emacs/Node.js).

## Prerequisites

- [Claude Code CLI](https://docs.anthropic.com/en/docs/claude-code) installed and authenticated
- Git (only if cloning the repo)

## Quick install

If you just want to use emacs-gravity, install the plugin from the GitHub marketplace directly inside Claude Code:

```
/plugin marketplace add gdanov/emacs-gravity
/plugin install emacs-bridge@emacs-gravity-marketplace
```

This pulls a prebuilt, self-contained bundle — no `npm install`, no path juggling.

> **Auto-update:** Third-party marketplaces have auto-update disabled by default. Enable it in `/plugin` → **Marketplaces** → `emacs-gravity-marketplace` → **Enable auto-update** so new releases are applied on Claude Code startup.
>
> **Important:** After an auto-update, restart any running `claude` CLIs to pick up the new hook wiring. Running sessions retain their startup-time plugin state and won't fire hooks from the updated version until restarted.

After installing, add the Emacs side to your init file (see [Load in Emacs](#load-in-emacs) below) and you're done. The contributor-only sections below are **not** required for end users.

## Option A: Nix (recommended)

Nix handles all dependencies (Emacs, magit-section, transient, Node.js, Make) in one step. Nothing is installed globally — everything lives in the Nix store.

### 1. Install Nix

If you don't have Nix:

```bash
curl -L https://nixos.org/nix/install | sh
```

Follow the prompts. After installation, open a new terminal or source the Nix profile:

```bash
# Single-user install:
. ~/.nix-profile/etc/profile.d/nix.sh

# Multi-user install:
. /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh
```

Verify flakes are enabled:

```bash
nix --version
# If you get "experimental feature 'nix-command' is disabled", add to ~/.config/nix/nix.conf:
# experimental-features = nix-command flakes
```

### 2. Clone and build

```bash
git clone https://github.com/anthropics/emacs-gravity.git  # or your fork
cd emacs-gravity

# Enter the dev shell and build (fetches Emacs 30, Node.js 22, etc.)
nix develop --command make build
```

First run downloads dependencies from the Nix binary cache (~5 min). Subsequent runs are instant.

### 3. Verify

```bash
nix develop --command make test
```

All 276 bridge tests and 73/74 elisp tests should pass.

### 4. Optional: direnv (recommended)

[direnv](https://direnv.net/) automatically activates the dev shell when you `cd` into the project:

```bash
# Install direnv (if not already available)
nix profile install nixpkgs#direnv

# Add the hook to your shell (bash example; see direnv.net for zsh/fish)
echo 'eval "$(direnv hook bash)"' >> ~/.bashrc
source ~/.bashrc

# Trust the project
cd emacs-gravity
direnv allow
```

After this, every new shell in the project directory automatically has Emacs, Node.js, and Make available.

## Option B: Manual

### Dependencies

| Dependency | Version | Purpose |
|---|---|---|
| Emacs | >= 29.1 | Runtime |
| magit-section | >= 3.0 | UI framework (install via `M-x package-install`) |
| transient | >= 0.3 | Menu system (install via `M-x package-install`) |
| Node.js | >= 18 | Bridge build/runtime |
| npm | >= 8 | Bridge dependency management |

### 1. Clone and build

```bash
git clone https://github.com/anthropics/emacs-gravity.git  # or your fork
cd emacs-gravity
npm install
```

## Register the Claude Code plugin (contributors only)

This step is the same for both Nix and manual contributor setups, and is **only** needed if you want Claude Code to load your local checkout instead of the published GitHub-marketplace release.

Run `make sync-cache` from the repo root. It builds both bundles and copies them to a versioned local cache dir:

```bash
make sync-cache
```

That deploys to `~/.claude/plugins/cache/local-emacs-marketplace/emacs-bridge/<version>/` where `<version>` is read from `packages/emacs-bridge/.claude-plugin/plugin.json`. The cache-side bundle picks up your source changes on the next hook fire.

Then register a **local** marketplace so Claude Code sees the cache dir. Create or edit `~/.claude/plugins/marketplace.json`:

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
      "description": "Bridge to Emacs via Unix Socket (local dev)",
      "source": "/absolute/path/to/emacs-gravity/packages/emacs-bridge"
    }
  ]
}
```

**The `source` path must be absolute.** Re-run `make sync-cache` after every TypeScript change; otherwise stale cached code runs instead of your dev version.

> If you already installed the plugin from `gdanov/emacs-gravity` via the quick-install path, uninstall it first (`/plugin uninstall emacs-bridge@emacs-gravity-marketplace`) so the local version is the one Claude Code loads.

After saving, restart Claude Code. You should see hook status messages (e.g., "gravity: session start") confirming the plugin is active.

## Load in Emacs

Add to your Emacs init file (`~/.emacs.d/init.el` or `~/.config/emacs/init.el`):

```elisp
(add-to-list 'load-path "/absolute/path/to/emacs-gravity")
(require 'claude-gravity)
(claude-gravity-server-start)
```

This starts gravity-server (if not already running) and connects Emacs as a terminal client.

**With Nix:** Make sure you launch Emacs from inside the dev shell (`nix develop --command emacs`) or via direnv, so the correct Emacs with magit-section and transient is used.

## Verify it works

1. In Emacs: `M-x claude-gravity-status` opens the overview buffer
2. Start a Claude Code session in any terminal
3. The session should appear in the overview buffer within seconds

### Quick navigation

```
M-x claude-gravity-status    — open the overview buffer
TAB                          — expand/collapse sections
RET                          — visit session or file
?                            — transient menu (all commands)
```

## Troubleshooting

### Bridge not connecting

Check the bridge log:

```bash
tail -f /tmp/emacs-bridge.log
```

### Socket path mismatch

The system uses two Unix sockets:
- **Hook socket:** `~/.local/state/gravity-hooks.sock` (bridge → gravity-server). Override: `GRAVITY_HOOK_SOCK`
- **Terminal socket:** `~/.local/state/gravity-terminal.sock` (gravity-server → Emacs). Override: `GRAVITY_TERMINAL_SOCK`

Both are created by gravity-server on startup.

### Plugin not loading

Verify the plugin is registered:

```bash
cat ~/.claude/plugins/marketplace.json
```

Ensure the `source` path points to the `packages/emacs-bridge` directory (not the project root) and that `src/index.ts` exists inside it.

### Nix: "experimental feature 'flakes' is disabled"

Add to `~/.config/nix/nix.conf`:

```
experimental-features = nix-command flakes
```

### Emacs: "Cannot open load file: magit-section"

If running Emacs outside the Nix dev shell, install magit-section manually:

```
M-x package-install RET magit-section RET
M-x package-install RET transient RET
```
