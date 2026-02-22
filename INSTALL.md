# Installation Guide

Two installation paths: **Nix** (recommended, reproducible) or **manual** (bring your own Emacs/Node.js).

## Prerequisites

- [Claude Code CLI](https://docs.anthropic.com/en/docs/claude-code) installed and authenticated
- Git

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
cd emacs-gravity/emacs-bridge
npm install
npm run build
```

## Register the Claude Code plugin

This step is the same for both installation paths.

The bridge is a Claude Code plugin. Register it by creating `~/.claude/plugins/marketplace.json`:

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

**The `source` path must be absolute.** Claude Code resolves plugins at startup from this file.

After saving, restart Claude Code. You should see hook status messages (e.g., "gravity: session start") confirming the plugin is active.

## Load in Emacs

Add to your Emacs init file (`~/.emacs.d/init.el` or `~/.config/emacs/init.el`):

```elisp
(add-to-list 'load-path "/absolute/path/to/emacs-gravity")
(require 'claude-gravity)
(claude-gravity-server-start)
```

This starts the Unix domain socket server that receives events from the bridge.

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

The bridge and Emacs must agree on the socket path. Default: `~/.local/state/claude-gravity.sock`. Override with the `CLAUDE_GRAVITY_SOCK` environment variable.

### Plugin not loading

Verify the plugin is registered:

```bash
cat ~/.claude/plugins/marketplace.json
```

Ensure the `source` path points to the `emacs-bridge` directory (not the project root) and that `dist/index.js` exists inside it.

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
