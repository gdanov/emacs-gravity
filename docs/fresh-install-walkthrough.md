# Fresh Install Walkthrough

Step-by-step record of installing emacs-gravity from scratch on a bare Linux system (Proxmox VE, Debian-based, x86_64). Tested 2026-02-22. Use this as a reference for reproducing the setup or debugging installation issues.

## Starting point

Bare system with only:
- bash, basic `/usr/bin` utilities
- Node.js 22.22.0 (via nvm)
- Claude Code CLI (`~/.local/bin/claude`)
- tmux
- No Emacs, no Make, no grep/head/tail (minimal coreutils)

## Step 1: Install Nix

```bash
curl -L https://nixos.org/nix/install | sh
```

This installs Nix in single-user mode. The `nix` binary lands in `/nix/var/nix/profiles/default/bin/`. User-installed packages go to `~/.nix-profile/bin/`.

### PATH setup

**Gotcha:** Nix installs to two separate profile paths. Both must be on PATH:

- `/nix/var/nix/profiles/default/bin` — the `nix` binary itself
- `~/.nix-profile/bin` — packages installed via `nix profile install`

Add to `~/.bashrc` (before the interactive guard, or after it if only needed in interactive shells):

```bash
# Nix
export PATH="/nix/var/nix/profiles/default/bin:$HOME/.nix-profile/bin:$PATH"
```

**Why not source the profile.d scripts?** The standard Nix profile scripts (`/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh`) may not add `~/.nix-profile/bin` to PATH. The explicit export is simpler and more reliable.

**Gotcha for automation:** `.bashrc` has an interactive guard at the top (`case $- in *i*) ;; *) return;; esac`). Non-interactive shells (CI, `make-process`, `nix develop --command`) skip everything after it. The PATH export must be either:
- Before the guard, or
- Sourced explicitly in scripts: `export PATH="/nix/var/nix/profiles/default/bin:$HOME/.nix-profile/bin:$PATH"`

### Verify flakes are enabled

```bash
nix show-config 2>&1 | grep experimental-features
# Should include: flakes nix-command
```

If not, create `~/.config/nix/nix.conf`:

```
experimental-features = nix-command flakes
```

## Step 2: Install system utilities

The flake dev shell provides Emacs, Node.js, and Make, but basic CLI tools (`grep`, `head`, `find`, etc.) are needed outside the dev shell for general use:

```bash
nix profile install nixpkgs#coreutils nixpkgs#gnugrep nixpkgs#findutils nixpkgs#gnused nixpkgs#direnv nixpkgs#gnumake
```

These install to `~/.nix-profile/bin/`.

## Step 3: Set up direnv

The project has an `.envrc` file containing `use flake`. When direnv is hooked into your shell, it automatically activates the Nix dev shell when you `cd` into the project.

```bash
# Add to ~/.bashrc (after the interactive guard is fine for this one):
eval "$(direnv hook bash)"

# Trust the project:
cd /path/to/emacs-gravity
direnv allow
```

**What direnv provides:** Every new shell in the project directory gets Emacs 30 (with magit-section + transient), Node.js 22, and GNU Make — without any manual activation.

## Step 4: Build the bridge

```bash
cd emacs-gravity

# With direnv (automatic):
make build

# Without direnv (explicit):
nix develop --command make build
```

Output:
```
cd emacs-bridge && npm ci && npm run build
added 56 packages, and audited 57 packages in 4s
> emacs-bridge@1.0.0 build
> tsc
```

**What this does:** Installs npm dependencies and compiles TypeScript to `emacs-bridge/dist/index.js`. The compiled bridge is what Claude Code hooks invoke.

## Step 5: Run tests

```bash
make test        # or: nix develop --command make test
```

### Expected results

**Bridge tests (vitest):** 276/276 pass.

**Elisp tests (ERT):** 73/74 pass. One pre-existing failure:
- `cgc-notification-stores-message` — bug in notification event handler (unrelated to installation)

### Snapshot mismatches on first run

The bridge snapshot tests (`test/enrichment.test.ts`) contain absolute paths from the original development machine. On a new machine, these 5 tests fail with path mismatches:

```
- "transcript_path": "/Users/gdanov/work/playground/emacs-gravity/..."
+ "transcript_path": "/home/youruser/emacs-gravity/..."
```

**Fix:** Update snapshots for your machine:

```bash
cd emacs-bridge && npx vitest run --update
```

This rewrites `test/__snapshots__/enrichment.test.ts.snap` with your local paths. **Do not commit this change** — it's machine-specific.

### Nix flake checks

```bash
nix flake check
```

Runs the ERT test suite in a hermetic sandbox (both GUI and nox Emacs variants). Currently fails due to the 1 pre-existing test failure. Useful for verifying the flake definition is valid.

## Step 6: Register the Claude Code plugin

Create `~/.claude/plugins/marketplace.json`:

```bash
mkdir -p ~/.claude/plugins
```

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

**The `source` path must be absolute** and point to the `emacs-bridge` directory (not the project root). Claude Code resolves plugins at startup.

**Verify:** After restarting Claude Code, hook status messages appear in the Claude Code output (e.g., `gravity: session start`).

## Step 7: Load in Emacs

Add to your Emacs init file:

```elisp
(add-to-list 'load-path "/absolute/path/to/emacs-gravity")
(require 'claude-gravity)
(claude-gravity-server-start)
```

**With Nix:** Launch Emacs from the dev shell so it has magit-section and transient:

```bash
# With direnv active:
emacs

# Without direnv:
nix develop --command emacs
```

### Verify the socket server

```bash
# In a separate terminal:
ls -la ~/.local/state/claude-gravity.sock
# Should exist and show type 's' (socket)
```

Or from Emacs batch mode:

```bash
nix develop --command emacs -nw --batch \
  -L /path/to/emacs-gravity \
  -l claude-gravity \
  --eval '(progn
    (claude-gravity-server-start)
    (message "Socket: %s" claude-gravity-server-sock-path)
    (message "Server: %s" (process-status claude-gravity-server-process))
    (claude-gravity-server-stop)
    (message "Stopped cleanly"))'
```

Expected output:
```
Socket: /home/youruser/.local/state/claude-gravity.sock
Server: listen
Stopped cleanly
```

## Step 8: End-to-end verification

1. Start Emacs with the gravity config loaded
2. `M-x claude-gravity-status` — opens the overview buffer
3. Start a Claude Code session in any terminal (in a project directory)
4. The session appears in the overview buffer within seconds
5. Tool calls, agent activity, and prompts update live

## Flake infrastructure reference

### What the flake provides

| Component | Dev shell (`nix develop`) | Flake checks (`nix flake check`) |
|---|---|---|
| Emacs 30.2 + magit-section + transient | `default` shell (GUI), `nox` shell (terminal) | Used in `elisp-tests` and `elisp-tests-nox` |
| Node.js 22 | Both shells | Not used (bridge tests not in flake checks) |
| GNU Make | Both shells | Not used |

### Dev shells

```bash
nix develop                  # default: Emacs with GUI + Node.js + Make
nix develop .#nox            # nox: Emacs terminal-only + Node.js + Make
```

### Make targets

```bash
make build       # Build the Node.js bridge (npm ci + tsc)
make test        # Run all tests (elisp + bridge)
make test-elisp  # Run ERT tests only
make test-bridge # Run vitest bridge tests only
make clean       # Remove node_modules and dist/
```

### File layout

```
flake.nix          — Nix flake (dev shells + CI checks)
flake.lock         — Pinned dependency versions
.envrc             — direnv config ("use flake")
Makefile           — Build and test targets
.gitignore         — Excludes: dist/, node_modules/, .direnv/, result
```

## Known issues

### `make-network-process` warning on load

```
claude-gravity-daemon.el: Warning: `make-network-process´ called without required keyword argument :service
```

Harmless. The daemon module (on hold) has a top-level `make-network-process` call that fires at load time. Does not affect functionality.

### direnv slow on first activation

First `direnv allow` triggers a full Nix evaluation and may download packages from the binary cache. Takes 2-5 minutes. Subsequent activations are instant (cached).

### Socket path

Default socket: `~/.local/state/claude-gravity.sock`. Override with `CLAUDE_GRAVITY_SOCK` environment variable. Both the bridge and Emacs must use the same path.

### Bridge logs

All bridge activity is logged to `/tmp/emacs-bridge.log`. Check this first when debugging hook issues:

```bash
tail -f /tmp/emacs-bridge.log
```
