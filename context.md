# Code Context

## Files Retrieved

### Commit History (last 15 commits)
```
7912777 chore(release): v4.0.6 (patch from 4.0.5)
9d6676d Merge pull request #13 from gdanov/perf/render-latency
d9d1f70 fix(emacs): review fixes — read-process-output-max once, memq guard, kill accum buffer
e4c2d41 fix(emacs): review fixes — deduplicate beads render, fix streaming section ordering
72eb429 perf(emacs): Tier 2 — frozen turn washers + streaming fast-path
75e05b1 perf(emacs): Tier 1 render optimizations + instrumentation
548638d refactor(emacs): convert configuration section to washer pattern
776359e chore(release): v4.0.5 (patch from 4.0.4)
8eb0002 Merge pull request #12 from gdanov/docs/readme-release-sections
cee58d1 chore: resolve merge conflicts with master
61c8dfd fix(marketplace): use git-subdir source for monorepo plugin resolution
e084b44 chore(release): v4.0.4 (patch from 4.0.3)
a3b93e3 chore(beads): record interactions log
cd37335 fix(plugins): drop project-level enabledPlugins={} override
c5ad227 fix(dev): eagerly respawn server in restart-server so clients reconnect
```

### Key Modified Files (from HEAD~10..HEAD)
1. `claude-gravity-ui.el` - Major changes (383 lines diff)
2. `claude-gravity-client.el` - Major changes (124 lines diff)
3. `claude-gravity-state.el` - New functionality (56 lines added)
4. `claude-gravity-render.el` - Washer pattern + frozen turns (32 lines diff)
5. `claude-gravity-session.el` - Turn index support (27 lines diff)
6. `Makefile` - Build process, sync-marketplace (22 lines diff)
7. `.github/workflows/release.yml` - Release path fix (2 lines diff)
8. `packages/emacs-bridge/hooks/_ensure-server` - Drop monorepo fallback
9. `test/claude-gravity-patch-test.el` - Test additions

---

## Key Code Changes

### 1. Rendering Performance - Washer Pattern (PR #13)

**Architecture Change:** Introduced magit "washer" mechanism for lazy section population.

```elisp
;; From claude-gravity-ui.el - Configuration section washer
(defun claude-gravity--insert-configuration-washer (key)
  "Insert a washer-equipped Configuration section."
  (magit-insert-section section (configuration key t)
    (magit-insert-heading ...)
    (let ((sec section)
          (washer-fn nil))
      (setq washer-fn
            (lambda ()
              (if claude-gravity--rendering-p
                  ;; Visibility cache forced open during render → re-attach washer
                  (progn (oset sec washer washer-fn)
                         (oset sec hidden t))
                ;; Real user expansion → populate children
                (claude-gravity--insert-configuration-full project-dir))))
      (oset section washer washer-fn))))
```

**Key Benefits:**
- Frozen turns only render heading; children populated on first TAB expansion
- Configuration section uses washer when capabilities unchanged
- Beads status section uses washer for lazy issue list loading
- Prevents redundant full tree construction on every render

### 2. Streaming Text Fast-Path

**New State Functions (claude-gravity-state.el):**
```elisp
(defvar claude-gravity--streaming-refresh-timers (make-hash-table :test 'equal)
  "Per-session fast debounce timers for streaming-text updates.")

(defcustom claude-gravity-streaming-refresh-interval 0.08
  "Debounce interval for streaming-text updates (seconds).")

(defun claude-gravity--schedule-streaming-refresh (session-id)
  "Schedule a fast streaming-text-only refresh for SESSION-ID."
  ...)

(defun claude-gravity--do-streaming-refresh (session-id)
  "Fast refresh: only update the streaming-text section for SESSION-ID."
  (claude-gravity--update-streaming-text-fast session))
```

### 3. Turn Index for O(1) Lookups

```elisp
;; From claude-gravity-state.el
(defun claude-gravity--get-turn-node (session turn-number)
  "Get turn node for TURN-NUMBER from SESSION, using the turn index."
  (let ((index (plist-get session :turn-index)))
    (when index
      (gethash turn-number index))))

(defun claude-gravity--index-turn (session turn-node)
  "Add TURN-NODE to SESSION's turn index."
  (let ((index (plist-get session :turn-index)))
    (unless index
      (setq index (make-hash-table :test 'eql))
      (plist-put session :turn-index index))
    (puthash (alist-get 'turn-number turn-node) turn-node index)))
```

### 4. Client Process Filter - Buffer-Based Accumulation

```elisp
;; From claude-gravity-client.el - Changed from string concat to buffer
(defvar claude-gravity--client-process-buffer " *gravity-client-accum*"
  "Name of buffer used to accumulate partial JSON from gravity-server.")

(defun claude-gravity--client-filter (_proc string)
  "Process incoming messages from gravity-server.
Accumulates partial data in a buffer and processes complete newline-delimited JSON."
  (let ((buf (get-buffer-create claude-gravity--client-process-buffer)))
    (with-current-buffer buf
      (goto-char (point-max))
      (insert string)
      (goto-char (point-min))
      (while (search-forward "\n" nil t)
        (let ((line (buffer-substring-no-properties (point-min) (1- (point)))))
          (delete-region (point-min) (point))
          ...)))))
```

### 5. Review Fixes - Multiple Small Corrections

**From commit d9d1f70:**
- `read-process-output-max` set once at connect time (1MB batching)
- `memq` guard in `magit-section-set-visibility-hook` to prevent duplicate handlers
- Accumulation buffer killed on disconnect

**From commit e4c2d41:**
- Deduplicated beads render path
- Fixed streaming section ordering
- Removed redundant `magit-section-set-visibility-hook` callback

### 6. Plugin Self-Containment Fix

**From _ensure-server - Dropped monorepo fallback:**
```bash
# Before: Tried 3 locations (GRAVITY_SERVER_BIN → monorepo → plugin cache)
# After: Only plugin layout
SERVER_BIN="$(dirname "$0")/../dist/gravity-server.mjs"
```

**Release workflow fix:**
```yaml
# Changed from:
packages/gravity-server/dist/gravity-server.mjs
# To:
packages/emacs-bridge/dist/gravity-server.mjs
```

### 7. Makefile - sync-marketplace

```makefile
# Renamed: sync-cache → sync-marketplace
# Targets the published emacs-gravity-marketplace install dir
MARKETPLACE_CACHE := $(shell ls -td $(HOME)/.claude/plugins/cache/emacs-gravity-marketplace/emacs-bridge/*/ 2>/dev/null | head -1)

sync-marketplace: build-bridge build-server
    # Stages bundles + eagerly respawns server for client reconnection
```

---

## Architecture Overview

```
┌─────────────────────────────────────────────────────────────────┐
│                     Rendering Pipeline                           │
├─────────────────────────────────────────────────────────────────┤
│  gravity-server                                                │
│       │                                                         │
│       ▼ JSON over terminal socket                               │
│  claude-gravity-client.el                                       │
│       │                                                         │
│       ├──▶ claude-gravity-session.el (turn index, tlist)       │
│       │                                                         │
│       └──▶ claude-gravity-state.el                             │
│                    │                      │                     │
│                    ▼                      ▼                     │
│         claude-gravity-render.el  claude-gravity-ui.el        │
│         (per-session sections)   (overview/session buffers)  │
│                    │                      │                     │
│                    └──────────┬───────────┘                     │
│                               ▼                                  │
│                    Magit Sections with Washers                  │
│                    (lazy child population on TAB)              │
└─────────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────────┐
│                  State Management                                │
├─────────────────────────────────────────────────────────────────┤
│  Session Plist:                                                 │
│    :turns (tlist) → :turn-index (hash table, O(1) lookup)      │
│    :streaming-text → dedicated 80ms refresh timer              │
│                                                                  │
│  UI State:                                                      │
│    claude-gravity--rendering-p (suppresses visibility hooks)   │
│    claude-gravity--config-render-data (capability cache)       │
│    claude-gravity--beads-render-data (beads cache)              │
└─────────────────────────────────────────────────────────────────┘
```

---

## Beads Interactions Summary

Recent closed issues from `.beads/interactions.jsonl`:

| Issue | Status Change | Summary |
|-------|---------------|---------|
| emacs-gravity-9mn | open → closed | App Nap disabled, heartbeat switched to CACurrentMediaTime, 2-miss tolerance |
| emacs-gravity-1ig | in_progress → closed | Self-contained bridge package; monorepo fallback removed |
| emacs-gravity-nof | open → closed | All fixes applied, 90/90 ERT tests pass |
| emacs-gravity-i8h | open → closed | All fixes applied, 90/90 ERT tests pass |
| emacs-gravity-3n3 | open → closed | All fixes applied, 90/90 ERT tests pass |
| emacs-gravity-6c9 | open → closed | All fixes applied, 90/90 ERT tests pass |
| emacs-gravity-7sr | open → closed | All fixes applied, 90/90 ERT tests pass |

---

## Main Themes/Features

### 1. **Rendering Performance (PR #13)**
- Washer pattern for frozen turns, Configuration, Beads sections
- Turn index hash table for O(1) lookups
- Streaming text fast-path (80ms debounce vs full refresh)
- `read-process-output-max` increased to 1MB

### 2. **Review Fixes**
- Buffer-based accumulation (memory efficiency)
- Visibility hook guards to prevent duplicates
- Proper cleanup of accumulation buffer on disconnect
- Section ordering fixes

### 3. **Server/Bridge Management**
- Self-contained plugin package (no monorepo dependency)
- `sync-marketplace` target for development iteration
- Eager server respawn on restart-server
- Proper terminal/hooks socket cleanup

### 4. **Release Automation**
- Automatic version bumping in plugin.json
- Corrected release workflow path (emacs-bridge/dist)
- Interaction logging with beads

---

## Start Here

**Primary entry point:** `claude-gravity-ui.el` (lines 1-100)
- Contains the main render loop and washer implementations
- Start with `claude-gravity-ui-refresh` and `claude-gravity--insert-configuration-washer`

**For understanding the rendering pipeline:**
1. `claude-gravity-render.el` - Section renderers (frozen turn washer at line 626+)
2. `claude-gravity-ui.el` - Buffer construction, washer patterns
3. `claude-gravity-state.el` - Refresh scheduling, turn indexing

**For understanding the server communication:**
1. `claude-gravity-client.el` - Network process, message dispatch
2. `packages/gravity-server/src/gravity-server.ts` - Backend protocol

---

## Test Coverage

- `test/claude-gravity-patch-test.el` - Extended for washer pattern testing
- 90/90 ERT tests passing
- Byte-compile clean
