# Window Opening Mechanisms Analysis

## Context

This analysis documents all window and buffer display mechanisms used in the emacs-gravity codebase. The goal is to understand the different patterns for opening buffers and their purposes.

## Complete Window Management Patterns

### 1. Full Window Display (Current Window Replacement)

**switch-to-buffer (5 uses)**
- **Overview buffer** (`claude-gravity-ui.el:1383`)
  - Buffer: `*Structured Claude Sessions*`
  - Main entry point for the plugin

- **Plan review** (`claude-gravity-socket.el:562`)
  - Buffer: `*Claude Plan Review: <slug>*`
  - Bidirectional PermissionRequest handler

- **Terminal buffers** (`claude-gravity-tmux.el:753, 755`)
  - Buffer: `*Claude Terminal: <project>*`
  - Tmux-attached terminal display

- **Resume picker** (`claude-gravity-tmux.el:822`)
  - Interactive session resume selection

**pop-to-buffer (3 uses)**
- **Session buffers** (`claude-gravity-ui.el:554, 560`)
  - Buffer: `*Structured Claude Session <slug>*`
  - Per-session detail view

- **Plan view** (`claude-gravity-text.el:313`)
  - Buffer: `*Claude Plan: <label>*`
  - Read-only plan display (non-interactive)

### 2. Side Windows (Interactive/Action Buffers)

**display-buffer-in-side-window + select-window (7 uses)**

All follow the pattern:
```elisp
(display-buffer-in-side-window buf '((side . SIDE) (DIMENSION . SIZE)))
(select-window (get-buffer-window buf))
```

**Bottom side (35-40% height):**
- **Permission requests** (`claude-gravity-actions.el:193`)
  - Buffer: `*Claude Permission: <item-id>*`
  - Height: 35%

- **Questions** (`claude-gravity-actions.el:449`)
  - Buffer: `*Claude Question: <item-id>*`
  - Height: 35%

- **Compose buffers** (`claude-gravity-tmux.el:458`, `claude-gravity-daemon.el:472`)
  - Buffer: `*Claude Compose: <label>*`
  - Height: 40%

**Right side (90-100 columns):**
- **Debug messages** (`claude-gravity-debug.el:141`)
  - Buffer: `*Claude Debug: Messages*`
  - Width: 90 columns

- **Bridge debug** (`claude-gravity-debug.el:515`)
  - Buffer: `*Claude Debug: Bridge*`
  - Width: 100 columns

### 3. Default Display (Helper Buffers)

**display-buffer (2 uses)**
- **Plan diff** (`claude-gravity-socket.el:908`)
  - Buffer: `*Claude Plan Diff*`
  - Unified diff view (triggered by `C-c C-d` in plan review)

- **Tmux summary** (`claude-gravity-tmux.el:551`)
  - Tmux session list

### 4. Window Management Utilities

**get-buffer-window (14 uses)**
- **Visibility checks** (4 uses):
  - `claude-gravity-state.el:210, 246` - Skip rendering if buffer not visible
  - `claude-gravity-debug.el:69, 74` - Check debug buffer visibility

- **Window selection** (6 uses):
  - After `display-buffer-in-side-window` to select the new window

- **Scrolling/cleanup** (4 uses):
  - `claude-gravity-ui.el:1349, 1364` - Tail command window selection
  - `claude-gravity-tmux.el:508, 838` - Window cleanup

**with-selected-window (2 uses)**
- **Tail command** (`claude-gravity-ui.el:1351, 1366`)
  - Ensures scrolling happens in correct window

**delete-window (2 uses)**
- **Compose cleanup** (`claude-gravity-tmux.el:509, 842`)
  - Only location where windows are explicitly closed

**window-width (10 uses)**
- Dynamic text wrapping and divider width calculation
- Files: `claude-gravity-text.el`, `claude-gravity-render.el`, `claude-gravity-ui.el`, `claude-gravity-socket.el`

## Key Design Patterns

### 1. Side Window Pattern (Consistent)
```elisp
(let ((buf (get-buffer-create "*Buffer Name*")))
  (with-current-buffer buf
    ;; Setup buffer content
    )
  (display-buffer-in-side-window buf '((side . bottom) (window-height . 0.35)))
  (select-window (get-buffer-window buf)))
```

**Always followed by select-window** - ensures focus in new buffer for interaction.

### 2. Main Buffer Pattern
```elisp
;; For primary views (overview, sessions)
(switch-to-buffer buf)  ; or (pop-to-buffer buf)
```

**No display-action parameters** - relies on default window selection behavior.

### 3. Visibility-Aware Rendering
```elisp
(when (get-buffer-window overview-buf)
  ;; Only render if visible
  (claude-gravity--render-overview))
```

**Performance optimization** - avoids rendering hidden buffers.

### 4. Dynamic Width Calculation
```elisp
(defun claude-gravity--get-fill-column ()
  (let ((win (get-buffer-window (current-buffer))))
    (if win
        (max 40 (- (window-width win) 2))
      80)))
```

**Used throughout** - adapts text wrapping to window size.

## Observations

1. **No custom display-action alists** - Only basic side-window parameters used (side, window-height, window-width)
2. **No split-window calls** - All window creation via display-buffer variants
3. **Minimal window destruction** - Only compose buffers explicitly delete windows
4. **Consistent side-window pattern** - Always followed by select-window
5. **No pop-up-buffer** - Not used anywhere
6. **No other-window** - Not used for buffer display (title asked about it)
7. **No interactive selection** - No completing-read for buffer/window choice (except resume picker content, not window selection)

## Window Type Usage Summary

| Mechanism | Count | Purpose |
|-----------|-------|---------|
| `switch-to-buffer` | 5 | Main entry points, full window replacement |
| `pop-to-buffer` | 3 | Session/plan views, respects window rules |
| `display-buffer-in-side-window` | 7 | Interactive action buffers (bottom/right) |
| `display-buffer` | 2 | Helper buffers (diff, summary) |
| `select-window` | 6 | Focus side windows after creation |
| `delete-window` | 2 | Cleanup compose windows |
| `get-buffer-window` | 14 | Visibility checks, window lookup |
| `with-selected-window` | 2 | Scrolling in correct window |
| `window-width` | 10 | Dynamic layout calculation |

## Not Used

- `other-window` - 0 occurrences
- `pop-up-buffer` - 0 occurrences
- `split-window` - 0 occurrences
- Interactive window/buffer selection (completing-read, etc.) - 0 occurrences
- Custom display-action alists beyond side-window parameters - 0 occurrences
