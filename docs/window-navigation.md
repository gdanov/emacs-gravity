# Window Navigation Spec

## Buffer Classes

- **Gravity buffers**: overview (`*Structured Claude Sessions*`), session detail (`*Claude: <label>*`) — detected via `(derived-mode-p 'claude-gravity-mode)`
- **Action buffers**: permission, question — already use `display-buffer-in-side-window`, no changes
- **Popup buffers**: detail view (`*Claude Detail*`) — full-screen with wconf save/restore, no changes
- **Plan buffers**: plan review, plan display — gravity-like navigation
- **Terminal buffers**: tmux attach — user-initiated, stays as-is
- **File buffers**: transcripts, plan files via `find-file` — follow Emacs defaults

## Navigation Rules

| Origin | Action | Window Behavior |
|--------|--------|----------------|
| Source window | `M-x claude-gravity-status` | other window (preserve source) |
| Source window | `b` (quick switch) | gravity window if visible, else other window |
| Overview | RET on session | same window (replace overview) |
| Overview | any gravity navigation | same window |
| Session buffer | `o` (go to overview) | same window (already correct, no change) |
| Session buffer | `P` (show plan) | same window |
| Session buffer | `b` (quick switch) | same window (replace current session) |
| Any | `C-u b` | other window (explicit) |
| Any | `C-u C-u b` | new frame |
| Async | plan review arrives | gravity window if visible, else other window |

## Key Principle

**"Gravity stays in gravity."** Once the user has a gravity window, all gravity navigation targets that window. Non-gravity windows are never stolen.

## Implementation

### Helper: `claude-gravity--gravity-window`

Find a visible window showing a gravity buffer. Prefers selected window, then other windows in frame.

### Helper: `claude-gravity--display-buffer`

Central router with context parameter:
- `nil`/`'default`: if in gravity window → same window; elif gravity window visible → select + switch; else → other window
- `'same-window`: always same window
- `'other-window`: always other window
- `'new-frame`: new frame

### Call Sites

| Call site | Current | New |
|-----------|---------|-----|
| `claude-gravity-open-session` | `pop-to-buffer` | `--display-buffer buf context` |
| `claude-gravity-setup-buffer` | `switch-to-buffer` | `--display-buffer` |
| `claude-gravity-switch-session` | calls open-session | map prefix-arg → context |
| `claude-gravity--handle-plan-review` | `switch-to-buffer` | `--display-buffer` |
| `claude-gravity--show-plan-buffer` | `pop-to-buffer` | `--display-buffer` |

### No changes needed

- `claude-gravity-return-to-overview` — already correct
- Action buffers — already use side windows
- Popup detail — deliberate full-screen with wconf restore
- Tmux terminal — user-initiated terminal focus
- `find-file` calls — follow Emacs defaults
