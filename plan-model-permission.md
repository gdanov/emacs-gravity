# Plan: Model & Permission Mode Switching (Bridge-Aware)

## Goal

Two unified commands — `set-model` and `set-permission-mode` — that work for any managed session type. Cloud (tmux) sessions are the first implementation. The design is bridge-aware: each backend implements its own `send-command` primitive, and the unified commands dispatch through it.

## Design

### Layer 1: Backend-specific send-command

Each backend already has a "send command" primitive:
- **Tmux**: `claude-gravity--tmux-send-keys` (sends keystrokes)
- **Daemon**: `claude-gravity--daemon-send-command` (sends IPC JSON)

For model/permission, the tmux backend uses Claude Code slash commands and keystrokes:
- Model: `/model <name>` via send-keys
- Permission: Shift-Tab cycling (no direct command exists)

### Layer 2: Backend-specific implementations

**In `claude-gravity-tmux.el`:**

```elisp
(defun claude-gravity-tmux-set-model (model &optional session-id)
  "Set MODEL for tmux session via /model slash command."
  (let* ((resolved (claude-gravity--resolve-tmux-session session-id))
         (sid (car resolved))
         (tmux-name (cdr resolved))
         (session (claude-gravity--get-session sid)))
    (claude-gravity--tmux-send-keys tmux-name (format "/model %s" model))
    ;; Optimistic update — will be corrected by next SessionStart if wrong
    (when session (plist-put session :model-name model))
    (message "Model → %s" model)))
```

```elisp
(defun claude-gravity-tmux-set-permission-mode (mode &optional session-id)
  "Set permission MODE for tmux session by cycling Shift-Tab.
Calculates presses needed based on tracked current mode.
Cycle order: default → auto-edit → plan → default."
  (let* ((resolved (claude-gravity--resolve-tmux-session session-id))
         (sid (car resolved))
         (tmux-name (cdr resolved))
         (session (claude-gravity--get-session sid))
         (current (or (plist-get session :permission-mode) "default"))
         (cycle '("default" "auto-edit" "plan"))
         (cur-idx (or (seq-position cycle current #'string=) 0))
         (tgt-idx (seq-position cycle mode #'string=))
         (presses (mod (- tgt-idx cur-idx) 3)))
    (dotimes (_ presses)
      (call-process "tmux" nil nil nil "send-keys" "-t" tmux-name "BTab")
      (sleep-for 0.3))
    (when session
      (claude-gravity-model-set-permission-mode session mode))
    (message "Permission mode → %s" mode)))
```

**Daemon** already has `claude-gravity-daemon-set-model` and `claude-gravity-daemon-set-permission-mode` — no changes needed.

### Layer 3: Unified dispatch (in `claude-gravity-daemon.el`, alongside other unified-* commands)

```elisp
(defun claude-gravity-set-model (model)
  "Set MODEL for the current managed session."
  (interactive (list (completing-read "Model: " '("sonnet" "opus" "haiku") nil t)))
  (cond
   ((claude-gravity--current-session-daemon-p) (claude-gravity-daemon-set-model model))
   ((claude-gravity--current-session-tmux-p)   (claude-gravity-tmux-set-model model))
   (t (user-error "No managed session at point"))))

(defun claude-gravity-set-permission-mode (mode)
  "Set permission MODE for the current managed session."
  (interactive (list (completing-read "Permission mode: " '("default" "auto-edit" "plan") nil t)))
  (cond
   ((claude-gravity--current-session-daemon-p) (claude-gravity-daemon-set-permission-mode mode))
   ((claude-gravity--current-session-tmux-p)   (claude-gravity-tmux-set-permission-mode mode))
   (t (user-error "No managed session at point"))))
```

### Layer 4: Transient menu update (in `claude-gravity-ui.el`)

```
["Sessions"
  ...
  ("m" "Set model" claude-gravity-set-model
   :inapt-if-not claude-gravity--current-session-managed-p)
  ("p" "Set permission mode" claude-gravity-set-permission-mode
   :inapt-if-not claude-gravity--current-session-managed-p)]
```

Change `m` from daemon-only → managed (unified). Add `p` for permission mode.

## Files to Change

| File | Changes |
|------|---------|
| `claude-gravity-tmux.el` | Add `tmux-set-model`. Rewrite `toggle-permission-mode` → `tmux-set-permission-mode`. |
| `claude-gravity-daemon.el` | Add `claude-gravity-set-model` and `claude-gravity-set-permission-mode` unified dispatchers (alongside existing unified-* pattern). |
| `claude-gravity-ui.el` | Menu: `m` → unified `set-model` with `:inapt-if-not managed-p`. Add `p` → unified `set-permission-mode`. |

## Extensibility

Adding a new backend later requires:
1. Implement `<backend>-set-model` and `<backend>-set-permission-mode`
2. Add a `<backend>-p` predicate
3. Add a clause to each unified dispatcher

The unified commands don't know about backends — they just dispatch on predicates.

## Risks

- **Permission cycle order** (`default → auto-edit → plan`) must match Claude Code. If CC changes it, cycling breaks.
- **Shift-Tab timing**: 0.3s between presses. May need tuning.
- **Model `/model` latency**: Optimistic update; corrected by next hook event if wrong.
- **Unknown initial permission mode**: Sessions started outside gravity default to `"default"`.
