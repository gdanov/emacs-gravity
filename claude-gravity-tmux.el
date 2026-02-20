;;; claude-gravity-tmux.el --- Tmux session management for Claude Gravity  -*- lexical-binding: t; -*-

;;; Code:

(require 'claude-gravity-core)
(require 'claude-gravity-session)
(require 'claude-gravity-state)
(require 'claude-gravity-socket)

(defvar claude-gravity-mode-map)
(declare-function claude-gravity--session-header-line "claude-gravity-ui")
(declare-function claude-gravity--infer-cwd-from-section "claude-gravity-ui")


;;; ============================================================================
;;; Tmux-based Interactive Claude Sessions
;;; ============================================================================
;;; All data (tools, text, agents, tasks, permissions, questions) comes from
;;; hooks via the bridge.  Prompts are sent via `tmux send-keys`.  Interactive
;;; Claude (no -p flag) runs inside a tmux session with TERM=dumb.

(defun claude-gravity--read-project-dir (prompt &optional initial-dir)
  "Read a project directory, with helm-compatible behavior.
When helm-mode is active, uses `helm-read-file-name' so that the user
can navigate into the target directory and press RET to confirm.
Falls back to `read-directory-name' otherwise."
  (let ((dir (if (and (bound-and-true-p helm-mode)
                      (fboundp 'helm-read-file-name))
                 (helm-read-file-name
                  (concat prompt "(navigate into dir, then RET) ")
                  :initial-input (or initial-dir default-directory)
                  :test #'file-directory-p
                  :must-match t)
               (read-directory-name prompt (or initial-dir default-directory)))))
    (file-name-as-directory (expand-file-name dir))))

(defvar claude-gravity--tmux-sessions (make-hash-table :test 'equal)
  "Map of session-id → tmux session name for Emacs-managed Claude sessions.")


(defvar claude-gravity--tmux-pending (make-hash-table :test 'equal)
  "Map of temp-id → tmux-name for tmux sessions awaiting SessionStart re-keying.")


(defvar claude-gravity--tmux-heartbeat-timer nil
  "Timer for periodic tmux session liveness checks.")


(defvar claude-gravity--tmux-resize-timers (make-hash-table :test 'equal)
  "Map of session-id → pending resize timer, for debouncing.")


(defun claude-gravity--claude-command ()
  "Return the claude command path."
  (or (executable-find "claude")
      (error "Claude CLI not found in PATH")))


(defun claude-gravity--tmux-check ()
  "Check that tmux is available and a server is running; signal error if not."
  (unless (executable-find "tmux")
    (error "tmux is required for managed sessions but not found in PATH"))
  (unless (= 0 (call-process "tmux" nil nil nil "list-sessions"))
    (user-error "No tmux sessions found.  Start a tmux session from an interactive terminal first to avoid macOS security prompts")))


(defun claude-gravity--tmux-alive-p (tmux-name)
  "Return non-nil if tmux session TMUX-NAME is alive."
  (= 0 (call-process "tmux" nil nil nil "has-session" "-t" tmux-name)))


(defun claude-gravity--attach-tmux-terminal (buf-name tmux-name)
  "Create a terminal buffer BUF-NAME attached to tmux session TMUX-NAME.
Uses the backend from `claude-gravity-terminal-backend' (vterm, eat, or term).
Returns the buffer.  Does NOT display the buffer — caller should switch/pop."
  (pcase claude-gravity-terminal-backend
    ('vterm
     (unless (fboundp 'vterm-mode)
       (error "vterm not available; install emacs-libvterm or set `claude-gravity-terminal-backend' to `eat' or `term'"))
     ;; vterm-mode reads vterm-shell to start the process.
     ;; We let-bind it and create the buffer + mode manually
     ;; to avoid vterm's own pop-to-buffer logic.
     (let* ((vterm-shell (format "tmux attach-session -t %s"
                                 (shell-quote-argument tmux-name)))
            (buf (get-buffer-create buf-name)))
       (with-current-buffer buf
         (unless (derived-mode-p 'vterm-mode)
           (vterm-mode)))
       buf))
    ('eat
     (unless (fboundp 'eat-make)
       (error "eat not available; install eat package or set `claude-gravity-terminal-backend' to `vterm' or `term'"))
     (let ((buf (eat-make buf-name "tmux" nil "attach-session" "-t" tmux-name)))
       (with-current-buffer buf
         (when (fboundp 'eat-char-mode)
           (eat-char-mode)))
       buf))
    (_
     (let ((buf (make-term (if (string-prefix-p "*" buf-name)
                               (substring buf-name 1 -1)
                             buf-name)
                           "tmux" nil "attach-session" "-t" tmux-name)))
       (with-current-buffer buf
         (term-char-mode))
       buf))))


(defun claude-gravity--tmux-send-keys (tmux-name text)
  "Send TEXT to tmux session TMUX-NAME, then press Enter.
For multi-line text, uses tmux load-buffer/paste-buffer to avoid
send-keys -l interpreting newlines as Enter."
  (if (string-match-p "\n" text)
      ;; Multi-line: pipe through load-buffer then paste
      (let ((tmpfile (make-temp-file "claude-tmux-")))
        (unwind-protect
            (progn
              (with-temp-file tmpfile (insert text))
              (call-process "tmux" nil nil nil "load-buffer" tmpfile)
              (call-process "tmux" nil nil nil "paste-buffer" "-t" tmux-name)
              (call-process "tmux" nil nil nil "send-keys" "-t" tmux-name "Enter"))
          (delete-file tmpfile)))
    ;; Single line: send-keys -l for literal text
    (call-process "tmux" nil nil nil "send-keys" "-t" tmux-name "-l" text)
    (call-process "tmux" nil nil nil "send-keys" "-t" tmux-name "Enter")))


(defun claude-gravity--current-session-tmux-p ()
  "Return non-nil if the current session has a tmux session."
  (let ((sid (or claude-gravity--buffer-session-id
                 (let ((section (magit-current-section)))
                   (when (and section (eq (oref section type) 'session-entry))
                     (oref section value))))))
    (and sid (gethash sid claude-gravity--tmux-sessions))))


(defun claude-gravity--tmux-handle-trust-prompt (tmux-name &optional attempt)
  "Detect and auto-accept the workspace trust prompt in TMUX-NAME.
Polls the tmux pane content up to 5 times at 1.5-second intervals.
If Claude's \"Do you trust the files in this folder?\" prompt is
detected, sends `y' to accept it.  This is safe because the user
explicitly chose the directory when starting the session."
  (let ((attempt (or attempt 0)))
    (when (and (< attempt 5) (claude-gravity--tmux-alive-p tmux-name))
      (condition-case nil
          (let ((content (with-temp-buffer
                           (call-process "tmux" nil t nil
                                         "capture-pane" "-t" tmux-name "-p")
                           (buffer-string))))
            (cond
             ((string-match-p "trust the files\\|Do you trust" content)
              (claude-gravity--log 'info
                "Trust prompt detected in %s, auto-accepting" tmux-name)
              (call-process "tmux" nil nil nil
                            "send-keys" "-t" tmux-name "y"))
             ;; Still loading — retry
             ((< attempt 4)
              (run-at-time 1.5 nil #'claude-gravity--tmux-handle-trust-prompt
                           tmux-name (1+ attempt)))))
        (error nil)))))


(defun claude-gravity--statusline-parts (plugin-root)
  "Return statusline override parts for managed sessions.
PLUGIN-ROOT is the directory containing claude-gravity.el.
Returns (ENV-VARS . CLI-ARGS) where ENV-VARS are strings like
\"KEY=val\" for the env command and CLI-ARGS are strings like
\"--settings\" \"{...}\" for the claude command.  Returns nil when
`claude-gravity-managed-statusline' is nil."
  (when claude-gravity-managed-statusline
    (let* ((script-path
            (cond
             ((stringp claude-gravity-managed-statusline)
              claude-gravity-managed-statusline)
             (t (expand-file-name "emacs-bridge/statusline.js" plugin-root))))
           (mode (if (eq claude-gravity-managed-statusline 'suppress)
                     "suppress" "minimal"))
           (statusline-cmd (if (stringp claude-gravity-managed-statusline)
                               script-path
                             (format "node %s" script-path)))
           (settings-json (json-encode
                           `((statusLine . ((type . "command")
                                           (command . ,statusline-cmd)))))))
      (cons
       ;; env vars
       (list (format "CLAUDE_GRAVITY_SOCK=%s" claude-gravity-server-sock-path)
             (format "CLAUDE_GRAVITY_STATUSLINE_MODE=%s" mode))
       ;; cli args
       (list "--settings" settings-json)))))


(defun claude-gravity--plugin-dirs (plugin-root)
  "Return list of --plugin-dir args for all plugins in PLUGIN-ROOT.
Reads marketplace.json and expands each plugin source path."
  (let ((manifest (expand-file-name "marketplace.json" plugin-root)))
    (if (file-exists-p manifest)
        (let* ((data (claude-gravity--json-read-file manifest))
               (plugins (alist-get 'plugins data)))
          (cl-mapcan (lambda (p)
                       (let ((src (alist-get 'source p)))
                         (when src
                           (let ((dir (expand-file-name src plugin-root)))
                             (when (file-directory-p dir)
                               (list "--plugin-dir" dir))))))
                     plugins))
      ;; Fallback: just emacs-bridge
      (list "--plugin-dir" (expand-file-name "emacs-bridge" plugin-root)))))


(defun claude-gravity-start-session (cwd &optional model permission-mode
                                         max-budget max-turns)
  "Start a new Claude session in CWD via tmux.
Optional MODEL overrides the default.  PERMISSION-MODE sets the mode.
MAX-BUDGET is a string for --max-budget-usd.  MAX-TURNS is a string
for --max-turns.
Returns the temp session-id (re-keyed when SessionStart hook arrives)."
  (interactive
   (list (claude-gravity--read-project-dir
          "Project directory: "
          (or (claude-gravity--infer-cwd-from-section)
              claude-gravity--last-project-dir
              default-directory))))
  (claude-gravity--tmux-check)
  (claude-gravity--ensure-server)
  (setq cwd (claude-gravity--normalize-cwd cwd))
  ;; Warn if another alive tmux session already runs in the same cwd
  (let ((existing-in-cwd nil))
    (maphash (lambda (sid tmux-name)
               (when (and (claude-gravity--tmux-alive-p tmux-name)
                          (let ((s (claude-gravity--get-session sid)))
                            (and s (equal (plist-get s :project) cwd))))
                 (setq existing-in-cwd tmux-name)))
             claude-gravity--tmux-sessions)
    (when existing-in-cwd
      (unless (y-or-n-p (format "Session already running in %s. Start another? " cwd))
        (user-error "Aborted"))))
  (let* ((temp-id (format "tmux-%s" (format-time-string "%s%3N")))
         (project-base (file-name-nondirectory (directory-file-name cwd)))
         (tmux-name (format "claude-%s-%s"
                            project-base
                            (substring (md5 (format "%s%s" (float-time) temp-id)) 0 4)))
         (plugin-root (file-name-directory
                       (or load-file-name
                           (locate-library "claude-gravity")
                           (error "Cannot locate claude-gravity.el for --plugin-dir"))))
         (sl-parts (claude-gravity--statusline-parts plugin-root))
         (cmd-parts `("env"
                      ,(format "CLAUDE_GRAVITY_TEMP_ID=%s" temp-id)
                      ,@(car sl-parts)
                      "claude" ,@(claude-gravity--plugin-dirs plugin-root))))
    (when model
      (setq cmd-parts (append cmd-parts (list "--model" model))))
    (when permission-mode
      (setq cmd-parts (append cmd-parts (list "--permission-mode" permission-mode))))
    (when max-budget
      (setq cmd-parts (append cmd-parts (list "--max-budget-usd" max-budget))))
    (when max-turns
      (setq cmd-parts (append cmd-parts (list "--max-turns" max-turns))))
    (when (cdr sl-parts)
      (setq cmd-parts (append cmd-parts (cdr sl-parts))))
    (let ((result (apply #'call-process "tmux" nil nil nil
                         "new-session" "-d" "-s" tmux-name
                         "-c" cwd
                         cmd-parts)))
      (unless (= result 0)
        (error "Failed to create tmux session %s" tmux-name))
      ;; Set initial tmux window width
      (let ((cols (claude-gravity--tmux-initial-width)))
        (call-process "tmux" nil nil nil
                      "resize-window" "-t" tmux-name
                      "-x" (number-to-string cols)))
      ;; Register pending re-key by temp-id (allows multiple sessions per cwd)
      (puthash temp-id tmux-name claude-gravity--tmux-pending)
      ;; Create session with temp ID
      (let ((session (claude-gravity--ensure-session temp-id cwd)))
        (plist-put session :tmux-session tmux-name)
        (plist-put session :temp-id temp-id)
        (plist-put session :tmux-width (claude-gravity--tmux-initial-width))
        (claude-gravity-model-set-claude-status session 'idle))
      (claude-gravity--tmux-ensure-heartbeat)
      (run-at-time 2 nil #'claude-gravity--tmux-handle-trust-prompt tmux-name)
      (claude-gravity--record-last-project cwd)
      (claude-gravity--schedule-refresh)
      (claude-gravity--log 'debug "Claude tmux session starting in %s" cwd)
      temp-id)))


(defun claude-gravity-resume-session (session-id &optional cwd model)
  "Resume an existing Claude session by SESSION-ID via tmux.
CWD defaults to the session's stored cwd.  MODEL overrides the default."
  (interactive
   (let* ((candidates nil))
     (maphash (lambda (id session)
                (when (eq (plist-get session :status) 'ended)
                  (push (cons (format "%s [%s]"
                                      (or (plist-get session :slug) id)
                                      (plist-get session :project))
                              id)
                        candidates)))
              claude-gravity--sessions)
     (if candidates
         (let* ((choice (completing-read "Resume session: " candidates nil t))
                (sid (cdr (assoc choice candidates))))
           (list sid))
       (list (read-string "Session ID to resume: ")))))
  (claude-gravity--tmux-check)
  (claude-gravity--ensure-server)
  (let* ((existing (claude-gravity--get-session session-id))
         (cwd (claude-gravity--normalize-cwd
               (or cwd
                   (and existing (plist-get existing :cwd))
                   (claude-gravity--read-project-dir "Project directory: "))))
         (temp-id (format "tmux-%s" (format-time-string "%s%3N")))
         (project-base (file-name-nondirectory
                        (directory-file-name
                         (or cwd
                             (and existing (plist-get existing :cwd))
                             default-directory))))
         (tmux-name (format "claude-resume-%s-%s"
                            project-base
                            (substring session-id 0 (min 8 (length session-id)))))
         (plugin-root (file-name-directory
                       (or load-file-name
                           (locate-library "claude-gravity")
                           (error "Cannot locate claude-gravity.el for --plugin-dir"))))
         (sl-parts (claude-gravity--statusline-parts plugin-root))
         (cmd-parts `("env"
                      ,(format "CLAUDE_GRAVITY_TEMP_ID=%s" temp-id)
                      ,@(car sl-parts)
                      "claude" "--resume" ,session-id
                      ,@(claude-gravity--plugin-dirs plugin-root))))
    (when model
      (setq cmd-parts (append cmd-parts (list "--model" model))))
    (when (cdr sl-parts)
      (setq cmd-parts (append cmd-parts (cdr sl-parts))))
    ;; Register pending re-key by temp-id (allows multiple sessions per cwd)
    (puthash temp-id tmux-name claude-gravity--tmux-pending)
    (let ((result (apply #'call-process "tmux" nil nil nil
                         "new-session" "-d" "-s" tmux-name
                         "-c" cwd
                         cmd-parts)))
      (unless (= result 0)
        (remhash temp-id claude-gravity--tmux-pending)
        (error "Failed to create tmux session %s" tmux-name))
      ;; Set initial tmux window width
      (let ((cols (claude-gravity--tmux-initial-width)))
        (call-process "tmux" nil nil nil
                      "resize-window" "-t" tmux-name
                      "-x" (number-to-string cols)))
      (puthash session-id tmux-name claude-gravity--tmux-sessions)
      ;; Update or create session
      (let ((session (claude-gravity--ensure-session session-id cwd)))
        (when (eq (plist-get session :status) 'ended)
          (plist-put session :status 'active))
        (plist-put session :tmux-session tmux-name)
        (plist-put session :temp-id temp-id)
        (plist-put session :tmux-width (claude-gravity--tmux-initial-width))
        (claude-gravity-model-set-claude-status session 'idle))
      (claude-gravity--tmux-ensure-heartbeat)
      (run-at-time 2 nil #'claude-gravity--tmux-handle-trust-prompt tmux-name)
      (claude-gravity--schedule-refresh)
      (claude-gravity--log 'debug "Claude tmux session resuming %s" session-id)
      session-id)))


(defun claude-gravity--resolve-tmux-session (&optional session-id)
  "Resolve SESSION-ID to a (sid . tmux-name) cons.
Falls back to buffer-local session, section at point, then any active tmux session.
Signals error if no session found or not alive."
  (let* ((sid (or session-id
                  claude-gravity--buffer-session-id
                  ;; In overview buffer: use session at point
                  (let ((section (magit-current-section)))
                    (when (and section (eq (oref section type) 'session-entry))
                      (oref section value)))
                  (let ((alive nil))
                    (maphash (lambda (id tmux-name)
                               (when (claude-gravity--tmux-alive-p tmux-name)
                                 (push (cons id tmux-name) alive)))
                             claude-gravity--tmux-sessions)
                    (cond
                     ((null alive) nil)
                     ((= (length alive) 1) (caar alive))
                     (t ;; Multiple alive sessions — let user pick
                      (let* ((candidates
                              (mapcar (lambda (pair)
                                        (let* ((id (car pair))
                                               (tn (cdr pair))
                                               (s (claude-gravity--get-session id))
                                               (label (if s
                                                          (format "%s [%s]"
                                                                  (or (plist-get s :slug) id)
                                                                  tn)
                                                        (format "%s [%s]" id tn))))
                                          (cons label id)))
                                      alive))
                             (choice (completing-read "Select session: " candidates nil t)))
                        (cdr (assoc choice candidates))))))))
         (tmux-name (and sid (gethash sid claude-gravity--tmux-sessions))))
    (unless tmux-name
      (error "No tmux Claude session found for %s" (or sid "any")))
    (unless (claude-gravity--tmux-alive-p tmux-name)
      (error "Tmux session %s is not running" tmux-name))
    (cons sid tmux-name)))


(defun claude-gravity--send-prompt-core (prompt sid tmux-name)
  "Send PROMPT text to tmux TMUX-NAME and update model for session SID."
  (claude-gravity--tmux-send-keys tmux-name prompt)
  (let ((session (claude-gravity--get-session sid)))
    (when session
      (plist-put session :tmux-prompt-sent t)
      (claude-gravity-model-add-prompt
       session (list (cons 'text prompt)
                     (cons 'submitted (current-time))
                     (cons 'elapsed nil)
                     (cons 'stop_text nil)
                     (cons 'stop_thinking nil)))
      (claude-gravity-model-set-claude-status session 'responding)
      (claude-gravity--schedule-session-refresh sid)
      ;; Auto-enable follow mode so the buffer tails the response
      (let ((buf (or (let ((b (plist-get session :buffer)))
                       (and b (buffer-live-p b) b))
                     (get-buffer (claude-gravity--session-buffer-name session)))))
        (when (and buf (not (buffer-local-value 'claude-gravity--follow-mode buf)))
          (with-current-buffer buf
            (setq claude-gravity--follow-mode t)
            (add-hook 'post-command-hook #'claude-gravity--follow-detect-manual nil t)
            (force-mode-line-update))))))
  (claude-gravity--log 'debug "Sent prompt to Claude [%s]" sid))


(defun claude-gravity-send-prompt (prompt &optional session-id)
  "Send PROMPT to the tmux Claude session for SESSION-ID.
If SESSION-ID is nil, uses the current buffer's session."
  (interactive
   (list (read-string "Prompt: ")))
  (let ((resolved (claude-gravity--resolve-tmux-session session-id)))
    (claude-gravity--send-prompt-core prompt (car resolved) (cdr resolved))))


;;; ── Compose buffer (chat-style prompt entry) ──────────────────────

(defvar-local claude-gravity--compose-session-id nil
  "Session ID targeted by this compose buffer.")


(defvar-local claude-gravity--compose-separator nil
  "Marker for the start of the editable compose area.")


(defvar claude-gravity-compose-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'claude-gravity-compose-send)
    (define-key map (kbd "C-c C-k") #'claude-gravity-compose-cancel)
    (define-key map (kbd "C-g") #'claude-gravity-compose-cancel)
    map)
  "Keymap for `claude-gravity-compose-mode'.")


(define-minor-mode claude-gravity-compose-mode
  "Minor mode for the Claude prompt compose buffer.
\\{claude-gravity-compose-mode-map}"
  :lighter " Compose"
  :keymap claude-gravity-compose-mode-map)


(defun claude-gravity--compose-insert-history (session)
  "Insert conversation history from SESSION's turn tree."
  (let* ((turns-tl (plist-get session :turns))
         (turn-nodes (when turns-tl (claude-gravity--tlist-items turns-tl))))
    (when turn-nodes
      (dolist (turn-node turn-nodes)
        (let* ((entry (alist-get 'prompt turn-node))
               (ptype (and (listp entry) (alist-get 'type entry)))
               (text (and (listp entry) (alist-get 'text entry)))
               (stop-text (and (listp entry) (alist-get 'stop_text entry))))
          ;; Skip question/phase-boundary entries and turn 0
          (unless (or (memq ptype '(question phase-boundary))
                      (= (alist-get 'turn-number turn-node) 0))
            (when text
              (insert (propertize (concat "❯ " text "\n")
                                  'face 'claude-gravity-prompt)))
            (when stop-text
              (insert (propertize (concat "\n◀ " stop-text "\n\n")
                                  'face 'claude-gravity-assistant-text)))))))))


(defun claude-gravity--compose-guard-history (beg _end)
  "Prevent edits before the compose separator marker."
  (when (and claude-gravity--compose-separator
             (< beg (marker-position claude-gravity--compose-separator)))
    (user-error "History is read-only — type below the separator")))


(defun claude-gravity-compose-prompt (&optional session-id)
  "Open a chat-style compose buffer to send a prompt.
SESSION-ID defaults to the current buffer's session or any active tmux session."
  (interactive)
  (let* ((resolved (claude-gravity--resolve-tmux-session session-id))
         (sid (car resolved))
         (session (claude-gravity--get-session sid))
         (label (if session (claude-gravity--session-label session) (substring sid 0 8)))
         (buf-name (format "*Claude Compose: %s*" label))
         (buf (get-buffer-create buf-name)))
    (with-current-buffer buf
      ;; Major mode first (kills buffer-locals), then content + minor modes
      (if (fboundp 'markdown-mode) (markdown-mode) (text-mode))
      ;; Remove stale guard from previous buffer incarnation, then
      ;; rebuild content with inhibit-read-only to bypass old text props
      (remove-hook 'before-change-functions
                   #'claude-gravity--compose-guard-history t)
      (let ((inhibit-read-only t))
        (erase-buffer)
        ;; Insert conversation history
        (when session
          (claude-gravity--compose-insert-history session))
        ;; Separator
        (insert (propertize (concat (make-string 50 ?─) "\n")
                            'face 'claude-gravity-divider))
        ;; Mark where the editable compose area starts
        (setq-local claude-gravity--compose-separator (copy-marker (point)))
        ;; Lock history + separator as read-only, except the very last
        ;; char so the property doesn't block typing at the boundary.
        (when (> (point) 2)
          (put-text-property 1 (1- (point)) 'read-only t)
          (put-text-property 1 (1- (point)) 'front-sticky '(read-only))))
      ;; Install guard for the boundary char + belt-and-suspenders
      (add-hook 'before-change-functions
                #'claude-gravity--compose-guard-history nil t)
      ;; Buffer-local state (after major mode so they survive)
      (setq-local claude-gravity--compose-session-id sid)
      ;; Minor modes
      (claude-gravity-compose-mode 1)
      (when (fboundp 'olivetti-mode) (olivetti-mode 1))
      ;; C-g support: post-command-hook detects keyboard-quit
      (add-hook 'post-command-hook
                #'claude-gravity--compose-quit-hook nil t)
      ;; Place point at end (compose area)
      (goto-char (point-max)))
    ;; Display in side window
    (display-buffer-in-side-window buf '((side . bottom) (window-height . 0.40)))
    (select-window (get-buffer-window buf))
    (goto-char (point-max))))


(declare-function claude-gravity-daemon-compose-send "claude-gravity-daemon")
(declare-function claude-gravity--current-session-daemon-p "claude-gravity-daemon")
(declare-function claude-gravity-daemon-set-permission-mode "claude-gravity-daemon")

(defun claude-gravity-compose-send ()
  "Send the composed prompt and close the compose buffer."
  (interactive)
  (if (eq claude-gravity--compose-backend 'daemon)
      (claude-gravity-daemon-compose-send)
    (let* ((sep claude-gravity--compose-separator)
           (text (string-trim
                  (buffer-substring-no-properties
                   (if sep (marker-position sep) (point-min))
                   (point-max)))))
      (if (string-empty-p text)
          (message "Nothing to send")
        (let* ((sid claude-gravity--compose-session-id)
               (resolved (claude-gravity--resolve-tmux-session sid)))
          (claude-gravity--send-prompt-core text (car resolved) (cdr resolved))
          (claude-gravity--compose-cleanup)
          (message "Prompt sent"))))))


(defun claude-gravity-compose-cancel ()
  "Cancel composing and close the compose buffer."
  (interactive)
  (claude-gravity--compose-cleanup)
  (message "Compose cancelled"))


(defun claude-gravity--compose-quit-hook ()
  "Close compose buffer when C-g (keyboard-quit) is invoked."
  (when (eq this-command 'keyboard-quit)
    (run-at-time 0 nil #'claude-gravity--compose-cleanup-buffer
                 (current-buffer))))


(defun claude-gravity--compose-cleanup ()
  "Kill the compose buffer and its window."
  (claude-gravity--compose-cleanup-buffer (current-buffer)))


(defun claude-gravity--compose-cleanup-buffer (buf)
  "Kill compose BUF and its window."
  (when (buffer-live-p buf)
    (let ((win (get-buffer-window buf t)))
      (when win (delete-window win))
      (kill-buffer buf))))


(defun claude-gravity-slash-command (command &optional session-id)
  "Send slash COMMAND to the tmux Claude session and display output.
COMMAND should include the leading slash, e.g. \"/context\".
Captures the tmux pane content after a short delay and shows it
in a read-only buffer."
  (interactive
   (list (read-string "Slash command: " "/")))
  (let* ((sid (or session-id
                  claude-gravity--buffer-session-id
                  (let ((found nil))
                    (maphash (lambda (id tmux-name)
                               (when (and (not found)
                                          (claude-gravity--tmux-alive-p tmux-name))
                                 (setq found id)))
                             claude-gravity--tmux-sessions)
                    found)))
         (tmux-name (and sid (gethash sid claude-gravity--tmux-sessions))))
    (unless tmux-name
      (error "No tmux Claude session found for %s" (or sid "any")))
    (unless (claude-gravity--tmux-alive-p tmux-name)
      (error "Tmux session %s is not running" tmux-name))
    (claude-gravity--tmux-send-keys tmux-name command)
    (let ((cmd command))
      (run-at-time 1.0 nil
        (lambda ()
          (let* ((raw (shell-command-to-string
                        (format "tmux capture-pane -t %s -p -S -300"
                                (shell-quote-argument tmux-name))))
                 (pos (string-search cmd raw))
                 (output (if pos
                             (substring raw pos)
                           raw)))
            (with-current-buffer (get-buffer-create "*Claude Slash Output*")
              (let ((inhibit-read-only t))
                (erase-buffer)
                (insert output)
                (goto-char (point-min)))
              (special-mode)
              (display-buffer (current-buffer)))))))))


(defun claude-gravity-tmux-set-model (model &optional session-id)
  "Set MODEL for tmux session SESSION-ID via /model slash command.
MODEL is a short name like \"sonnet\", \"opus\", or \"haiku\"."
  (interactive
   (list (completing-read "Model: " '("sonnet" "opus" "haiku") nil t)))
  (let* ((resolved (claude-gravity--resolve-tmux-session session-id))
         (sid (car resolved))
         (tmux-name (cdr resolved))
         (session (claude-gravity--get-session sid)))
    (claude-gravity--tmux-send-keys tmux-name (format "/model %s" model))
    ;; Optimistic update — corrected by next hook event if wrong
    (when session
      (plist-put session :model-name model)
      (claude-gravity--schedule-refresh))
    (claude-gravity--log 'debug "Set model %s for tmux [%s]" model sid)
    (message "Model → %s" model)))

(defun claude-gravity-tmux-set-permission-mode (mode &optional session-id)
  "Set permission MODE for tmux session SESSION-ID by cycling Shift-Tab.
Calculates presses needed based on tracked current mode.
Cycle order: default → auto-edit → plan → default.
MODE is one of \"default\", \"auto-edit\", or \"plan\"."
  (interactive
   (list (completing-read "Permission mode: " '("default" "auto-edit" "plan") nil t)))
  (let* ((resolved (claude-gravity--resolve-tmux-session session-id))
         (sid (car resolved))
         (tmux-name (cdr resolved))
         (session (claude-gravity--get-session sid))
         (current (or (and session (plist-get session :permission-mode)) "default"))
         (cycle '("default" "auto-edit" "plan"))
         (cur-idx (or (seq-position cycle current #'string=) 0))
         (tgt-idx (seq-position cycle mode #'string=))
         (presses (mod (- tgt-idx cur-idx) 3)))
    (when (> presses 0)
      (dotimes (_ presses)
        (call-process "tmux" nil nil nil "send-keys" "-t" tmux-name "BTab")
        (sleep-for 0.3)))
    (when session
      (claude-gravity-model-set-permission-mode session mode)
      (claude-gravity--schedule-refresh))
    (claude-gravity--log 'debug "Set permission mode %s for tmux [%s] (%d presses)" mode sid presses)
    (message "Permission mode → %s" mode)))


(defun claude-gravity-send-escape ()
  "Send Escape to the managed tmux session (interrupt current operation)."
  (interactive)
  (let* ((sid (or claude-gravity--buffer-session-id
                  (let ((section (magit-current-section)))
                    (when (and section (eq (oref section type) 'session-entry))
                      (oref section value)))))
         (tmux-name (and sid (gethash sid claude-gravity--tmux-sessions))))
    (unless tmux-name
      (user-error "No tmux session at point"))
    (call-process "tmux" nil nil nil "send-keys" "-t" tmux-name "Escape")
    (claude-gravity--log 'debug "Sent Escape to %s" tmux-name)))


(defvar claude-gravity--tmux-stop-timeout 5
  "Seconds to wait for graceful /quit before force-killing tmux session.")

(defun claude-gravity--tmux-resolve-session-id (&optional session-id)
  "Resolve SESSION-ID from context: argument, buffer, section, or first alive."
  (or session-id
      claude-gravity--buffer-session-id
      (let ((section (magit-current-section)))
        (when (and section (eq (oref section type) 'session-entry))
          (oref section value)))
      (let ((found nil))
        (maphash (lambda (id tmux-name)
                   (when (and (not found)
                              (claude-gravity--tmux-alive-p tmux-name))
                     (setq found id)))
                 claude-gravity--tmux-sessions)
        found)))

(defun claude-gravity-stop-session (&optional session-id)
  "Gracefully stop the tmux Claude session for SESSION-ID.
Sends /quit to Claude Code first.  If the session doesn't exit
within `claude-gravity--tmux-stop-timeout' seconds, force-kills it."
  (interactive)
  (let* ((sid (claude-gravity--tmux-resolve-session-id session-id))
         (tmux-name (and sid (gethash sid claude-gravity--tmux-sessions))))
    (when tmux-name
      (if (not (claude-gravity--tmux-alive-p tmux-name))
          ;; Already dead — just clean up
          (claude-gravity--tmux-finalize-stop sid tmux-name)
        ;; Set stopping status
        (let ((session (claude-gravity--get-session sid)))
          (when session
            (plist-put session :claude-status 'stopping)))
        (claude-gravity--schedule-refresh)
        (claude-gravity--log 'info "Sending Escape + /quit to tmux session [%s]" sid)
        ;; Send Escape first to interrupt any active generation
        (call-process "tmux" nil nil nil "send-keys" "-t" tmux-name "Escape")
        ;; Delay before /quit so Escape is processed and input field is ready
        (run-at-time 2 nil #'claude-gravity--tmux-send-quit sid tmux-name)
        ;; Force-kill timeout starts after the /quit delay
        (run-at-time (+ 2 claude-gravity--tmux-stop-timeout) nil
                     #'claude-gravity--tmux-force-stop sid tmux-name)))))

(defun claude-gravity--tmux-send-quit (sid tmux-name)
  "Send /quit to tmux session TMUX-NAME for SID."
  (when (claude-gravity--tmux-alive-p tmux-name)
    (claude-gravity--log 'info "Sending /quit to tmux session [%s]" sid)
    (call-process "tmux" nil nil nil "send-keys" "-t" tmux-name "-l" "/quit")
    (call-process "tmux" nil nil nil "send-keys" "-t" tmux-name "Enter")))

(defun claude-gravity--tmux-force-stop (sid tmux-name)
  "Force-kill tmux session TMUX-NAME for SID if still alive."
  (when (claude-gravity--tmux-alive-p tmux-name)
    (claude-gravity--log 'info "Force-killing tmux session [%s] after timeout" sid)
    (call-process "tmux" nil nil nil "kill-session" "-t" tmux-name))
  (claude-gravity--tmux-finalize-stop sid tmux-name))

(defun claude-gravity--tmux-finalize-stop (sid tmux-name)
  "Clean up state after tmux session TMUX-NAME for SID has stopped."
  (ignore tmux-name)
  (remhash sid claude-gravity--tmux-sessions)
  (let ((session (claude-gravity--get-session sid)))
    (when session
      (claude-gravity-model-session-end session)))
  (claude-gravity--schedule-refresh)
  (claude-gravity--schedule-session-refresh sid)
  (claude-gravity--log 'debug "Finalized stop for tmux session [%s]" sid))


;;; Tmux window size sync — keep tmux width = Emacs window width

(defun claude-gravity--tmux-resize (session-id width)
  "Resize the tmux window for SESSION-ID to WIDTH columns.
No-op if WIDTH matches the cached `:tmux-width' or no tmux session exists."
  (when-let* ((tmux-name (gethash session-id claude-gravity--tmux-sessions))
              (session (claude-gravity--get-session session-id)))
    (unless (eql width (plist-get session :tmux-width))
      (when (= 0 (call-process "tmux" nil nil nil
                                "resize-window" "-t" tmux-name
                                "-x" (number-to-string width)))
        (plist-put session :tmux-width width)
        (claude-gravity--log 'debug "Resized tmux %s to %d columns" tmux-name width)))))


(defun claude-gravity--tmux-sync-width-for-buffer (buf)
  "Schedule a debounced tmux resize for the session displayed in BUF.
Uses the width of BUF's window.  Debounces at 0.3s per session."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (when-let* ((sid claude-gravity--buffer-session-id)
                  (win (and (gethash sid claude-gravity--tmux-sessions)
                            (get-buffer-window buf)))
                  (width (window-width win)))
        ;; Cancel existing timer for this session
        (let ((old-timer (gethash sid claude-gravity--tmux-resize-timers)))
          (when old-timer (cancel-timer old-timer)))
        (puthash sid
                 (run-at-time 0.3 nil
                              #'claude-gravity--tmux-resize sid width)
                 claude-gravity--tmux-resize-timers)))))


(defun claude-gravity--tmux-on-window-size-change (frame)
  "Sync tmux width for any session buffers visible on FRAME."
  (dolist (win (window-list frame 'no-minibuf))
    (let ((buf (window-buffer win)))
      (when (and (buffer-live-p buf)
                 (buffer-local-value 'claude-gravity--buffer-session-id buf))
        (claude-gravity--tmux-sync-width-for-buffer buf)))))


(add-hook 'window-size-change-functions #'claude-gravity--tmux-on-window-size-change)


(defun claude-gravity--tmux-initial-width ()
  "Return the initial width for a new tmux session.
Uses the current window's width if in a session buffer, else the default."
  (or (and claude-gravity--buffer-session-id
           (window-width))
      claude-gravity-tmux-default-columns))


;;; Tmux heartbeat — detect dead sessions

(defun claude-gravity--tmux-heartbeat ()
  "Check all tmux sessions for liveness asynchronously.
Uses a single `tmux list-sessions' call instead of N `has-session' calls."
  (when (> (hash-table-count claude-gravity--tmux-sessions) 0)
    (let ((buf (generate-new-buffer " *tmux-heartbeat*")))
      (set-process-sentinel
       (start-process "tmux-heartbeat" buf "tmux" "list-sessions"
                      "-F" "#{session_name}")
       (lambda (proc _event)
         (when (memq (process-status proc) '(exit signal))
           (let ((alive-names
                  (when (= (process-exit-status proc) 0)
                    (with-current-buffer (process-buffer proc)
                      (split-string (buffer-string) "\n" t))))
                 (dead nil))
             (maphash (lambda (sid tmux-name)
                        (unless (member tmux-name alive-names)
                          (push sid dead)))
                      claude-gravity--tmux-sessions)
             (dolist (sid dead)
               (remhash sid claude-gravity--tmux-sessions)
               (let ((session (claude-gravity--get-session sid)))
                 (when (and session (eq (plist-get session :status) 'active))
                   (claude-gravity-model-session-end session)
                   (claude-gravity--schedule-refresh)
                   (claude-gravity--schedule-session-refresh sid)))))
           (kill-buffer (process-buffer proc))))))))


(defun claude-gravity--tmux-ensure-heartbeat ()
  "Start the tmux heartbeat timer if not already running."
  (unless claude-gravity--tmux-heartbeat-timer
    (setq claude-gravity--tmux-heartbeat-timer
          (run-with-timer 5 5 #'claude-gravity--tmux-heartbeat))))


(defun claude-gravity--tmux-cleanup-all ()
  "Kill all tmux sessions managed by gravity.  For `kill-emacs-hook'."
  (when claude-gravity--tmux-heartbeat-timer
    (cancel-timer claude-gravity--tmux-heartbeat-timer)
    (setq claude-gravity--tmux-heartbeat-timer nil))
  ;; Cancel pending resize timers
  (maphash (lambda (_sid timer) (when timer (cancel-timer timer)))
           claude-gravity--tmux-resize-timers)
  (clrhash claude-gravity--tmux-resize-timers)
  (maphash (lambda (_sid tmux-name)
             (ignore-errors
               (call-process "tmux" nil nil nil "kill-session" "-t" tmux-name)))
           claude-gravity--tmux-sessions)
  (clrhash claude-gravity--tmux-sessions)
  (clrhash claude-gravity--tmux-pending))


(add-hook 'kill-emacs-hook #'claude-gravity--tmux-cleanup-all)


(defun claude-gravity--tmux-list-claude-sessions ()
  "Return list of live tmux session names matching the claude-* prefix.
Sources from `tmux list-sessions' directly for robustness."
  (condition-case nil
      (seq-filter (lambda (name) (string-prefix-p "claude-" name))
                  (process-lines "tmux" "list-sessions" "-F" "#{session_name}"))
    (error nil)))

(defun claude-gravity--tmux-session-display (tmux-name)
  "Build a display string for TMUX-NAME, enriched with gravity metadata if available.
Untracked sessions (not in gravity session list) get a ghost prefix."
  (let ((label tmux-name)
        (found nil))
    (maphash (lambda (sid name)
               (when (equal name tmux-name)
                 (let ((session (gethash sid claude-gravity--sessions)))
                   (when session
                     (setq found t)
                     (setq label (format "%s  %s  %s"
                                         tmux-name
                                         (or (plist-get session :project) "")
                                         (or (plist-get session :slug) "")))))))
             claude-gravity--tmux-sessions)
    (unless found
      (setq label (format "👻 %s" tmux-name)))
    label))

(defun claude-gravity-terminal-session ()
  "Open a terminal buffer attached to a tmux Claude session.
When point is on a session entry or inside a session buffer, attaches
to that session's tmux.  Otherwise offers a picker over all live
claude-* tmux sessions (sourced from `tmux list-sessions')."
  (interactive)
  (let* ((sid (or claude-gravity--buffer-session-id
                  (let ((section (magit-current-section)))
                    (when (and section (eq (oref section type) 'session-entry))
                      (oref section value)))))
         (tmux-name (and sid (gethash sid claude-gravity--tmux-sessions))))
    ;; Fallback: pick from live tmux sessions
    (unless tmux-name
      (let ((live (claude-gravity--tmux-list-claude-sessions)))
        (cond
         ((null live)
          (user-error "No running Claude tmux sessions"))
         ((= (length live) 1)
          (setq tmux-name (car live)))
         (t
          (let* ((candidates (mapcar (lambda (name)
                                       (cons (claude-gravity--tmux-session-display name) name))
                                     live))
                 (choice (completing-read "Attach to tmux session: " candidates nil t)))
            (setq tmux-name (cdr (assoc choice candidates))))))))
    (let* ((session (and sid (gethash sid claude-gravity--sessions)))
           (project (or (and session (plist-get session :project))
                        tmux-name))
           (buf-name (format "*Claude Terminal: %s*" project))
           (existing (get-buffer buf-name)))
      (if (and existing (buffer-live-p existing)
                (get-buffer-process existing)
                (process-live-p (get-buffer-process existing)))
          (switch-to-buffer existing)
        (when existing (kill-buffer existing))
        (switch-to-buffer
         (claude-gravity--attach-tmux-terminal buf-name tmux-name))))))


(defvar claude-gravity--resume-picker-buffer nil
  "Buffer showing the interactive `claude --resume` picker.
Buried when the user picks a session (second SessionStart).")

(defvar claude-gravity--resume-picker-tmux nil
  "Tmux session name for the active resume picker.")

(defvar claude-gravity--resume-picker-init-seen nil
  "Non-nil after the init SessionStart from the resume picker.")


(defun claude-gravity-resume-in-tmux (&optional cwd)
  "Resume a Claude session via the interactive session picker.
Starts `claude --resume` (no session ID) in a tmux session with all
managed-session params, then attaches via `term-mode' so the user
can use arrow keys to select a session.

The terminal buffer stays alive after session selection — it becomes
the terminal view of the running session (like `$').  The gravity
session buffer opens alongside it automatically via SessionStart."
  (interactive
   (list (claude-gravity--read-project-dir "Project directory: " default-directory)))
  (claude-gravity--tmux-check)
  (claude-gravity--ensure-server)
  (setq cwd (claude-gravity--normalize-cwd (or cwd default-directory)))
  (let* ((temp-id (format "tmux-%s" (format-time-string "%s%3N")))
         (tmux-name (format "claude-resume-%s" temp-id))
         (plugin-root (file-name-directory
                       (or load-file-name
                           (locate-library "claude-gravity")
                           (error "Cannot locate claude-gravity.el"))))
         (sl-parts (claude-gravity--statusline-parts plugin-root))
         (cmd-parts `("env"
                      ,(format "CLAUDE_GRAVITY_TEMP_ID=%s" temp-id)
                      ,@(car sl-parts)
                      "claude" "--resume"
                      ,@(claude-gravity--plugin-dirs plugin-root))))
    (when (cdr sl-parts)
      (setq cmd-parts (append cmd-parts (cdr sl-parts))))
    (let ((result (apply #'call-process "tmux" nil nil nil
                         "new-session" "-d" "-s" tmux-name
                         "-c" cwd
                         cmd-parts)))
      (unless (= result 0)
        (error "Failed to create tmux session %s" tmux-name))
      ;; Set initial tmux window width
      (let ((cols (claude-gravity--tmux-initial-width)))
        (call-process "tmux" nil nil nil
                      "resize-window" "-t" tmux-name
                      "-x" (number-to-string cols)))
      ;; Register pending re-key so SessionStart hooks can find this.
      ;; NOTE: `claude --resume` fires SessionStart immediately during init
      ;; (before picker).  That first SessionStart creates a session that
      ;; gets replaced when the user picks.  We still need the pending
      ;; entry so the re-key mechanism can track the tmux mapping.
      (puthash temp-id tmux-name claude-gravity--tmux-pending)
      (claude-gravity--tmux-ensure-heartbeat)
      (run-at-time 2 nil #'claude-gravity--tmux-handle-trust-prompt tmux-name)
      ;; Attach via terminal emulator so user can interact with the picker.
      ;; The buffer stays alive — it becomes the terminal view of
      ;; the resumed session after selection.
      (let* ((buf-name "*Claude Resume Picker*")
             (existing (get-buffer buf-name)))
        (when existing (kill-buffer existing))
        (let ((buf (claude-gravity--attach-tmux-terminal buf-name tmux-name)))
          (setq claude-gravity--resume-picker-buffer buf
                claude-gravity--resume-picker-tmux tmux-name
                claude-gravity--resume-picker-init-seen nil)
          (switch-to-buffer buf)))
      (claude-gravity--log 'debug "Resume picker started in tmux %s" tmux-name)
      temp-id)))


(defun claude-gravity--bury-resume-picker ()
  "Bury the resume picker terminal buffer if it exists.
Called from SessionStart handler after the init SessionStart that fires
before the picker.  Only buries — does not kill, since the buffer
becomes the terminal view of the running session."
  (when (and claude-gravity--resume-picker-buffer
             (buffer-live-p claude-gravity--resume-picker-buffer))
    (let ((buf claude-gravity--resume-picker-buffer))
      (setq claude-gravity--resume-picker-buffer nil
            claude-gravity--resume-picker-tmux nil
            claude-gravity--resume-picker-init-seen nil)
      (let ((win (get-buffer-window buf t)))
        (when win
          (if (one-window-p nil (window-frame win))
              (bury-buffer buf)
            (delete-window win)))))))


(defun claude-gravity--recover-tmux-sessions ()
  "Rebuild tmux session mappings by matching session PIDs to tmux pane PIDs.
Scans all running tmux panes for claude-* sessions and matches their
pane PIDs against known session PIDs.  This recovers lost mappings
after suspend/resume, server crash, or Emacs restart."
  (let ((tmux-panes (make-hash-table :test 'equal))) ; pid -> tmux-name
    ;; Build PID→tmux-name lookup from running tmux sessions
    (condition-case nil
        (dolist (line (process-lines "tmux" "list-panes" "-a"
                                     "-F" "#{session_name} #{pane_pid}"))
          (when (string-match "^\\(claude-[^ ]+\\) \\([0-9]+\\)$" line)
            (puthash (string-to-number (match-string 2 line))
                     (match-string 1 line) tmux-panes)))
      (error nil)) ; tmux not running or no sessions
    ;; Match against sessions with known PIDs
    (let ((recovered 0))
      (maphash (lambda (sid session)
                 (let* ((pid (plist-get session :pid))
                        (tmux-name (and pid (gethash pid tmux-panes))))
                   (when (and tmux-name
                              (not (gethash sid claude-gravity--tmux-sessions)))
                     (puthash sid tmux-name claude-gravity--tmux-sessions)
                     (cl-incf recovered))))
               claude-gravity--sessions)
      (when (> recovered 0)
        (claude-gravity--log 'info "Recovered %d tmux session mapping(s)" recovered)
        (claude-gravity--render-overview))
      recovered)))


(defun claude-gravity-recover-tmux ()
  "Interactively recover lost tmux session mappings."
  (interactive)
  (let ((n (claude-gravity--recover-tmux-sessions)))
    (message "Recovered %d tmux session mapping(s)" n)))


(defun claude-gravity-reset-session (&optional session-id)
  "Reset (clear) the managed tmux Claude session for SESSION-ID.
Sends /clear to the running tmux session.  The resulting SessionEnd/SessionStart
cycle will re-key the session automatically."
  (interactive)
  (let* ((sid (or session-id
                  claude-gravity--buffer-session-id
                  (let ((found nil))
                    (maphash (lambda (id tmux-name)
                               (when (and (not found)
                                          (claude-gravity--tmux-alive-p tmux-name))
                                 (setq found id)))
                             claude-gravity--tmux-sessions)
                    found)))
         (tmux-name (and sid (gethash sid claude-gravity--tmux-sessions))))
    (unless tmux-name
      (error "No tmux Claude session found for %s" (or sid "any")))
    (unless (claude-gravity--tmux-alive-p tmux-name)
      (error "Tmux session %s is not running" tmux-name))
    (claude-gravity--tmux-send-keys tmux-name "/clear")
    (claude-gravity--log 'debug "Sent /clear to Claude [%s]" sid)))

(defun claude-gravity-toggle-permission-mode ()
  "Cycle permission mode for the managed session at point.
Cycle order: default -> auto-edit -> plan -> default."
  (interactive)
  (let* ((sid (or claude-gravity--buffer-session-id
               (let ((section (magit-current-section)))
                 (when (and section (eq (oref section type) 'session-entry))
                   (oref section value)))))
         (session (and sid (claude-gravity--get-session sid)))
         (current (or (and session (plist-get session :permission-mode)) "default"))
         (cycle '("default" "auto-edit" "plan"))
         (cur-idx (or (seq-position cycle current #'string=) 0))
         (next-mode (nth (mod (1+ cur-idx) 3) cycle)))
    (cond
     ((claude-gravity--current-session-tmux-p)
      (claude-gravity-tmux-set-permission-mode next-mode sid))
     ((claude-gravity--current-session-daemon-p)
      (claude-gravity-daemon-set-permission-mode next-mode sid))
     (t (user-error "No managed session at point")))))

(defun claude-gravity--sanitize-session-name (name)
  "Sanitize NAME for use as tmux session name and display label.
Downcases, replaces non-alphanumeric with hyphens, trims hyphens."
  (let ((s (downcase (string-trim name))))
    (setq s (replace-regexp-in-string "[^a-z0-9]+" "-" s))
    (setq s (replace-regexp-in-string "^-+\\|-+$" "" s))
    (if (string-empty-p s) "session" s)))


(defun claude-gravity-rename-session (new-name &optional session-id)
  "Rename session to NEW-NAME (updates display-name, tmux name, and buffer)."
  (interactive
   (let* ((sid (or claude-gravity--buffer-session-id
                   (let ((section (magit-current-section)))
                     (when (and section (eq (oref section type) 'session-entry))
                       (oref section value)))))
          (session (and sid (claude-gravity--get-session sid)))
          (current (if session (claude-gravity--session-label session) "")))
     (list (read-string "New session name: " current) sid)))
  (let* ((sanitized (claude-gravity--sanitize-session-name new-name))
         (sid (or session-id
                  claude-gravity--buffer-session-id
                  (let ((section (magit-current-section)))
                    (when (and section (eq (oref section type) 'session-entry))
                      (oref section value)))))
         (session (and sid (claude-gravity--get-session sid))))
    (unless session
      (user-error "No session found"))
    ;; Update display-name
    (plist-put session :display-name sanitized)
    ;; Rename buffer
    (let ((buf (or (plist-get session :buffer)
                   (get-buffer (format "*Claude: %s*"
                                       (or (plist-get session :slug)
                                           (claude-gravity--session-short-id sid)))))))
      (when (and buf (buffer-live-p buf))
        (with-current-buffer buf
          (rename-buffer (claude-gravity--session-buffer-name session) t))))
    ;; Rename tmux session if exists
    (let ((old-tmux (gethash sid claude-gravity--tmux-sessions)))
      (when (and old-tmux (claude-gravity--tmux-alive-p old-tmux))
        (let ((new-tmux (format "claude-%s" sanitized)))
          (when (= 0 (call-process "tmux" nil nil nil
                                    "rename-session" "-t" old-tmux new-tmux))
            (remhash sid claude-gravity--tmux-sessions)
            (puthash sid new-tmux claude-gravity--tmux-sessions)
            (plist-put session :tmux-session new-tmux)))))
    (claude-gravity--schedule-refresh)
    (message "Session renamed to: %s" sanitized)))


;;; ============================================================================
;;; Worktree Session Start (claude --tmux -w)
;;; ============================================================================

(defun claude-gravity-start-worktree-session (cwd branch &optional model
                                                   permission-mode max-budget
                                                   max-turns)
  "Start a Claude session with a git worktree via `claude --tmux -w BRANCH'.
CWD is the project directory.  BRANCH is the worktree branch name.
Optional MODEL, PERMISSION-MODE, MAX-BUDGET, MAX-TURNS configure the session."
  (claude-gravity--tmux-check)
  (claude-gravity--ensure-server)
  (setq cwd (claude-gravity--normalize-cwd cwd))
  (let* ((temp-id (format "tmux-%s" (format-time-string "%s%3N")))
         (plugin-root (file-name-directory
                       (or load-file-name
                           (locate-library "claude-gravity")
                           (error "Cannot locate claude-gravity.el for --plugin-dir"))))
         (sl-parts (claude-gravity--statusline-parts plugin-root))
         (cmd-args (append (claude-gravity--plugin-dirs plugin-root)
                           (list "--tmux" "-w" branch)))
         (process-environment
          (append (list (format "CLAUDE_GRAVITY_TEMP_ID=%s" temp-id))
                  (when (car sl-parts)
                    (car sl-parts))
                  process-environment)))
    (when model
      (setq cmd-args (append cmd-args (list "--model" model))))
    (when permission-mode
      (setq cmd-args (append cmd-args (list "--permission-mode" permission-mode))))
    (when max-budget
      (setq cmd-args (append cmd-args (list "--max-budget-usd" max-budget))))
    (when max-turns
      (setq cmd-args (append cmd-args (list "--max-turns" max-turns))))
    (when (cdr sl-parts)
      (setq cmd-args (append cmd-args (cdr sl-parts))))
    ;; Launch async — claude --tmux creates its own tmux session
    (let ((proc (apply #'start-process "claude-worktree" nil
                       "claude" cmd-args)))
      (set-process-sentinel
       proc (lambda (_proc event)
              (claude-gravity--log 'debug "claude --tmux -w process: %s"
                                   (string-trim event)))))
    ;; Register pending re-key; tmux session name resolved on SessionStart
    (puthash temp-id 'worktree claude-gravity--tmux-pending)
    ;; Create session with temp ID
    (let ((session (claude-gravity--ensure-session temp-id cwd)))
      (plist-put session :temp-id temp-id)
      (plist-put session :worktree-branch branch)
      (claude-gravity-model-set-claude-status session 'idle))
    (claude-gravity--tmux-ensure-heartbeat)
    (claude-gravity--record-last-project cwd)
    (claude-gravity--schedule-refresh)
    (claude-gravity--log 'debug "Claude worktree session starting: branch=%s cwd=%s"
                         branch cwd)
    temp-id))


(defun claude-gravity--find-tmux-session-by-temp-id (temp-id)
  "Find the tmux session name that has TEMP-ID in its environment.
Returns the session name or nil."
  (let ((sessions (split-string
                   (with-temp-buffer
                     (call-process "tmux" nil t nil
                                   "list-sessions" "-F" "#{session_name}")
                     (buffer-string))
                   "\n" t))
        (found nil))
    (dolist (name sessions found)
      (unless found
        (let ((env-out (with-temp-buffer
                         (call-process "tmux" nil t nil
                                       "show-environment" "-t" name
                                       "CLAUDE_GRAVITY_TEMP_ID")
                         (buffer-string))))
          (when (string-match (regexp-quote temp-id) env-out)
            (setq found name)))))))


;;; ============================================================================
;;; Transient Start Menu
;;; ============================================================================

(defun claude-gravity--read-project-dir-for-transient (prompt _initial-input _history)
  "Read a project directory for the transient menu.
PROMPT is the prompt string.  _INITIAL-INPUT and _HISTORY are ignored."
  (claude-gravity--read-project-dir prompt
                                    (or (claude-gravity--infer-cwd-from-section)
                                        claude-gravity--last-project-dir
                                        default-directory)))

;;;###autoload (autoload 'claude-gravity-start-menu "claude-gravity-tmux" nil t)
(transient-define-prefix claude-gravity-start-menu ()
  "Configure and start a new Claude session."
  ["Project"
   ("-d" "Directory" "--directory="
    :reader claude-gravity--read-project-dir-for-transient)]
  ["Parameters"
   ("-m" "Model" "--model=" :choices ("opus" "sonnet" "haiku"))
   ("-p" "Permission mode" "--permission-mode="
    :choices ("default" "auto-edit" "plan" "full-auto"))
   ("-b" "Max budget (USD)" "--max-budget=")
   ("-t" "Max turns" "--max-turns=")]
  ["Worktree"
   ("-w" "Branch name" "--worktree=")]
  ["Actions"
   ("s" "Start" claude-gravity--start-session-from-transient)
   ("q" "Quit" transient-quit-one)])

(defun claude-gravity--transient-get-value (args flag)
  "Extract value for FLAG from transient ARGS list.
FLAG should be like \"--model=\"."
  (cl-some (lambda (arg)
             (when (string-prefix-p flag arg)
               (substring arg (length flag))))
           args))

(transient-define-suffix claude-gravity--start-session-from-transient ()
  "Start a Claude session using transient parameters."
  :transient nil
  (interactive)
  (let* ((args (transient-args 'claude-gravity-start-menu))
         (dir (claude-gravity--transient-get-value args "--directory="))
         (model (claude-gravity--transient-get-value args "--model="))
         (perm (claude-gravity--transient-get-value args "--permission-mode="))
         (budget (claude-gravity--transient-get-value args "--max-budget="))
         (turns (claude-gravity--transient-get-value args "--max-turns="))
         (worktree (claude-gravity--transient-get-value args "--worktree="))
         (cwd (or dir
                  (claude-gravity--infer-cwd-from-section)
                  claude-gravity--last-project-dir
                  default-directory)))
    (if worktree
        (claude-gravity-start-worktree-session cwd worktree model perm budget turns)
      (claude-gravity-start-session cwd model perm budget turns))))


(provide 'claude-gravity-tmux)
;;; claude-gravity-tmux.el ends here