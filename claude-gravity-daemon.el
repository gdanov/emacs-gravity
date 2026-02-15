;;; claude-gravity-daemon.el --- SDK Daemon session management for Claude Gravity  -*- lexical-binding: t; -*-

;;; Code:

(require 'claude-gravity-core)
(require 'claude-gravity-session)
(require 'claude-gravity-state)
(require 'claude-gravity-socket)

(defvar claude-gravity-mode-map)
(defvar claude-gravity--follow-mode)
(defvar claude-gravity--buffer-session-id)
(defvar claude-gravity--compose-session-id)
(defvar claude-gravity--compose-separator)
(defvar-local claude-gravity--compose-backend nil
  "Backend for this compose buffer: `daemon' or nil (tmux).")
(declare-function claude-gravity--session-buffer-name "claude-gravity-ui")
(declare-function claude-gravity--session-header-line "claude-gravity-ui")
(declare-function claude-gravity--follow-detect-manual "claude-gravity-ui")
(declare-function claude-gravity--compose-insert-history "claude-gravity-tmux")
(declare-function claude-gravity--compose-guard-history "claude-gravity-tmux")
(declare-function claude-gravity--compose-cleanup "claude-gravity-tmux")
(declare-function claude-gravity--compose-cleanup-buffer "claude-gravity-tmux")
(declare-function claude-gravity--compose-quit-hook "claude-gravity-tmux")
(declare-function claude-gravity-compose-mode "claude-gravity-tmux")
(declare-function claude-gravity--reset-session "claude-gravity-state")


;;; ============================================================================
;;; SDK Daemon Sessions
;;; ============================================================================
;;; The daemon is a long-running Node.js process (emacs-bridge/dist/daemon.js)
;;; that manages Claude Agent SDK sessions.  Emacs sends commands over a
;;; Unix domain socket, the daemon forwards hook events and streaming text
;;; back through the gravity socket (same format as the one-shot bridge).
;;;
;;; All hook data (tools, agents, prompts) arrives through the standard
;;; gravity socket → handle-event path.  The daemon adds: StreamDelta
;;; (streaming text), AssistantComplete, DaemonResult, ToolProgress.

(defvar claude-gravity--daemon-process nil
  "The daemon Node.js process (Emacs subprocess).")

(defvar claude-gravity--daemon-socket-path nil
  "Path to the daemon command socket (set when daemon reports READY).")

(defvar claude-gravity--daemon-sessions (make-hash-table :test 'equal)
  "Set of session-ids managed by the daemon.  Value is t.")

(defvar claude-gravity--daemon-pending (make-hash-table :test 'equal)
  "Map of temp-id → t for daemon sessions awaiting SessionStart re-keying.")

(defvar claude-gravity--daemon-ready nil
  "Non-nil when daemon has reported READY and socket path is known.")


;;; ============================================================================
;;; Daemon lifecycle
;;; ============================================================================

(defun claude-gravity--daemon-js-path ()
  "Return the path to the daemon.js entry point."
  (let ((plugin-root (file-name-directory
                      (or load-file-name
                          (locate-library "claude-gravity")
                          (error "Cannot locate claude-gravity.el")))))
    (expand-file-name "emacs-bridge/dist/daemon.js" plugin-root)))


(defun claude-gravity-daemon-start ()
  "Start the gravity daemon process.
The daemon listens on a Unix domain socket for commands and forwards
SDK hook events and streaming text to Emacs via the gravity socket."
  (interactive)
  (when (and claude-gravity--daemon-process
             (process-live-p claude-gravity--daemon-process))
    (user-error "Daemon is already running"))
  (claude-gravity--ensure-server)
  (let ((daemon-js (claude-gravity--daemon-js-path)))
    (unless (file-exists-p daemon-js)
      (error "Daemon not built: %s not found.  Run `cd emacs-bridge && npm run build'" daemon-js))
    (setq claude-gravity--daemon-ready nil
          claude-gravity--daemon-socket-path nil
          claude-gravity--daemon-process
          (make-process
           :name "gravity-daemon"
           :command (list "node" daemon-js)
           :buffer (get-buffer-create " *gravity-daemon*")
           :filter #'claude-gravity--daemon-process-filter
           :sentinel #'claude-gravity--daemon-process-sentinel
           :coding 'utf-8
           :noquery t
           :connection-type 'pipe))
    (claude-gravity--log 'info "Daemon starting (PID %s)"
                         (process-id claude-gravity--daemon-process))))


(defun claude-gravity--daemon-process-filter (proc string)
  "Process output from daemon PROC.
Watches for READY line to capture the command socket path."
  ;; Accumulate in process buffer for debugging
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (goto-char (point-max))
      (insert string)))
  ;; Parse READY signal
  (when (and (not claude-gravity--daemon-ready)
             (string-match "^READY \\(.+\\)$" string))
    (setq claude-gravity--daemon-socket-path (match-string 1 string)
          claude-gravity--daemon-ready t)
    (claude-gravity--log 'info "Daemon ready: %s" claude-gravity--daemon-socket-path)))


(defun claude-gravity--daemon-process-sentinel (proc event)
  "Handle daemon PROC state change EVENT."
  (claude-gravity--log 'warn "Daemon process event: %s" (string-trim event))
  (when (memq (process-status proc) '(exit signal))
    (setq claude-gravity--daemon-process nil
          claude-gravity--daemon-ready nil
          claude-gravity--daemon-socket-path nil)
    ;; Mark all daemon sessions as ended
    (maphash (lambda (sid _)
               (let ((session (claude-gravity--get-session sid)))
                 (when (and session (not (eq (plist-get session :status) 'ended)))
                   (claude-gravity-model-session-end session))))
             claude-gravity--daemon-sessions)
    (clrhash claude-gravity--daemon-sessions)
    (clrhash claude-gravity--daemon-pending)
    (claude-gravity--schedule-refresh)))


(defun claude-gravity-daemon-stop ()
  "Shut down the gravity daemon."
  (interactive)
  (if (not (claude-gravity--daemon-alive-p))
      (message "Daemon is not running")
    (claude-gravity--daemon-send-command '((cmd . "shutdown")))
    ;; Give it a moment, then force kill if still alive
    (run-at-time 2 nil
      (lambda ()
        (when (and claude-gravity--daemon-process
                   (process-live-p claude-gravity--daemon-process))
          (kill-process claude-gravity--daemon-process))))))


(defun claude-gravity--daemon-alive-p ()
  "Return non-nil if the daemon is running and ready."
  (and claude-gravity--daemon-process
       (process-live-p claude-gravity--daemon-process)
       claude-gravity--daemon-ready))


(defun claude-gravity-daemon-ensure ()
  "Start the daemon if it is not already running.
Waits up to 5 seconds for the daemon to become ready."
  (unless (claude-gravity--daemon-alive-p)
    (claude-gravity-daemon-start)
    ;; Wait for READY signal
    (let ((deadline (+ (float-time) 5.0)))
      (while (and (< (float-time) deadline)
                  (not claude-gravity--daemon-ready))
        (accept-process-output claude-gravity--daemon-process 0.1))
      (unless claude-gravity--daemon-ready
        (error "Daemon failed to start within 5 seconds")))))


;;; ============================================================================
;;; Command communication
;;; ============================================================================

(defun claude-gravity--daemon-send-command (cmd)
  "Send CMD alist to the daemon and return the parsed response.
Returns nil on error."
  (unless claude-gravity--daemon-socket-path
    (error "Daemon not ready (no socket path)"))
  (let ((proc nil)
        (buf (generate-new-buffer " *daemon-cmd*")))
    (unwind-protect
        (progn
          (setq proc (make-network-process
                      :name "daemon-cmd"
                      :family 'local
                      :remote claude-gravity--daemon-socket-path
                      :buffer buf
                      :coding 'utf-8
                      :noquery t))
          (process-send-string proc (concat (json-encode cmd) "\n"))
          ;; Wait for response (up to 5 seconds)
          (with-current-buffer buf
            (let ((deadline (+ (float-time) 5.0)))
              (while (and (< (float-time) deadline)
                          (process-live-p proc)
                          (progn (goto-char (point-min))
                                 (not (search-forward "\n" nil t))))
                (accept-process-output proc 0.1)))
            (goto-char (point-min))
            (when (search-forward "\n" nil t)
              (condition-case nil
                  (json-parse-string
                   (buffer-substring-no-properties (point-min) (1- (point)))
                   :object-type 'alist)
                (error nil)))))
      (when (and proc (process-live-p proc))
        (delete-process proc))
      (when (buffer-live-p buf)
        (kill-buffer buf)))))


;;; ============================================================================
;;; Session management
;;; ============================================================================

(defun claude-gravity--current-session-daemon-p ()
  "Return non-nil if the current session is daemon-managed."
  (let ((sid (or claude-gravity--buffer-session-id
                 (let ((section (magit-current-section)))
                   (when (and section (eq (oref section type) 'session-entry))
                     (oref section value))))))
    (and sid (gethash sid claude-gravity--daemon-sessions))))


(defun claude-gravity-daemon-start-session (cwd &optional model permission-mode)
  "Start a new Claude session in CWD via the SDK daemon.
Optional MODEL overrides the default.  PERMISSION-MODE sets the permission mode.
Returns the temp session-id (re-keyed when SessionStart arrives)."
  (interactive
   (list (read-directory-name "Project directory: " default-directory)))
  (claude-gravity-daemon-ensure)
  (setq cwd (claude-gravity--normalize-cwd cwd))
  (let* ((temp-id (format "daemon-%s" (format-time-string "%s%3N")))
         (cmd `((cmd . "start")
                (id . ,temp-id)
                (cwd . ,cwd)
                ,@(when model `((model . ,model)))
                ,@(when permission-mode `((permission_mode . ,permission-mode)))))
         (response (claude-gravity--daemon-send-command cmd)))
    (if (and response (eq (alist-get 'ok response) t))
        (progn
          ;; Register pending re-key
          (puthash temp-id t claude-gravity--daemon-pending)
          (puthash temp-id t claude-gravity--daemon-sessions)
          ;; Create session with temp ID
          (let ((session (claude-gravity--ensure-session temp-id cwd)))
            (plist-put session :temp-id temp-id)
            (plist-put session :managed-by 'daemon)
            (claude-gravity-model-set-claude-status session 'idle))
          (claude-gravity--schedule-refresh)
          (claude-gravity--log 'info "Daemon session starting: %s in %s" temp-id cwd)
          temp-id)
      (error "Failed to start daemon session: %s"
             (or (alist-get 'error response) "unknown error")))))


(defun claude-gravity-daemon-resume-session (session-id &optional cwd model)
  "Resume an existing Claude session by SESSION-ID via the SDK daemon.
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
  (claude-gravity-daemon-ensure)
  (let* ((existing (claude-gravity--get-session session-id))
         (cwd (claude-gravity--normalize-cwd
               (or cwd
                   (and existing (plist-get existing :cwd))
                   (read-directory-name "Project directory: "))))
         (temp-id (format "daemon-%s" (format-time-string "%s%3N")))
         (cmd `((cmd . "resume")
                (id . ,temp-id)
                (session_id . ,session-id)
                (cwd . ,cwd)
                ,@(when model `((model . ,model)))))
         (response (claude-gravity--daemon-send-command cmd)))
    (if (and response (eq (alist-get 'ok response) t))
        (progn
          (puthash temp-id t claude-gravity--daemon-pending)
          (puthash temp-id t claude-gravity--daemon-sessions)
          ;; Update or create session
          (let ((session (claude-gravity--ensure-session session-id cwd)))
            (when (eq (plist-get session :status) 'ended)
              (plist-put session :status 'active))
            (plist-put session :temp-id temp-id)
            (plist-put session :managed-by 'daemon)
            (claude-gravity-model-set-claude-status session 'idle))
          (claude-gravity--schedule-refresh)
          (claude-gravity--log 'info "Daemon session resuming: %s" session-id)
          session-id)
      (error "Failed to resume daemon session: %s"
             (or (alist-get 'error response) "unknown error")))))


(defun claude-gravity--resolve-daemon-session (&optional session-id)
  "Resolve SESSION-ID to a daemon-managed session ID.
Falls back to buffer-local session, section at point, then any active daemon session."
  (or session-id
      (and claude-gravity--buffer-session-id
           (gethash claude-gravity--buffer-session-id claude-gravity--daemon-sessions)
           claude-gravity--buffer-session-id)
      (let ((section (magit-current-section)))
        (when (and section (eq (oref section type) 'session-entry))
          (let ((sid (oref section value)))
            (when (gethash sid claude-gravity--daemon-sessions) sid))))
      ;; Any running daemon session
      (let ((found nil))
        (maphash (lambda (sid _)
                   (unless found
                     (let ((session (claude-gravity--get-session sid)))
                       (when (and session (eq (plist-get session :status) 'active))
                         (setq found sid)))))
                 claude-gravity--daemon-sessions)
        found)
      (error "No daemon session found")))


(defun claude-gravity-daemon-send-prompt (text &optional session-id)
  "Send TEXT prompt to daemon session SESSION-ID."
  (interactive (list (read-string "Prompt: ")))
  (let* ((sid (claude-gravity--resolve-daemon-session session-id))
         (cmd `((cmd . "prompt")
                (session_id . ,sid)
                (text . ,text)))
         (response (claude-gravity--daemon-send-command cmd)))
    (unless (and response (eq (alist-get 'ok response) t))
      (error "Failed to send prompt: %s"
             (or (alist-get 'error response) "unknown error")))
    ;; Update model state
    (let ((session (claude-gravity--get-session sid)))
      (when session
        (claude-gravity-model-add-prompt
         session (list (cons 'text text)
                       (cons 'submitted (current-time))
                       (cons 'elapsed nil)
                       (cons 'stop_text nil)
                       (cons 'stop_thinking nil)))
        ;; Flag to prevent UserPromptSubmit hook from adding duplicate turn
        (plist-put session :daemon-prompt-sent t)
        (claude-gravity-model-set-claude-status session 'responding)
        (claude-gravity--schedule-session-refresh sid)
        ;; Auto-enable follow mode
        (let ((buf (or (let ((b (plist-get session :buffer)))
                         (and b (buffer-live-p b) b))
                       (get-buffer (claude-gravity--session-buffer-name session)))))
          (when (and buf (not (buffer-local-value 'claude-gravity--follow-mode buf)))
            (with-current-buffer buf
              (setq claude-gravity--follow-mode t)
              (add-hook 'post-command-hook #'claude-gravity--follow-detect-manual nil t)
              (force-mode-line-update))))))
    (claude-gravity--log 'debug "Sent daemon prompt to [%s]" sid)))


(defun claude-gravity-daemon-stop-session (&optional session-id)
  "Stop the daemon session SESSION-ID."
  (interactive)
  (let* ((sid (claude-gravity--resolve-daemon-session session-id))
         (cmd `((cmd . "stop") (session_id . ,sid)))
         (response (claude-gravity--daemon-send-command cmd)))
    (when (and response (eq (alist-get 'ok response) t))
      (remhash sid claude-gravity--daemon-sessions)
      (let ((session (claude-gravity--get-session sid)))
        (when session
          (claude-gravity-model-session-end session)))
      (claude-gravity--schedule-refresh)
      (claude-gravity--log 'info "Stopped daemon session [%s]" sid))))


(defun claude-gravity-daemon-interrupt (&optional session-id)
  "Interrupt the current generation in daemon session SESSION-ID."
  (interactive)
  (let* ((sid (claude-gravity--resolve-daemon-session session-id))
         (cmd `((cmd . "interrupt") (session_id . ,sid))))
    (claude-gravity--daemon-send-command cmd)
    (claude-gravity--log 'debug "Interrupted daemon session [%s]" sid)))


(defun claude-gravity-daemon-set-model (model &optional session-id)
  "Change the MODEL for daemon session SESSION-ID."
  (interactive
   (list (completing-read "Model: " '("sonnet" "opus" "haiku") nil t)))
  (let* ((sid (claude-gravity--resolve-daemon-session session-id))
         (cmd `((cmd . "set_model") (session_id . ,sid) (model . ,model))))
    (claude-gravity--daemon-send-command cmd)
    (claude-gravity--log 'debug "Set model %s for [%s]" model sid)))


(defun claude-gravity-daemon-set-permission-mode (mode &optional session-id)
  "Change the permission MODE for daemon session SESSION-ID."
  (interactive
   (list (completing-read "Permission mode: " '("default" "plan" "auto-edit") nil t)))
  (let* ((sid (claude-gravity--resolve-daemon-session session-id))
         (cmd `((cmd . "set_permission_mode") (session_id . ,sid) (mode . ,mode))))
    (claude-gravity--daemon-send-command cmd)
    (claude-gravity--log 'debug "Set permission mode %s for [%s]" mode sid)))


;;; ============================================================================
;;; Compose buffer (chat-style prompt entry for daemon sessions)
;;; ============================================================================

(defun claude-gravity-daemon-compose-prompt (&optional session-id)
  "Open a compose buffer to send a prompt to a daemon session.
SESSION-ID defaults to the current buffer's session."
  (interactive)
  (let* ((sid (claude-gravity--resolve-daemon-session session-id))
         (session (claude-gravity--get-session sid))
         (label (if session (claude-gravity--session-label session) (substring sid 0 8)))
         (buf-name (format "*Claude Compose: %s*" label))
         (buf (get-buffer-create buf-name)))
    (with-current-buffer buf
      (if (fboundp 'markdown-mode) (markdown-mode) (text-mode))
      (remove-hook 'before-change-functions
                   #'claude-gravity--compose-guard-history t)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (when session
          (claude-gravity--compose-insert-history session))
        (insert (propertize (concat (make-string 50 ?─) "\n")
                            'face 'claude-gravity-divider))
        (setq-local claude-gravity--compose-separator (copy-marker (point)))
        (when (> (point) 2)
          (put-text-property 1 (1- (point)) 'read-only t)
          (put-text-property 1 (1- (point)) 'front-sticky '(read-only))))
      (add-hook 'before-change-functions
                #'claude-gravity--compose-guard-history nil t)
      (setq-local claude-gravity--compose-session-id sid)
      (setq-local claude-gravity--compose-backend 'daemon)
      (claude-gravity-compose-mode 1)
      (when (fboundp 'olivetti-mode) (olivetti-mode 1))
      (add-hook 'post-command-hook
                #'claude-gravity--compose-quit-hook nil t)
      (goto-char (point-max)))
    (display-buffer-in-side-window buf '((side . bottom) (window-height . 0.40)))
    (select-window (get-buffer-window buf))
    (goto-char (point-max))))


(defun claude-gravity-daemon-compose-send ()
  "Send the composed prompt from a daemon compose buffer."
  (interactive)
  (let* ((sep claude-gravity--compose-separator)
         (text (string-trim
                (buffer-substring-no-properties
                 (if sep (marker-position sep) (point-min))
                 (point-max)))))
    (if (string-empty-p text)
        (message "Nothing to send")
      (let ((sid claude-gravity--compose-session-id))
        (claude-gravity-daemon-send-prompt text sid)
        (claude-gravity--compose-cleanup)
        (message "Prompt sent")))))


;;; ============================================================================
;;; SessionStart re-keying support
;;; ============================================================================

(defun claude-gravity--daemon-rekey-session (temp-id session-id data pid)
  "Re-key daemon session from TEMP-ID to SESSION-ID.
DATA is the SessionStart payload, PID the process ID.
Returns non-nil if a daemon session was re-keyed."
  (when (gethash temp-id claude-gravity--daemon-pending)
    (remhash temp-id claude-gravity--daemon-pending)
    (let ((temp-session (gethash temp-id claude-gravity--sessions)))
      (when temp-session
        ;; Re-key session hash
        (remhash temp-id claude-gravity--sessions)
        (plist-put temp-session :session-id session-id)
        (plist-put temp-session :temp-id temp-id)
        (puthash session-id temp-session claude-gravity--sessions)
        ;; Update daemon sessions set
        (remhash temp-id claude-gravity--daemon-sessions)
        (puthash session-id t claude-gravity--daemon-sessions)
        ;; Rename buffer
        (let ((old-buf (plist-get temp-session :buffer)))
          (when (and old-buf (buffer-live-p old-buf))
            (let ((new-name (claude-gravity--session-buffer-name temp-session)))
              (with-current-buffer old-buf
                (rename-buffer new-name t)
                (setq claude-gravity--buffer-session-id session-id)))))
        ;; NOTE: No reset-session here — this is a brand-new daemon session,
        ;; not a /clear re-keying.  The prompt was already added by
        ;; daemon-send-prompt and must survive re-keying.
        ;; Apply metadata
        (claude-gravity-model-update-session-meta
         temp-session :pid pid :slug (alist-get 'slug data))))
    t))


;;; ============================================================================
;;; Cleanup
;;; ============================================================================

(defun claude-gravity--daemon-cleanup ()
  "Stop the daemon and clean up.  For `kill-emacs-hook'."
  (when (and claude-gravity--daemon-process
             (process-live-p claude-gravity--daemon-process))
    (ignore-errors
      (claude-gravity--daemon-send-command '((cmd . "shutdown"))))
    (run-at-time 1 nil
      (lambda ()
        (when (and claude-gravity--daemon-process
                   (process-live-p claude-gravity--daemon-process))
          (ignore-errors (kill-process claude-gravity--daemon-process))))))
  (clrhash claude-gravity--daemon-sessions)
  (clrhash claude-gravity--daemon-pending))


(add-hook 'kill-emacs-hook #'claude-gravity--daemon-cleanup)


;;; ============================================================================
;;; Unified session dispatch
;;; ============================================================================
;;; These commands auto-detect whether the current session is daemon-managed
;;; or tmux-managed and dispatch to the appropriate backend.  Keybindings
;;; point here so a single key (e.g. `s', `K') works for any session type.

(declare-function claude-gravity-compose-prompt "claude-gravity-tmux")
(declare-function claude-gravity-stop-session "claude-gravity-tmux")
(declare-function claude-gravity-send-escape "claude-gravity-tmux")
(declare-function claude-gravity-resume-session "claude-gravity-tmux")
(declare-function claude-gravity--current-session-tmux-p "claude-gravity-tmux")

(defun claude-gravity--current-session-managed-p ()
  "Return non-nil if the session at point is managed (daemon or tmux)."
  (or (claude-gravity--current-session-daemon-p)
      (claude-gravity--current-session-tmux-p)))

(defun claude-gravity-unified-compose (&optional session-id)
  "Open compose buffer for the current session (daemon or tmux)."
  (interactive)
  (cond
   ((claude-gravity--current-session-daemon-p)
    (claude-gravity-daemon-compose-prompt session-id))
   ((claude-gravity--current-session-tmux-p)
    (claude-gravity-compose-prompt session-id))
   (t (user-error "No managed session at point"))))

(defun claude-gravity-unified-stop (&optional session-id)
  "Stop the current session (daemon or tmux)."
  (interactive)
  (cond
   ((claude-gravity--current-session-daemon-p)
    (claude-gravity-daemon-stop-session session-id))
   ((claude-gravity--current-session-tmux-p)
    (claude-gravity-stop-session session-id))
   (t (user-error "No managed session at point"))))

(defun claude-gravity-unified-interrupt ()
  "Interrupt the current session (daemon: interrupt, tmux: send Escape)."
  (interactive)
  (cond
   ((claude-gravity--current-session-daemon-p)
    (claude-gravity-daemon-interrupt))
   ((claude-gravity--current-session-tmux-p)
    (claude-gravity-send-escape))
   (t (user-error "No managed session at point"))))

(defun claude-gravity-unified-resume (session-id &optional cwd model)
  "Resume an ended session (daemon or tmux).
SESSION-ID is the session to resume."
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
  ;; Decide backend: if daemon is running, use it; otherwise tmux
  (if (claude-gravity--daemon-alive-p)
      (claude-gravity-daemon-resume-session session-id cwd model)
    (claude-gravity-resume-session session-id cwd model)))


(provide 'claude-gravity-daemon)
;;; claude-gravity-daemon.el ends here
