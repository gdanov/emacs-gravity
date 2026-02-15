;;; claude-gravity-socket.el --- Socket server and bidirectional communication for Claude Gravity  -*- lexical-binding: t; -*-

;;; Code:

(require 'claude-gravity-core)
(require 'claude-gravity-faces)
(require 'claude-gravity-session)
(require 'claude-gravity-state)
(require 'claude-gravity-events)
(require 'claude-gravity-text)
(require 'claude-gravity-diff)

(declare-function claude-gravity--render-overview "claude-gravity-ui")
(declare-function claude-gravity--session-buffer-name "claude-gravity-ui")
(declare-function claude-gravity--follow-detect-manual "claude-gravity-ui")
(declare-function claude-gravity--inbox-act-permission "claude-gravity-actions")
(declare-function claude-gravity--inbox-act-question "claude-gravity-actions")
(declare-function claude-gravity--inbox-act-plan-review "claude-gravity-actions")
(declare-function claude-gravity--inbox-act-idle "claude-gravity-actions")
(declare-function claude-gravity--debug-capture-message "claude-gravity-debug")
(declare-function claude-gravity--recover-tmux-sessions "claude-gravity-tmux")
(defvar claude-gravity--debug-messages-enabled)


;;; Socket Server

(defvar claude-gravity--server-stopping nil
  "Non-nil when server stop is intentional (suppress sentinel auto-restart).")

(defvar claude-gravity--health-timer nil
  "Repeating timer that checks server liveness and reconciles sessions.")

;;; Notification mode-line indicator

(defvar claude-gravity--notification-indicator ""
  "Mode-line string showing Claude notification status.
Added to `global-mode-string' by `claude-gravity-server-start'.")

(put 'claude-gravity--notification-indicator 'risky-local-variable t)

(defun claude-gravity--update-notification-indicator (notification-type label)
  "Update the mode-line indicator based on NOTIFICATION-TYPE and LABEL."
  (setq claude-gravity--notification-indicator
        (cond
         ((equal notification-type "permission")
          (propertize (format " [Claude: permission]") 'face 'claude-gravity-question))
         ((or (equal notification-type "waiting")
              (equal notification-type "tool"))
          (propertize (format " [Claude: waiting]") 'face 'claude-gravity-status-responding))
         (notification-type
          (propertize (format " [Claude: %s]" notification-type) 'face 'claude-gravity-status-responding))
         (t
          (propertize " [Claude: notice]" 'face 'claude-gravity-status-responding))))
  (force-mode-line-update t))


(defun claude-gravity--clear-notification-indicator ()
  "Clear the mode-line notification indicator.
If there are inbox items needing attention, shows the inbox indicator instead."
  (let ((attention (cl-count-if
                    (lambda (item) (not (eq (alist-get 'type item) 'idle)))
                    claude-gravity--inbox)))
    (if (> attention 0)
        (claude-gravity--update-inbox-indicator)
      (when (not (string-empty-p claude-gravity--notification-indicator))
        (setq claude-gravity--notification-indicator "")
        (force-mode-line-update t)))))


(defun claude-gravity--update-inbox-indicator ()
  "Update the mode-line indicator based on inbox contents."
  (let ((attention (cl-count-if
                    (lambda (item) (not (eq (alist-get 'type item) 'idle)))
                    claude-gravity--inbox))
        (has-sessions (> (hash-table-count claude-gravity--sessions) 0)))
    (setq claude-gravity--notification-indicator
          (cond
           ((> attention 0)
            (propertize (format " [Claude: %d!]" attention)
                        'face 'claude-gravity-question))
           (has-sessions
            (propertize " [Claude]" 'face 'claude-gravity-status-idle))
           (t "")))
    (force-mode-line-update t)))


(defun claude-gravity--inbox-notify (item)
  "Flash a message and update mode-line for new inbox ITEM."
  (claude-gravity--update-inbox-indicator)
  (let ((type-label (pcase (alist-get 'type item)
                      ('permission "Permission")
                      ('question "Question")
                      ('plan-review "Plan review")
                      ('idle "Idle")
                      (_ "Notice")))
        (project (or (alist-get 'project item) "?")))
    (claude-gravity--log 'debug "Claude: %s from %s" type-label project)))


(defvar claude-gravity-server-sock-path
  (expand-file-name "claude-gravity.sock" (file-name-directory (or load-file-name buffer-file-name)))
  "Path to the Unix socket for Claude Gravity communication.")


(defvar claude-gravity-server-process nil
  "The current Claude Gravity server process.")


(defun claude-gravity--server-filter (proc string)
  "Process incoming JSON events from PROC in STRING.
Accumulates partial data per connection until complete lines arrive."
  (let ((buf (concat (or (process-get proc 'claude-gravity--buffer) "") string)))
    ;; Process all complete lines (terminated by newline)
    (while (string-match "\n" buf)
      (let ((line (substring buf 0 (match-beginning 0))))
        (setq buf (substring buf (match-end 0)))
        (when (> (length line) 0)
          (condition-case err
              (let* ((data (json-parse-string line :object-type 'alist :array-type 'array)))
                (when claude-gravity--debug-messages-enabled
                  (claude-gravity--debug-capture-message line data nil))
                (claude-gravity--log 'debug "Claude Gravity received: %s" (alist-get 'event data))
                (let ((event (alist-get 'event data))
                      (session-id (alist-get 'session_id data))
                      (cwd (alist-get 'cwd data))
                      (pid (alist-get 'pid data))
                      (source (alist-get 'source data))
                      (payload (alist-get 'data data))
                      (needs-response (alist-get 'needs_response data)))
                  (if needs-response
                      ;; Bidirectional: queue in inbox.
                      ;; Do NOT call handle-event "PreToolUse" here — the generic
                      ;; PreToolUse hook already fires for every tool and will
                      ;; register it in session state separately.
                      (let ((tool-name (alist-get 'tool_name payload)))
                        (claude-gravity--ensure-session session-id cwd)
                        (cond
                         ((equal tool-name "AskUserQuestion")
                          (claude-gravity--inbox-add 'question session-id payload proc))
                         ((equal tool-name "ExitPlanMode")
                          (claude-gravity--inbox-add 'plan-review session-id payload proc))
                         (t
                          (claude-gravity--inbox-add 'permission session-id payload proc))))
                    (when event
                      (claude-gravity-handle-event event session-id cwd payload pid source)))))
            (error
             (when claude-gravity--debug-messages-enabled
               (claude-gravity--debug-capture-message line nil err))
             (claude-gravity--log 'error "Claude Gravity JSON error: %s" err))))))
    ;; Store any remaining partial data
    (process-put proc 'claude-gravity--buffer buf)))


(defun claude-gravity-server-alive-p ()
  "Return non-nil if the socket server is running."
  (and claude-gravity-server-process
       (process-live-p claude-gravity-server-process)))


(defun claude-gravity--ensure-server ()
  "Start the socket server if it is not already running.
Respects `claude-gravity--server-stopping' to avoid restarting after intentional stop."
  (unless (or claude-gravity--server-stopping
              (claude-gravity-server-alive-p))
    (claude-gravity-server-start)))


(defun claude-gravity--server-sentinel (_proc event)
  "Auto-restart server on unexpected death (not user-initiated stop).
EVENT is the process status change string."
  (claude-gravity--log 'warn "Server process event: %s" (string-trim event))
  (unless claude-gravity--server-stopping
    (run-at-time 1 nil #'claude-gravity--ensure-server)))


(defun claude-gravity--reconcile-sessions ()
  "Re-check ended sessions and mark active if their PID is still alive.
Called after server restart to recover sessions that were falsely marked ended."
  (let ((recovered 0))
    (maphash (lambda (_id session)
               (let ((pid (plist-get session :pid)))
                 (when (and (eq (plist-get session :status) 'ended)
                            pid (numberp pid) (> pid 0)
                            (claude-gravity--process-alive-p pid))
                   (plist-put session :status 'active)
                   (plist-put session :claude-status 'idle)
                   (cl-incf recovered))))
             claude-gravity--sessions)
    (when (> recovered 0)
      (claude-gravity--log 'info "Reconciled %d session(s) back to active" recovered)
      (claude-gravity--render-overview))))


(defun claude-gravity--health-check ()
  "Periodic check: restart server if dead, reconcile session liveness."
  (unless (claude-gravity-server-alive-p)
    (claude-gravity--log 'warn "Health check: server dead, restarting")
    (claude-gravity--ensure-server))
  ;; Always reconcile: recover ended sessions whose PID is still alive
  (claude-gravity--reconcile-sessions)
  ;; Always try to recover lost tmux mappings
  (claude-gravity--recover-tmux-sessions))


(defun claude-gravity-server-start ()
  "Start the Unix socket server for Claude Gravity."
  (interactive)
  (setq claude-gravity--server-stopping t)  ; suppress sentinel during restart
  (claude-gravity-server-stop)
  (setq claude-gravity--server-stopping nil)
  (when (file-exists-p claude-gravity-server-sock-path)
    (delete-file claude-gravity-server-sock-path))
  (setq claude-gravity-server-process
        (make-network-process
         :name "claude-gravity-server"
         :server t
         :family 'local
         :service claude-gravity-server-sock-path
         :filter 'claude-gravity--server-filter
         :sentinel 'claude-gravity--server-sentinel))
  ;; Register notification indicator in global-mode-string (shown in mode-line misc-info).
  ;; global-mode-string must start with "" for format-mode-line to work;
  ;; without it, a bare symbol list is interpreted as an invalid mode-line construct.
  (unless (memq 'claude-gravity--notification-indicator global-mode-string)
    (if (or (null global-mode-string) (equal global-mode-string '("")))
        (setq global-mode-string '("" claude-gravity--notification-indicator))
      (push 'claude-gravity--notification-indicator (cdr (last global-mode-string)))))
  ;; Clear wrap cache on window resize (fill-column depends on window width)
  (add-hook 'window-size-change-functions
            (lambda (_frame) (claude-gravity--wrap-cache-clear)))
  ;; Start periodic health check (catches silent socket death from suspend/resume)
  (when claude-gravity--health-timer
    (cancel-timer claude-gravity--health-timer))
  (setq claude-gravity--health-timer
        (run-with-timer 30 30 #'claude-gravity--health-check))
  (claude-gravity--log 'info "Claude Gravity server started at %s" claude-gravity-server-sock-path))


(defun claude-gravity-server-stop ()
  "Stop the Claude Gravity server."
  (interactive)
  (setq claude-gravity--server-stopping t)
  (when claude-gravity--health-timer
    (cancel-timer claude-gravity--health-timer)
    (setq claude-gravity--health-timer nil))
  (when claude-gravity-server-process
    (delete-process claude-gravity-server-process)
    (setq claude-gravity-server-process nil))
  (when (file-exists-p claude-gravity-server-sock-path)
    (delete-file claude-gravity-server-sock-path))
  (claude-gravity--log 'info "Claude Gravity server stopped"))


;;; Plan Review Mode

(defvar claude-gravity--plan-review-queue nil
  "Queue of pending plan review requests.
Each entry is (event-data proc session-id).")


(defvar-local claude-gravity--plan-review-proc nil
  "The socket process to send the review decision back on.")


(defvar-local claude-gravity--plan-review-original nil
  "Original plan content for computing diffs.")


(defvar-local claude-gravity--plan-review-session-id nil
  "Session ID associated with this plan review.")


(defvar-local claude-gravity--plan-review-comments nil
  "List of inline comments in the plan review buffer.
Each entry is an alist with keys: line, text, overlay, context.")


(defvar-local claude-gravity--plan-review-inbox-id nil
  "Inbox item ID associated with this plan review buffer, if any.")


(defvar-local claude-gravity--plan-review-margin-overlays nil
  "List of fringe overlays for plan revision margin indicators.")


(defvar-local claude-gravity--plan-review-prev-content nil
  "Previous plan content string, set when revision indicators are shown.")



(defvar claude-gravity-plan-review-mode-map (make-sparse-keymap)
  "Keymap for `claude-gravity-plan-review-mode'.")

(define-key claude-gravity-plan-review-mode-map (kbd "C-c C-c") #'claude-gravity-plan-review-approve)

(define-key claude-gravity-plan-review-mode-map (kbd "C-c C-k") #'claude-gravity-plan-review-deny)

(define-key claude-gravity-plan-review-mode-map (kbd "C-c C-d") #'claude-gravity-plan-review-diff)

(define-key claude-gravity-plan-review-mode-map (kbd "C-c ;") #'claude-gravity-plan-review-comment)

(define-key claude-gravity-plan-review-mode-map (kbd "C-c ?") #'claude-gravity-plan-review-menu)

(define-key claude-gravity-plan-review-mode-map (kbd "C-c C-g") #'claude-gravity-plan-review-toggle-margins)

(define-key claude-gravity-plan-review-mode-map (kbd "m") #'maximize-window)


(transient-define-prefix claude-gravity-plan-review-menu ()
  "Plan review commands."
  ["Review"
   ("C-c C-c" "Approve plan" claude-gravity-plan-review-approve)
   ("C-c C-k" "Deny with feedback" claude-gravity-plan-review-deny)]
  ["Annotate"
   ("C-c ;" "Inline comment" claude-gravity-plan-review-comment)
   ("C-c C-d" "Show diff" claude-gravity-plan-review-diff)
   ("C-c C-g" "Toggle revision gutter" claude-gravity-plan-review-toggle-margins)
   ("m" "Maximize window" maximize-window)])


(define-minor-mode claude-gravity-plan-review-mode
  "Minor mode for reviewing Claude Code plans.
\\{claude-gravity-plan-review-mode-map}"
  :lighter " PlanReview[C-c ?:menu]"
  :keymap claude-gravity-plan-review-mode-map)


(defun claude-gravity--handle-ask-user-question (event-data proc session-id cwd)
  "Handle bidirectional AskUserQuestion from EVENT-DATA.
PROC is the socket to respond on.  SESSION-ID and CWD identify the session."
  ;; Fire normal PreToolUse handler to update session state
  (claude-gravity-handle-event "PreToolUse" session-id cwd event-data)
  ;; Defer interactive part to the command loop — completing-read
  ;; cannot run inside a process filter.
  (let ((tool-input (alist-get 'tool_input event-data))
        (tid (alist-get 'tool_use_id event-data)))
    (run-at-time 0 nil
                 #'claude-gravity--ask-user-question-prompt
                 tool-input proc session-id tid)))


(defun claude-gravity--ask-user-question-prompt (tool-input proc session-id tid)
  "Prompt user for AskUserQuestion answer via minibuffer.
TOOL-INPUT is the question data, PROC the socket, SESSION-ID the session,
TID the tool_use_id for updating prompts."
  (let* ((questions (alist-get 'questions tool-input))
         (first-q (and (vectorp questions) (> (length questions) 0) (aref questions 0)))
         (q-text (and first-q (alist-get 'question first-q)))
         (header (and first-q (alist-get 'header first-q)))
         (options (and first-q (alist-get 'options first-q)))
         (choices (when (vectorp options)
                    (cl-loop for i from 0 below (length options)
                             for opt = (aref options i)
                             for label = (alist-get 'label opt)
                             for desc = (alist-get 'description opt)
                             collect (if desc (format "%s -- %s" label desc) label))))
         (prompt-str (format "Claude asks [%s]: %s " (or header "?") (or q-text "?")))
         (answer (if choices
                     (completing-read prompt-str choices nil t)
                   (read-string prompt-str)))
         ;; Extract just the label (before " -- ")
         (answer-label (if (string-match "\\`\\(.+?\\) -- " answer)
                           (match-string 1 answer)
                         answer)))
    ;; Build deny response with the answer
    ;; Include top-level 'answer' for OpenCode bridge extraction
    (let ((response `((hookSpecificOutput
                       . ((hookEventName . "PreToolUse")
                          (permissionDecision . "deny")
                          (permissionDecisionReason
                           . ,(format "User answered from Emacs: %s\nQuestion: %s\nAnswer: %s"
                                      answer-label (or q-text "") answer-label))))
                      (answer . ,answer-label))))
      (claude-gravity--send-bidirectional-response proc response))
    ;; Update the prompt entry with the answer
    (let ((session (claude-gravity--get-session session-id)))
      (when (and session tid)
        (claude-gravity-model-update-prompt-answer session tid answer-label)))
    (claude-gravity--log 'debug "Answered: %s" answer-label)))


;;; Tool Permission Requests

(defun claude-gravity--handle-tool-permission (event-data proc session-id cwd)
  "Handle a tool permission request from Claude Code.
EVENT-DATA is the PermissionRequest payload, PROC the socket to respond on,
SESSION-ID and CWD identify the session."
  ;; Record the tool in session state for UI display
  (claude-gravity-handle-event "PreToolUse" session-id cwd event-data)
  ;; Defer interactive prompt out of process filter
  (run-at-time 0 nil
               #'claude-gravity--tool-permission-prompt
               event-data proc session-id cwd))


(defun claude-gravity--tool-permission-prompt (event-data proc session-id cwd)
  "Prompt user to approve/deny a tool permission request.
EVENT-DATA is the PermissionRequest payload, PROC the socket,
SESSION-ID and CWD identify the session."
  (let* ((tool-name (alist-get 'tool_name event-data))
         (tool-input (alist-get 'tool_input event-data))
         (signature (claude-gravity--tool-signature tool-name tool-input))
         (choices '("Allow" "Allow always" "Deny"))
         (answer (completing-read
                  (format "Claude permission [%s]: " signature)
                  choices nil t)))
    (pcase answer
      ("Allow"
       (claude-gravity--send-permission-response proc "allow"))
      ("Allow always"
       (claude-gravity--send-permission-response proc "allow")
       (claude-gravity--write-allow-pattern-for-tool tool-name tool-input session-id cwd))
      ("Deny"
       (let ((reason (read-string "Reason (optional): ")))
         (if (string-empty-p reason)
             (claude-gravity--send-permission-response proc "deny")
           (claude-gravity--send-permission-response proc "deny" reason)))))))


(defun claude-gravity--send-permission-response (proc behavior &optional message permissions)
  "Send allow/deny response for a PermissionRequest.
PROC is the socket, BEHAVIOR is \"allow\" or \"deny\", MESSAGE is optional reason.
PERMISSIONS is an optional vector of permission rules for updatedPermissions."
  (let* ((decision `((behavior . ,behavior)
                     ,@(when message `((message . ,message)))
                     ,@(when permissions `((updatedPermissions . ,permissions)))))
         (response `((hookSpecificOutput
                      . ((hookEventName . "PermissionRequest")
                         (decision . ,decision))))))
    (claude-gravity--send-bidirectional-response proc response)))


(defun claude-gravity--write-allow-pattern-for-tool (tool-name tool-input session-id cwd)
  "Write an allow pattern for TOOL-NAME/TOOL-INPUT to settings.local.json.
SESSION-ID and CWD identify the session project."
  (let* ((suggestions (claude-gravity--suggest-patterns tool-name tool-input))
         (chosen (car suggestions))
         (settings-path (expand-file-name ".claude/settings.local.json" cwd)))
    (when chosen
      (let* ((data (if (file-exists-p settings-path)
                       (claude-gravity--json-read-file settings-path)
                     (list (cons 'permissions (list (cons 'allow nil))))))
             (perms (or (alist-get 'permissions data)
                        (list (cons 'allow nil))))
             (allow (or (alist-get 'allow perms) nil)))
        (if (member chosen allow)
            (claude-gravity--log 'debug "Pattern already exists: %s" chosen)
          (setf (alist-get 'allow perms) (append allow (list chosen)))
          (setf (alist-get 'permissions data) perms)
          (let ((dir (file-name-directory settings-path)))
            (unless (file-exists-p dir)
              (make-directory dir t)))
          (with-temp-file settings-path
            (let ((json-encoding-pretty-print t))
              (insert (json-encode data))))
          (let ((session (claude-gravity--get-session session-id)))
            (when session
              (claude-gravity--load-allow-patterns session)))
          (claude-gravity--log 'debug "Added allow pattern: %s" chosen))))))


(defun claude-gravity--handle-plan-review (event-data proc session-id)
  "Open a plan review buffer for EVENT-DATA.
PROC is the socket connection to respond on.
SESSION-ID identifies the Claude Code session."
  (let* ((tool-input (alist-get 'tool_input event-data))
         (plan-content (or (alist-get 'planContent tool-input)
                           ;; ExitPlanMode may have allowedPrompts but plan
                           ;; content could be in different fields
                           (alist-get 'plan tool-input)
                           ;; Fallback: stringify the whole tool_input
                           (json-encode tool-input)))
         (allowed-prompts (alist-get 'allowedPrompts tool-input))
         (session (claude-gravity--get-session session-id))
         (label (if session
                    (claude-gravity--session-label session)
                  (or session-id "unknown")))
         (buf-name (format "*Claude Plan Review: %s*" label))
         (buf (get-buffer-create buf-name)))
    (with-current-buffer buf
      (erase-buffer)
      ;; Insert plan content
      (insert plan-content)
      (insert "\n")
      ;; Insert allowed prompts section if present
      (when (and allowed-prompts (> (length allowed-prompts) 0))
        (insert "\n## Allowed Prompts\n\n")
        (seq-doseq (p allowed-prompts)
          (let ((tool (alist-get 'tool p))
                (prompt (alist-get 'prompt p)))
            (insert (format "- **%s**: %s\n" (or tool "?") (or prompt "?"))))))
      ;; Set up modes
      (when (fboundp 'markdown-mode)
        (markdown-mode))
      (claude-gravity-plan-review-mode 1)
      ;; Store buffer-local state
      (setq-local claude-gravity--plan-review-proc proc)
      (setq-local claude-gravity--plan-review-original plan-content)
      (setq-local claude-gravity--plan-review-session-id session-id)
      ;; Compute and apply revision diff margin indicators asynchronously.
      ;; Use :last-reviewed-plan (set on every review open, not just approval)
      ;; so deny-revise-resubmit flow also shows diffs.
      (let ((prev-content (when session
                            (or (plist-get session :last-reviewed-plan)
                                (plist-get (plist-get session :plan) :content)))))
        (when prev-content
          (claude-gravity--plan-revision-diff-async prev-content plan-content
            (lambda (diff-data)
              (when (and diff-data (buffer-live-p buf))
                (with-current-buffer buf
                  (claude-gravity--plan-review-apply-margin-indicators diff-data)
                  (setq-local claude-gravity--plan-review-prev-content prev-content))))))
        ;; Always store current plan for next review's diff
        (when session
          (plist-put session :last-reviewed-plan plan-content)))
      ;; Kill hook to handle buffer closed without decision
      (add-hook 'kill-buffer-hook #'claude-gravity--plan-review-on-kill nil t)
      (goto-char (point-min))
      (set-buffer-modified-p nil))
    ;; Display in current window
    (switch-to-buffer buf)
    (if claude-gravity--plan-review-prev-content
        (claude-gravity--log 'debug "Plan REVISION: gutter shows changes | C-c C-c approve | C-c C-k deny | C-c C-g toggle")
      (claude-gravity--log 'debug "Plan review: C-c C-c approve | C-c C-k deny | c comment | C-c C-d diff"))))


(defun claude-gravity--send-bidirectional-response (proc response)
  "Send RESPONSE JSON to the bridge via socket PROC, then close."
  (if (and proc (process-live-p proc))
      (progn
        (process-send-string proc (concat (json-encode response) "\n"))
        (run-at-time 0.1 nil (lambda (p) (when (process-live-p p) (delete-process p))) proc))
    (claude-gravity--log 'error "Claude Gravity: bridge connection lost. Response not sent.")))


(defun claude-gravity--plan-review-send-response (response)
  "Send RESPONSE JSON to the bridge via the stored socket proc."
  (claude-gravity--send-bidirectional-response claude-gravity--plan-review-proc response)
  (setq-local claude-gravity--plan-review-proc nil))


(defun claude-gravity--plan-review-compute-diff ()
  "Compute a unified diff between original and current buffer content.
Returns the diff string or nil if no changes."
  (let ((original claude-gravity--plan-review-original)
        (current (buffer-substring-no-properties (point-min) (point-max))))
    ;; Trim trailing whitespace for comparison
    (setq current (string-trim-right current))
    (setq original (string-trim-right original))
    (if (string= original current)
        nil
      (let ((orig-file (make-temp-file "plan-orig"))
            (curr-file (make-temp-file "plan-curr")))
        (unwind-protect
            (progn
              (with-temp-file orig-file (insert original))
              (with-temp-file curr-file (insert current))
              (with-temp-buffer
                (call-process "diff" nil t nil "-u" orig-file curr-file)
                (buffer-string)))
          (delete-file orig-file)
          (delete-file curr-file))))))


(defun claude-gravity-plan-review-comment ()
  "Add an inline comment on the current line in the plan review buffer."
  (interactive)
  (let* ((text (read-string "Comment: "))
         (line-num (line-number-at-pos))
         (line-text (string-trim
                     (buffer-substring-no-properties
                      (line-beginning-position) (line-end-position))))
         (beg (line-beginning-position))
         (end (line-end-position))
         (ov (make-overlay beg end)))
    (when (string-empty-p text)
      (delete-overlay ov)
      (user-error "Empty comment, cancelled"))
    (overlay-put ov 'face '(:underline (:style wave :color "orange")))
    (overlay-put ov 'help-echo text)
    (overlay-put ov 'after-string
                 (propertize (format "  « %s »" text)
                             'face '(:foreground "orange" :slant italic)))
    (overlay-put ov 'claude-plan-comment t)
    (push `((line . ,line-num)
            (text . ,text)
            (overlay . ,ov)
            (context . ,line-text))
          claude-gravity--plan-review-comments)
    (claude-gravity--log 'debug "Comment added on line %d" line-num)))


(defun claude-gravity--plan-review-collect-feedback ()
  "Collect inline comments from `claude-gravity--plan-review-comments'.
Returns a formatted markdown string or nil if no comments."
  (when claude-gravity--plan-review-comments
    (let ((sorted (sort (copy-sequence claude-gravity--plan-review-comments)
                        (lambda (a b)
                          (< (alist-get 'line a) (alist-get 'line b))))))
      (concat "## Inline comments:\n"
              (mapconcat
               (lambda (entry)
                 (let ((line (alist-get 'line entry))
                       (text (alist-get 'text entry))
                       (ctx (alist-get 'context entry)))
                   (format "- Line %d (near \"%s\"): \"%s\""
                           line
                           (if (> (length ctx) 60)
                               (concat (substring ctx 0 57) "...")
                             ctx)
                           text)))
               sorted "\n")
              "\n"))))


(defun claude-gravity--plan-review-scan-markers ()
  "Scan the buffer for @claude text markers.
Returns a formatted markdown string or nil if none found."
  (save-excursion
    (goto-char (point-min))
    (let ((markers nil))
      (while (re-search-forward "^.*@[Cc]laude:?\\s-*\\(.+\\)" nil t)
        (let* ((marker-text (string-trim (match-string 1)))
               (line-num (line-number-at-pos (match-beginning 0)))
               ;; Get surrounding context: previous non-empty line
               (ctx (save-excursion
                      (forward-line -1)
                      (while (and (not (bobp))
                                  (looking-at-p "^\\s-*$"))
                        (forward-line -1))
                      (string-trim
                       (buffer-substring-no-properties
                        (line-beginning-position) (line-end-position))))))
          (push `((line . ,line-num)
                  (text . ,marker-text)
                  (context . ,ctx))
                markers)))
      (when markers
        (let ((sorted (sort (nreverse markers)
                            (lambda (a b)
                              (< (alist-get 'line a) (alist-get 'line b))))))
          (concat "## @claude markers:\n"
                  (mapconcat
                   (lambda (entry)
                     (let ((line (alist-get 'line entry))
                           (text (alist-get 'text entry))
                           (ctx (alist-get 'context entry)))
                       (format "- Line %d (near \"%s\"): \"%s\""
                               line
                               (if (> (length ctx) 60)
                                   (concat (substring ctx 0 57) "...")
                                 ctx)
                               text)))
                   sorted "\n")
                  "\n"))))))


(defun claude-gravity--plan-review-build-feedback-message (diff comments markers general-comment)
  "Build a structured feedback message from DIFF, COMMENTS, MARKERS, and GENERAL-COMMENT.
Returns the formatted markdown string.  Omits empty sections."
  (let ((parts (list "# Plan Feedback\n")))
    (when comments
      (push (concat comments "\n") parts))
    (when markers
      (push (concat markers "\n") parts))
    (when diff
      (push (format "## Changes requested:\n```diff\n%s\n```\n" diff) parts))
    (when (and general-comment (not (string-empty-p general-comment)))
      (push (format "## General comment:\n%s\n" general-comment) parts))
    (when (and (not diff) (not comments) (not markers)
               (or (not general-comment) (string-empty-p general-comment)))
      (push "Plan was rejected without specific feedback.\n" parts))
    (string-join (nreverse parts) "\n")))


(defun claude-gravity--plan-review-cleanup-and-close ()
  "Remove inbox item, kill-buffer hook, and close the plan review buffer."
  (when claude-gravity--plan-review-inbox-id
    (claude-gravity--inbox-remove claude-gravity--plan-review-inbox-id))
  (remove-hook 'kill-buffer-hook #'claude-gravity--plan-review-on-kill t)
  (let ((buf (current-buffer)))
    (quit-window)
    (kill-buffer buf)))


(defun claude-gravity--enable-session-follow-mode (session-id)
  "Enable follow mode on the session buffer for SESSION-ID.
Does nothing if the buffer doesn't exist or follow mode is already on."
  (let* ((session (claude-gravity--get-session session-id))
         (buf (when session
                (or (let ((b (plist-get session :buffer)))
                      (and b (buffer-live-p b) b))
                    (get-buffer (claude-gravity--session-buffer-name session))))))
    (when (and buf (not (buffer-local-value 'claude-gravity--follow-mode buf)))
      (with-current-buffer buf
        (setq claude-gravity--follow-mode t)
        (add-hook 'post-command-hook #'claude-gravity--follow-detect-manual nil t)
        (force-mode-line-update)))))


(defun claude-gravity-plan-review-approve ()
  "Approve the plan and send allow decision to Claude Code.
If the user has made edits, added inline comments, or inserted
@claude markers, automatically deny instead so Claude can
incorporate the feedback (the allow channel cannot carry feedback)."
  (interactive)
  (let ((diff (claude-gravity--plan-review-compute-diff))
        (comments (claude-gravity--plan-review-collect-feedback))
        (markers (claude-gravity--plan-review-scan-markers))
        (session-id claude-gravity--plan-review-session-id))
    (if (or diff comments markers)
        ;; Feedback exists — deny so Claude sees it
        (let ((deny-message (claude-gravity--plan-review-build-feedback-message
                             diff comments markers
                             "I edited/annotated the plan. Please update your plan to match these changes and call ExitPlanMode again.")))
          (let ((response `((hookSpecificOutput
                             . ((hookEventName . "PermissionRequest")
                                (decision . ((behavior . "deny")
                                             (message . ,deny-message))))))))
            (claude-gravity--plan-review-send-response response))
          (claude-gravity--plan-review-cleanup-and-close)
          (claude-gravity--enable-session-follow-mode session-id)
          (claude-gravity--log 'debug "Feedback detected — denied for revision"))
      ;; No feedback — clean approve
      (let ((response `((hookSpecificOutput
                         . ((hookEventName . "PermissionRequest")
                            (decision . ((behavior . "allow"))))))))
        (claude-gravity--plan-review-send-response response))
      (claude-gravity--plan-review-cleanup-and-close)
      (claude-gravity--enable-session-follow-mode session-id)
      (claude-gravity--log 'debug "Plan approved"))))


(defun claude-gravity-plan-review-deny ()
  "Deny the plan and send feedback to Claude Code.
Collects inline comments, @claude markers, diff, and a general comment."
  (interactive)
  (let* ((diff (claude-gravity--plan-review-compute-diff))
         (comments (claude-gravity--plan-review-collect-feedback))
         (markers (claude-gravity--plan-review-scan-markers))
         (general-comment (read-string "Feedback comment (optional): "))
         (deny-message (claude-gravity--plan-review-build-feedback-message
                        diff comments markers general-comment)))
    (let ((response `((hookSpecificOutput
                       . ((hookEventName . "PermissionRequest")
                          (decision . ((behavior . "deny")
                                       (message . ,deny-message))))))))
      (claude-gravity--plan-review-send-response response))
    (claude-gravity--plan-review-cleanup-and-close)
    (claude-gravity--log 'debug "Plan denied with feedback")))


(defun claude-gravity-plan-review-diff ()
  "Show a diff between the original plan and current edits."
  (interactive)
  (let ((diff (claude-gravity--plan-review-compute-diff)))
    (if diff
        (let ((buf (get-buffer-create "*Claude Plan Diff*")))
          (with-current-buffer buf
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert diff))
            (diff-mode)
            (goto-char (point-min))
            (setq buffer-read-only t))
          (display-buffer buf))
      (claude-gravity--log 'debug "No changes from original plan"))))


(defun claude-gravity--plan-review-on-kill ()
  "Handle plan review buffer being killed without explicit decision.
Sends a deny response and cleans up inbox item."
  (when claude-gravity--plan-review-inbox-id
    (claude-gravity--inbox-remove claude-gravity--plan-review-inbox-id))
  (when claude-gravity--plan-review-proc
    (let ((response `((hookSpecificOutput
                       . ((hookEventName . "PermissionRequest")
                          (decision . ((behavior . "deny")
                                       (message . "Plan review cancelled (buffer closed)"))))))))
      (claude-gravity--plan-review-send-response response))))


(defun claude-gravity-test-plan-review ()
  "Open a simulated plan review buffer for testing.
No socket proc is attached — approve/deny will just message."
  (interactive)
  (claude-gravity--handle-plan-review
   '((tool_input . ((planContent . "## Test Plan\n\n1. Step one\n2. Step two\n3. Step three\n")
                     (allowedPrompts . [((tool . "Bash") (prompt . "npm test"))]))))
   nil
   "test-session"))

(provide 'claude-gravity-socket)
;;; claude-gravity-socket.el ends here