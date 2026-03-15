;;; claude-gravity-plan-review.el --- Plan review mode for Claude Gravity  -*- lexical-binding: t; -*-

;; Plan review functionality extracted from socket.el to be backend-agnostic.
;; Works with both legacy socket mode and new server mode.

;;; Code:

(require 'claude-gravity-core)
(require 'claude-gravity-faces)
(require 'claude-gravity-session)
(require 'claude-gravity-state)

;; Forward declarations — functions from other modules
(declare-function claude-gravity--display-buffer "claude-gravity-ui")
(declare-function claude-gravity--session-buffer-name "claude-gravity-ui")
(declare-function claude-gravity--follow-detect-manual "claude-gravity-ui")
(declare-function claude-gravity--plan-revision-diff-async "claude-gravity-diff")
(declare-function claude-gravity--plan-review-apply-margin-indicators "claude-gravity-diff")
(declare-function claude-gravity--send-plan-review-response "claude-gravity-client")
(declare-function claude-gravity-handle-event "claude-gravity-events")

;; Forward declarations — variables from other modules
(defvar claude-gravity--tmux-sessions)


(defvar-local claude-gravity--plan-review-original nil
  "Original plan content for computing diffs.")


(defvar-local claude-gravity--plan-review-session-id nil
  "Session ID associated with this plan review.")


(defvar-local claude-gravity--plan-review-comments nil
  "List of inline comments in the plan review buffer.
Each entry is an alist with keys: line, text, overlay, context.")


(defvar-local claude-gravity--plan-review-inbox-id nil
  "Inbox item ID associated with this plan review buffer, if any.")


(defvar-local claude-gravity--plan-review-session-permissions nil
  "Permission rules to send with plan approval via updatedPermissions.
Derived from ExitPlanMode's allowedPrompts — each unique tool name
becomes a `toolAlwaysAllow' entry so subsequent PermissionRequests
for those tools are auto-approved within the session.")


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

(define-key claude-gravity-plan-review-mode-map (kbd "C-c C-l") #'claude-gravity-plan-review-approve-and-clear)




(transient-define-prefix claude-gravity-plan-review-menu ()
  "Plan review commands."
  ["Review"
   ("C-c C-c" "Approve plan" claude-gravity-plan-review-approve)
   ("C-c C-l" "Approve + clear & proceed" claude-gravity-plan-review-approve-and-clear)
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


(defun claude-gravity--handle-plan-review (event-data session-id)
  "Open a plan review buffer for EVENT-DATA.
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
      ;; Align markdown pipe tables for readability
      (when (fboundp 'markdown-table-align)
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward "^\\s-*|" nil t)
            (beginning-of-line)
            (condition-case nil
                (markdown-table-align)
              (error nil))
            (forward-line 1))))
      ;; Store buffer-local state
      (setq-local claude-gravity--plan-review-original
                  (buffer-substring-no-properties (point-min) (point-max)))
      (setq-local claude-gravity--plan-review-session-id session-id)
      ;; Build session-scoped permissions from allowedPrompts tool names.
      ;; Each unique tool becomes a toolAlwaysAllow entry so subsequent
      ;; PermissionRequests for those tools are auto-approved.
      (when (and allowed-prompts (> (length allowed-prompts) 0))
        (let ((tools (delete-dups
                      (mapcar (lambda (p) (alist-get 'tool p))
                              (append allowed-prompts nil)))))
          (setq-local claude-gravity--plan-review-session-permissions
                      (vconcat
                       (mapcar (lambda (tool)
                                 `((type . "toolAlwaysAllow") (tool . ,tool)))
                               tools)))))
      ;; Compute and apply revision diff margin indicators asynchronously.
      ;; Only diff within the same planning turn — cross-turn diffs are noise.
      (let* ((current-turn (when session (length (plist-get session :turns))))
             (prev-turn (when session (plist-get session :last-reviewed-plan-turn)))
             (prev-content (when (and current-turn prev-turn
                                      (= prev-turn current-turn))
                             (plist-get session :last-reviewed-plan))))
        (when prev-content
          (claude-gravity--plan-revision-diff-async prev-content plan-content
            (lambda (diff-data)
              (when (and diff-data (buffer-live-p buf))
                (with-current-buffer buf
                  (claude-gravity--plan-review-apply-margin-indicators diff-data)
                  (setq-local claude-gravity--plan-review-prev-content prev-content))))))
        ;; Always store current plan and turn for next review's diff
        (when session
          (plist-put session :last-reviewed-plan plan-content)
          (plist-put session :last-reviewed-plan-turn current-turn)))
      ;; Kill hook to handle buffer closed without decision
      (add-hook 'kill-buffer-hook #'claude-gravity--plan-review-on-kill nil t)
      (goto-char (point-min))
      (set-buffer-modified-p nil))
    ;; Display in gravity window (or other window if no gravity window)
    (claude-gravity--display-buffer buf)
    (if claude-gravity--plan-review-prev-content
        (claude-gravity--log 'debug "Plan REVISION: gutter shows changes | C-c C-c approve | C-c C-k deny | C-c C-g toggle")
      (claude-gravity--log 'debug "Plan review: C-c C-c approve | C-c C-k deny | c comment | C-c C-d diff"))))


(defun claude-gravity--plan-review-send-response (response)
  "Send RESPONSE for the plan review via gravity-server."
  (if claude-gravity--plan-review-inbox-id
      (let* ((hook (alist-get 'hookSpecificOutput response))
             (decision (alist-get 'decision hook))
             (behavior (alist-get 'behavior decision))
             (message (alist-get 'message decision)))
        (claude-gravity--send-plan-review-response
         claude-gravity--plan-review-inbox-id behavior message))
    (claude-gravity--log 'warn "Plan review: no inbox-id for response")))


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
                 (propertize (format "  \u00ab %s \u00bb" text)
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
      ;; No feedback — clean approve with session-scoped permissions.
      ;; NOTE: We send {"behavior":"allow"} here. The gravity-server handles
      ;; the deny-as-approve workaround for Claude Code bug #15755.
      (let* ((perms claude-gravity--plan-review-session-permissions)
             (decision (if (and perms (> (length perms) 0))
                           `((behavior . "allow")
                             (updatedPermissions . ,perms))
                         `((behavior . "allow"))))
             (response `((hookSpecificOutput
                          . ((hookEventName . "PermissionRequest")
                             (decision . ,decision))))))
        (claude-gravity--plan-review-send-response response))
      (claude-gravity--plan-review-cleanup-and-close)
      (claude-gravity--enable-session-follow-mode session-id)
      (claude-gravity--log 'debug "Plan approved%s"
                           (if claude-gravity--plan-review-session-permissions
                               (format " + %d session permissions"
                                       (length claude-gravity--plan-review-session-permissions))
                             "")))))


(defun claude-gravity-plan-review-approve-and-clear ()
  "Approve the plan, then clear context and re-prompt with plan reference.
Sends allow (with permissions), persists allowedPrompts to settings.local.json
so they survive the session boundary, sets :awaiting-clear on the session,
then waits for the Stop event to trigger /clear via tmux.

After /clear re-keys the session (preserving all turns/tools), auto-sends
an implementation prompt referencing the plan file."
  (interactive)
  (let* ((session-id claude-gravity--plan-review-session-id)
         (session (claude-gravity--get-session session-id))
         (perms claude-gravity--plan-review-session-permissions))
    (unless session
      (user-error "No session found for %s" session-id))
    (unless (gethash session-id claude-gravity--tmux-sessions)
      (user-error "Clear-and-proceed requires a tmux session"))
    ;; 1. Send allow response (with session-scoped permissions)
    (let* ((decision (if (and perms (> (length perms) 0))
                         `((behavior . "allow")
                           (updatedPermissions . ,perms))
                       `((behavior . "allow"))))
           (response `((hookSpecificOutput
                        . ((hookEventName . "PermissionRequest")
                           (decision . ,decision))))))
      (claude-gravity--plan-review-send-response response))
    ;; 2. Persist allowedPrompts to settings.local.json (survives /clear)
    (let ((cwd (plist-get session :cwd)))
      (when cwd
        (claude-gravity--persist-allowed-prompts
         claude-gravity--plan-review-original session-id cwd)))
    ;; 3. Store plan file path and set awaiting-clear flag
    (let ((plan (plist-get session :plan)))
      (plist-put session :clear-plan-path
                 (when plan (plist-get plan :file-path))))
    (plist-put session :awaiting-clear t)
    ;; 4. Timeout safety: cancel after 2 minutes if /clear never completes
    (plist-put session :clear-timeout
               (run-at-time 120 nil
                 (lambda ()
                   (when (plist-get session :awaiting-clear)
                     (plist-put session :awaiting-clear nil)
                     (plist-put session :clear-timeout nil)
                     (claude-gravity--log 'warn
                       "Clear-and-proceed timed out for %s" session-id)))))
    ;; 5. Cleanup
    (claude-gravity--plan-review-cleanup-and-close)
    (claude-gravity--enable-session-follow-mode session-id)
    (claude-gravity--log 'info
      "Plan approved + clear-and-proceed armed for %s" session-id)))


(defun claude-gravity--persist-allowed-prompts (plan-content session-id cwd)
  "Persist plan's allowedPrompts as allow patterns in settings.local.json.
PLAN-CONTENT is unused (for signature consistency).
SESSION-ID and CWD locate the project.  Reads allowedPrompts from
the session's :plan plist."
  (ignore plan-content)
  (let* ((session (claude-gravity--get-session session-id))
         (plan (when session (plist-get session :plan)))
         (prompts (when plan (plist-get plan :allowed-prompts)))
         (settings-path (expand-file-name ".claude/settings.local.json" cwd)))
    (when prompts
      (let* ((data (if (file-exists-p settings-path)
                       (claude-gravity--json-read-file settings-path)
                     (list (cons 'permissions (list (cons 'allow nil))))))
             (perms (or (alist-get 'permissions data)
                        (list (cons 'allow nil))))
             (allow (or (alist-get 'allow perms) nil))
             (added 0))
        (dolist (p prompts)
          (let* ((tool (alist-get 'tool p))
                 (prompt (alist-get 'prompt p))
                 (pattern (if prompt
                              (format "%s(%s)" (or tool "Bash") prompt)
                            (or tool "Bash"))))
            (unless (member pattern allow)
              (setq allow (append allow (list pattern)))
              (cl-incf added))))
        (when (> added 0)
          (setf (alist-get 'allow perms) allow)
          (setf (alist-get 'permissions data) perms)
          (let ((dir (file-name-directory settings-path)))
            (unless (file-exists-p dir) (make-directory dir t)))
          (with-temp-file settings-path
            (let ((json-encoding-pretty-print t))
              (insert (json-encode data))))
          (when session
            (claude-gravity--load-allow-patterns session))
          (claude-gravity--log 'info "Persisted %d allow patterns for clear-and-proceed"
                               added))))))


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
    (let ((response `((hookSpecificOutput
                       . ((hookEventName . "PermissionRequest")
                          (decision . ((behavior . "deny")
                                       (message . "Plan review cancelled (buffer closed)"))))))))
      (claude-gravity--plan-review-send-response response))
    (claude-gravity--inbox-remove claude-gravity--plan-review-inbox-id)))


(defun claude-gravity-test-plan-review ()
  "Open a simulated plan review buffer for testing.
No socket proc is attached — approve/deny will just message."
  (interactive)
  (claude-gravity--handle-plan-review
   '((tool_input . ((planContent . "## Test Plan\n\n1. Step one\n2. Step two\n3. Step three\n")
                     (allowedPrompts . [((tool . "Bash") (prompt . "npm test"))]))))
   "test-session"))

(provide 'claude-gravity-plan-review)
;;; claude-gravity-plan-review.el ends here
