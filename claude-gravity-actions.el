;;; claude-gravity-actions.el --- Action buffers for Claude Gravity  -*- lexical-binding: t; -*-

;;; Code:

(require 'claude-gravity-core)
(require 'claude-gravity-faces)
(require 'claude-gravity-session)
(require 'claude-gravity-state)
(require 'claude-gravity-socket)
(require 'claude-gravity-text)


;;; ============================================================================
;;; Inbox Action Buffers
;;; ============================================================================

;; --- Permission Action Buffer ---

(defvar claude-gravity-permission-action-mode-map (make-sparse-keymap)
  "Keymap for `claude-gravity-permission-action-mode'.")

(define-key claude-gravity-permission-action-mode-map (kbd "a") #'claude-gravity-permission-action-allow)

(define-key claude-gravity-permission-action-mode-map (kbd "A") #'claude-gravity-permission-action-allow-always)

(define-key claude-gravity-permission-action-mode-map (kbd "S") #'claude-gravity-permission-action-allow-with-permissions)

(define-key claude-gravity-permission-action-mode-map (kbd "d") #'claude-gravity-permission-action-deny)

(define-key claude-gravity-permission-action-mode-map (kbd "p") #'claude-gravity-permission-action-add-pattern)

(define-key claude-gravity-permission-action-mode-map (kbd "q") #'claude-gravity-permission-action-quit)


(define-minor-mode claude-gravity-permission-action-mode
  "Minor mode for permission action buffers.
\\{claude-gravity-permission-action-mode-map}"
  :lighter " PermAction"
  :keymap claude-gravity-permission-action-mode-map)


(defvar-local claude-gravity--action-inbox-item nil
  "The inbox item associated with this action buffer.")


;;; Rich permission views for Edit/Write tools

(defun claude-gravity--file-context-around (file-path search-string &optional context-lines)
  "Return context lines from FILE-PATH around where SEARCH-STRING appears.
Returns list of (LINE-NUM . LINE-TEXT) or nil if not found.
CONTEXT-LINES defaults to 3."
  (let ((ctx (or context-lines 3)))
    (when (and file-path (file-exists-p file-path)
               (not (string-match-p "[\x00-\x08]"
                                    (with-temp-buffer
                                      (insert-file-contents file-path nil 0 256)
                                      (buffer-string)))))
      (with-temp-buffer
        (insert-file-contents file-path)
        (goto-char (point-min))
        (when (search-forward search-string nil t)
          (let* ((match-start (match-beginning 0))
                 (match-line (line-number-at-pos match-start))
                 (match-end-line (line-number-at-pos (match-end 0)))
                 (start-line (max 1 (- match-line ctx)))
                 (end-line (+ match-end-line ctx))
                 (result nil))
            (goto-char (point-min))
            (forward-line (1- start-line))
            (let ((ln start-line))
              (while (and (<= ln end-line) (not (eobp)))
                (push (cons ln (buffer-substring-no-properties
                                (line-beginning-position) (line-end-position)))
                      result)
                (setq ln (1+ ln))
                (forward-line 1)))
            (nreverse result)))))))


(defun claude-gravity--permission-insert-edit-view (tool-input)
  "Insert rich Edit tool view: file path, context, word-level diff."
  (let ((file-path (alist-get 'file_path tool-input))
        (old-str (alist-get 'old_string tool-input))
        (new-str (alist-get 'new_string tool-input)))
    ;; File path
    (when file-path
      (insert (propertize "File: " 'face 'claude-gravity-detail-label)
              (propertize (abbreviate-file-name file-path)
                          'face 'claude-gravity-tool-name)
              "\n\n"))
    ;; File context around old_string
    (when old-str
      (let ((context (claude-gravity--file-context-around file-path old-str)))
        (when context
          (insert (propertize "Context:\n" 'face 'claude-gravity-detail-label))
          (dolist (entry context)
            (insert (propertize (format "%4d " (car entry))
                                'face 'claude-gravity-detail-label)
                    (propertize (cdr entry)
                                'face 'claude-gravity-diff-context)
                    "\n"))
          (insert "\n"))))
    ;; Word-level diff
    (when (and new-str (stringp new-str) (not (string-empty-p new-str)))
      (claude-gravity--insert-inline-diff (or old-str "") new-str "Diff:")
      (insert "\n"))))


(defun claude-gravity--permission-insert-write-view (tool-input)
  "Insert rich Write tool view: file path, diff or content preview."
  (let ((file-path (alist-get 'file_path tool-input))
        (content (alist-get 'content tool-input)))
    ;; File path
    (when file-path
      (insert (propertize "File: " 'face 'claude-gravity-detail-label)
              (propertize (abbreviate-file-name file-path)
                          'face 'claude-gravity-tool-name)
              "\n\n"))
    (cond
     ;; File exists: diff existing vs new content
     ((and file-path (file-exists-p file-path) content)
      (let ((existing (with-temp-buffer
                        (insert-file-contents file-path)
                        (buffer-string))))
        (if (string-match-p "[\x00-\x08]" existing)
            (insert (propertize "(binary file — diff not available)\n\n"
                                'face 'claude-gravity-detail-label))
          (claude-gravity--insert-inline-diff existing content "Diff:")
          (insert "\n"))))
     ;; New file: show content preview
     (content
      (insert (propertize "New file content:\n" 'face 'claude-gravity-detail-label))
      (let* ((lines (split-string content "\n"))
             (max-lines claude-gravity-diff-max-lines)
             (show-lines (if (> (length lines) max-lines)
                             (seq-take lines max-lines)
                           lines)))
        (dolist (line show-lines)
          (insert "  " line "\n"))
        (when (> (length lines) max-lines)
          (insert (propertize (format "  ... (%d more lines)\n"
                                      (- (length lines) max-lines))
                              'face 'claude-gravity-detail-label)))
        (insert "\n")))
     ;; No content
     (t
      (insert (propertize "(no content)\n\n" 'face 'claude-gravity-detail-label))))))


(defun claude-gravity--inbox-act-permission (item)
  "Open a permission action buffer for inbox ITEM."
  (let* ((data (alist-get 'data item))
         (label (alist-get 'label item))
         (tool-name (alist-get 'tool_name data))
         (tool-input (alist-get 'tool_input data))
         (signature (claude-gravity--tool-signature tool-name tool-input))
         (buf (get-buffer-create "*Claude Action: Permission*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize (format "Permission Request — %s\n" label)
                            'face 'claude-gravity-header-title))
        (insert (make-string 60 ?─) "\n\n")
        (insert (propertize "Tool: " 'face 'claude-gravity-detail-label)
                (propertize signature 'face 'claude-gravity-tool-name) "\n\n")
        ;; Show tool input details
        (when tool-input
          (cond
           ((equal tool-name "Edit")
            (claude-gravity--permission-insert-edit-view tool-input))
           ((equal tool-name "Write")
            (claude-gravity--permission-insert-write-view tool-input))
           (t
            (insert (propertize "Input:\n" 'face 'claude-gravity-detail-label))
            (let ((json-encoding-pretty-print t))
              (insert (json-encode tool-input) "\n\n")))))
        (insert (make-string 60 ?─) "\n")
        (insert (propertize "  a" 'face 'claude-gravity-tool-name) " Allow  "
                (propertize "  A" 'face 'claude-gravity-tool-name) " Allow always  "
                (propertize "  S" 'face 'claude-gravity-tool-name) " Session allow  "
                (propertize "  d" 'face 'claude-gravity-tool-name) " Deny  "
                (propertize "  p" 'face 'claude-gravity-tool-name) " Add pattern  "
                (propertize "  q" 'face 'claude-gravity-tool-name) " Close\n"))
      (setq buffer-read-only t)
      (goto-char (point-min))
      (claude-gravity-permission-action-mode 1)
      (setq-local claude-gravity--action-inbox-item item))
    (display-buffer-in-side-window buf '((side . bottom) (window-height . 0.35)))
    (select-window (get-buffer-window buf))))


(defun claude-gravity--permission-action-finish ()
  "Clean up after a permission action: remove inbox item, kill buffer."
  (let ((item claude-gravity--action-inbox-item))
    (when item
      (claude-gravity--inbox-remove (alist-get 'id item))))
  (let ((buf (current-buffer)))
    (quit-window)
    (kill-buffer buf)))


(defun claude-gravity-permission-action-allow ()
  "Allow the permission request."
  (interactive)
  (let* ((item claude-gravity--action-inbox-item)
         (proc (alist-get 'socket-proc item)))
    (claude-gravity--send-permission-response proc "allow")
    (claude-gravity--permission-action-finish)
    (claude-gravity--log 'debug "Permission allowed")))


(defun claude-gravity-permission-action-allow-always ()
  "Allow the permission and write an allow pattern."
  (interactive)
  (let* ((item claude-gravity--action-inbox-item)
         (data (alist-get 'data item))
         (proc (alist-get 'socket-proc item))
         (tool-name (alist-get 'tool_name data))
         (tool-input (alist-get 'tool_input data))
         (session-id (alist-get 'session-id item))
         (session (claude-gravity--get-session session-id))
         (cwd (when session (plist-get session :cwd))))
    (claude-gravity--send-permission-response proc "allow")
    (when cwd
      (claude-gravity--write-allow-pattern-for-tool tool-name tool-input session-id cwd))
    (claude-gravity--permission-action-finish)
    (claude-gravity--log 'debug "Permission allowed + pattern written")))


(defun claude-gravity-permission-action-allow-with-permissions ()
  "Allow and apply a session-scoped permission rule from suggestions."
  (interactive)
  (let* ((item claude-gravity--action-inbox-item)
         (data (alist-get 'data item))
         (proc (alist-get 'socket-proc item))
         (suggestions (alist-get 'permission_suggestions data)))
    (if (not suggestions)
        (progn
          (claude-gravity--send-permission-response proc "allow")
          (claude-gravity--log 'debug "No permission suggestions available, allowed without permissions"))
      (let ((chosen (elt suggestions 0)))
        (claude-gravity--send-permission-response proc "allow" nil (vector chosen))
        (let ((type (alist-get 'type chosen))
              (tool (alist-get 'tool chosen)))
          (claude-gravity--log 'debug "Permission allowed + session rule: %s: %s" type (or tool "all")))))
    (claude-gravity--permission-action-finish)))


(defun claude-gravity-permission-action-deny ()
  "Deny the permission request."
  (interactive)
  (let* ((item claude-gravity--action-inbox-item)
         (proc (alist-get 'socket-proc item))
         (reason (read-string "Reason (optional): ")))
    (if (string-empty-p reason)
        (claude-gravity--send-permission-response proc "deny")
      (claude-gravity--send-permission-response proc "deny" reason))
    (claude-gravity--permission-action-finish)
    (claude-gravity--log 'debug "Permission denied")))


(defun claude-gravity-permission-action-add-pattern ()
  "Interactively select and add an allow pattern for the current tool.
Does not approve/deny — just writes the pattern to settings.local.json."
  (interactive)
  (let* ((item claude-gravity--action-inbox-item)
         (data (alist-get 'data item))
         (tool-name (alist-get 'tool_name data))
         (tool-input (alist-get 'tool_input data))
         (session-id (alist-get 'session-id item))
         (session (claude-gravity--get-session session-id))
         (cwd (when session (plist-get session :cwd)))
         (suggestions (claude-gravity--suggest-patterns tool-name tool-input))
         (chosen (completing-read "Allow pattern: " suggestions nil nil
                                  (car suggestions))))
    (when (and chosen cwd)
      (let* ((settings-path (expand-file-name ".claude/settings.local.json" cwd))
             (file-data (if (file-exists-p settings-path)
                            (claude-gravity--json-read-file settings-path)
                          (list (cons 'permissions (list (cons 'allow nil))))))
             (perms (or (alist-get 'permissions file-data)
                        (list (cons 'allow nil))))
             (allow (or (alist-get 'allow perms) nil)))
        (if (member chosen allow)
            (claude-gravity--log 'debug "Pattern already exists: %s" chosen)
          (setf (alist-get 'allow perms) (append allow (list chosen)))
          (setf (alist-get 'permissions file-data) perms)
          (let ((dir (file-name-directory settings-path)))
            (unless (file-exists-p dir)
              (make-directory dir t)))
          (with-temp-file settings-path
            (let ((json-encoding-pretty-print t))
              (insert (json-encode file-data))))
          (when session
            (claude-gravity--load-allow-patterns session))
          (claude-gravity--log 'debug "Added allow pattern: %s" chosen))))))


(defun claude-gravity-permission-action-quit ()
  "Close action buffer without responding."
  (interactive)
  (let ((buf (current-buffer)))
    (quit-window)
    (kill-buffer buf)))


;; --- Question Action Buffer ---

(defvar claude-gravity-question-action-mode-map (make-sparse-keymap)
  "Keymap for `claude-gravity-question-action-mode'.")

(define-key claude-gravity-question-action-mode-map (kbd "1") #'claude-gravity-question-action-1)

(define-key claude-gravity-question-action-mode-map (kbd "2") #'claude-gravity-question-action-2)

(define-key claude-gravity-question-action-mode-map (kbd "3") #'claude-gravity-question-action-3)

(define-key claude-gravity-question-action-mode-map (kbd "4") #'claude-gravity-question-action-4)

(define-key claude-gravity-question-action-mode-map (kbd "o") #'claude-gravity-question-action-other)

(define-key claude-gravity-question-action-mode-map (kbd "q") #'claude-gravity-question-action-quit)


(define-minor-mode claude-gravity-question-action-mode
  "Minor mode for question action buffers.
\\{claude-gravity-question-action-mode-map}"
  :lighter " QuestionAction"
  :keymap claude-gravity-question-action-mode-map)


(defvar-local claude-gravity--question-choices nil
  "List of (label . description) choices for the current question buffer.")


(defun claude-gravity--inbox-act-question (item)
  "Open a question action buffer for inbox ITEM."
  (let* ((data (alist-get 'data item))
         (label (alist-get 'label item))
         (tool-input (alist-get 'tool_input data))
         (questions (alist-get 'questions tool-input))
         (first-q (and (vectorp questions) (> (length questions) 0) (aref questions 0)))
         (q-text (and first-q (alist-get 'question first-q)))
         (header (and first-q (alist-get 'header first-q)))
         (options (and first-q (alist-get 'options first-q)))
         (choices (when (vectorp options)
                    (cl-loop for i from 0 below (length options)
                             for opt = (aref options i)
                             for opt-label = (alist-get 'label opt)
                             for desc = (alist-get 'description opt)
                             collect (cons opt-label desc))))
         (buf (get-buffer-create "*Claude Action: Question*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize (format "Question — %s\n" label)
                            'face 'claude-gravity-header-title))
        (insert (make-string 60 ?─) "\n\n")
        (when header
          (insert (propertize (format "[%s]\n" header) 'face 'claude-gravity-detail-label)))
        (insert (propertize (or q-text "?") 'face 'default) "\n\n")
        (when choices
          (cl-loop for i from 0
                   for (clabel . desc) in choices
                   do (insert (propertize (format "  %d" (1+ i)) 'face 'claude-gravity-tool-name)
                              " " clabel
                              (if desc (format "  (%s)" desc) "")
                              "\n")))
        (insert "\n" (make-string 60 ?─) "\n")
        (insert (propertize "  1-4" 'face 'claude-gravity-tool-name) " Select option  "
                (propertize "  o" 'face 'claude-gravity-tool-name) " Free text  "
                (propertize "  q" 'face 'claude-gravity-tool-name) " Close\n"))
      (setq buffer-read-only t)
      (goto-char (point-min))
      (claude-gravity-question-action-mode 1)
      (setq-local claude-gravity--action-inbox-item item)
      (setq-local claude-gravity--question-choices choices))
    (display-buffer-in-side-window buf '((side . bottom) (window-height . 0.35)))
    (select-window (get-buffer-window buf))))


(defun claude-gravity--question-action-respond (answer-label)
  "Send ANSWER-LABEL as the question response and clean up."
  (let* ((item claude-gravity--action-inbox-item)
         (proc (alist-get 'socket-proc item))
         (data (alist-get 'data item))
         (q-text (let* ((tool-input (alist-get 'tool_input data))
                        (questions (alist-get 'questions tool-input))
                        (first-q (and (vectorp questions) (> (length questions) 0) (aref questions 0))))
                   (and first-q (alist-get 'question first-q))))
         (tid (alist-get 'tool_use_id data))
         (session-id (alist-get 'session-id item)))
    ;; Send deny response with the answer
    (let ((response `((hookSpecificOutput
                       . ((hookEventName . "PreToolUse")
                          (permissionDecision . "deny")
                          (permissionDecisionReason
                           . ,(format "User answered from Emacs: %s\nQuestion: %s\nAnswer: %s"
                                      answer-label (or q-text "") answer-label)))))))
      (claude-gravity--send-bidirectional-response proc response))
    ;; Update prompt entry with answer
    (let ((session (claude-gravity--get-session session-id)))
      (when (and session tid)
        (claude-gravity-model-update-prompt-answer session tid answer-label)))
    ;; Clean up
    (claude-gravity--inbox-remove (alist-get 'id item))
    (let ((buf (current-buffer)))
      (quit-window)
      (kill-buffer buf))
    (claude-gravity--log 'debug "Answered: %s" answer-label)))


(defun claude-gravity--question-action-select (n)
  "Select option N (1-based) from the question choices."
  (let ((choices claude-gravity--question-choices))
    (if (and choices (>= n 1) (<= n (length choices)))
        (claude-gravity--question-action-respond (car (nth (1- n) choices)))
      (claude-gravity--log 'debug "No option %d" n))))


(defun claude-gravity-question-action-1 () "Select option 1." (interactive) (claude-gravity--question-action-select 1))

(defun claude-gravity-question-action-2 () "Select option 2." (interactive) (claude-gravity--question-action-select 2))

(defun claude-gravity-question-action-3 () "Select option 3." (interactive) (claude-gravity--question-action-select 3))

(defun claude-gravity-question-action-4 () "Select option 4." (interactive) (claude-gravity--question-action-select 4))


(defun claude-gravity-question-action-other ()
  "Enter free text answer."
  (interactive)
  (let ((answer (read-string "Your answer: ")))
    (unless (string-empty-p answer)
      (claude-gravity--question-action-respond answer))))


(defun claude-gravity-question-action-quit ()
  "Close question action buffer without responding."
  (interactive)
  (let ((buf (current-buffer)))
    (quit-window)
    (kill-buffer buf)))


;; --- Plan Review Action (reuse existing) ---

(defun claude-gravity--inbox-act-plan-review (item)
  "Open a plan review buffer for inbox ITEM.
Reuses existing `claude-gravity--handle-plan-review', modified to
remove the inbox item when plan review is acted on."
  (let ((data (alist-get 'data item))
        (proc (alist-get 'socket-proc item))
        (session-id (alist-get 'session-id item))
        (inbox-id (alist-get 'id item)))
    (claude-gravity--handle-plan-review data proc session-id)
    ;; Store inbox-id on the plan review buffer so approve/deny can clean up
    (let ((buf (get-buffer (format "*Claude Plan Review: %s*"
                                    (or (alist-get 'label item)
                                        session-id "unknown")))))
      (when buf
        (with-current-buffer buf
          (setq-local claude-gravity--plan-review-inbox-id inbox-id))))))


;; --- Idle Session Action ---

(defun claude-gravity--inbox-act-idle (item)
  "Open the session buffer for an idle inbox ITEM."
  (let ((session-id (alist-get 'session-id item)))
    (claude-gravity--inbox-remove (alist-get 'id item))
    (claude-gravity-open-session session-id)))

(provide 'claude-gravity-actions)
;;; claude-gravity-actions.el ends here