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

(define-key claude-gravity-permission-action-mode-map (kbd "!") #'claude-gravity-permission-action-allow-turn)

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
         (item-id (alist-get 'id item))
         (tool-name (alist-get 'tool_name data))
         (tool-input (alist-get 'tool_input data))
         (signature (claude-gravity--tool-signature tool-name tool-input))
         (buf (get-buffer-create (format "*Claude Action: Permission #%d*" item-id))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize (format "Permission Request — %s\n" label)
                            'face 'claude-gravity-header-title))
        ;; Actions at top — immediately visible even with large tool bodies
        (insert (propertize "  a" 'face 'claude-gravity-tool-name) " Allow  "
                (propertize "  A" 'face 'claude-gravity-tool-name) " Allow always  "
                (propertize "  S" 'face 'claude-gravity-tool-name) " Session allow  "
                (propertize "  !" 'face 'claude-gravity-tool-name) " Allow turn  "
                (propertize "  d" 'face 'claude-gravity-tool-name) " Deny  "
                (propertize "  p" 'face 'claude-gravity-tool-name) " Add pattern  "
                (propertize "  q" 'face 'claude-gravity-tool-name) " Close\n")
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
              (insert (json-encode tool-input) "\n\n"))))))
      (setq buffer-read-only t)
      (goto-char (point-min))
      (claude-gravity-permission-action-mode 1)
      (setq-local claude-gravity--action-inbox-item item))
    (puthash item-id buf claude-gravity--inbox-action-buffers)
    (display-buffer-in-side-window buf '((side . bottom) (window-height . 0.35)))
    (select-window (get-buffer-window buf))))


(defun claude-gravity--permission-action-finish ()
  "Clean up after a permission action: remove inbox item, kill buffer."
  (let* ((item claude-gravity--action-inbox-item)
         (session-id (when item (alist-get 'session-id item))))
    (when item
      (remhash (alist-get 'id item) claude-gravity--inbox-action-buffers)
      (claude-gravity--inbox-remove (alist-get 'id item)))
    (let ((buf (current-buffer)))
      (quit-window)
      (kill-buffer buf))
    (when session-id
      (claude-gravity--inbox-pop-next session-id))))


(defun claude-gravity--inbox-pop-next (session-id)
  "Auto-open the next pending actionable inbox item for SESSION-ID.
Called after finishing a permission or question action."
  (let ((next (cl-find-if
               (lambda (item)
                 (and (equal (alist-get 'session-id item) session-id)
                      (memq (alist-get 'type item) '(permission question))))
               claude-gravity--inbox)))
    (when next
      (run-at-time 0 nil
                   (lambda (it)
                     (pcase (alist-get 'type it)
                       ('permission (claude-gravity--inbox-act-permission it))
                       ('question (claude-gravity--inbox-act-question it))))
                   next))))


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


(defun claude-gravity-permission-action-allow-turn ()
  "Allow this and all remaining permission requests for the current turn.
Approves all currently pending permissions for the same session, then sets
a turn-scoped auto-approve flag so any new permissions arriving during this
turn are also automatically approved.  The flag is cleared on turn boundaries
\(Stop, UserPromptSubmit, SessionEnd)."
  (interactive)
  (let* ((item claude-gravity--action-inbox-item)
         (session-id (alist-get 'session-id item))
         (proc (alist-get 'socket-proc item))
         (session (claude-gravity--get-session session-id))
         (turn (when session (plist-get session :current-turn))))
    ;; Approve current item
    (claude-gravity--send-permission-response proc "allow")
    ;; Approve all other pending permissions for this session
    (let ((others (cl-remove-if-not
                   (lambda (it)
                     (and (equal (alist-get 'session-id it) session-id)
                          (eq (alist-get 'type it) 'permission)
                          (not (eq (alist-get 'id it) (alist-get 'id item)))))
                   claude-gravity--inbox)))
      (dolist (other others)
        (let ((p (alist-get 'socket-proc other)))
          (when (and p (process-live-p p))
            (claude-gravity--send-permission-response p "allow")))
        (remhash (alist-get 'id other) claude-gravity--inbox-action-buffers)
        (claude-gravity--inbox-remove (alist-get 'id other))))
    ;; Set turn-scoped auto-approve flag
    (when turn
      (setq claude-gravity--turn-auto-approve
            (cons (cons session-id turn)
                  (assoc-delete-all session-id claude-gravity--turn-auto-approve))))
    ;; Clean up current item (don't pop-next since we approved them all)
    (when item
      (remhash (alist-get 'id item) claude-gravity--inbox-action-buffers)
      (claude-gravity--inbox-remove (alist-get 'id item)))
    (let ((buf (current-buffer)))
      (quit-window)
      (kill-buffer buf))
    (claude-gravity--log 'debug "Turn auto-approve set for session %s turn %s"
                         session-id turn)))


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
(define-key claude-gravity-question-action-mode-map (kbd "]") #'claude-gravity-question-next-tab)
(define-key claude-gravity-question-action-mode-map (kbd "[") #'claude-gravity-question-prev-tab)
(define-key claude-gravity-question-action-mode-map (kbd "<tab>") #'claude-gravity-question-next-tab)
(define-key claude-gravity-question-action-mode-map (kbd "<backtab>") #'claude-gravity-question-prev-tab)
(define-key claude-gravity-question-action-mode-map (kbd "n") #'claude-gravity-question-focus-next)
(define-key claude-gravity-question-action-mode-map (kbd "p") #'claude-gravity-question-focus-prev)
(define-key claude-gravity-question-action-mode-map (kbd "<down>") #'claude-gravity-question-focus-next)
(define-key claude-gravity-question-action-mode-map (kbd "<up>") #'claude-gravity-question-focus-prev)
(define-key claude-gravity-question-action-mode-map (kbd "RET") #'claude-gravity-question-toggle-or-select)
(define-key claude-gravity-question-action-mode-map (kbd "SPC") #'claude-gravity-question-toggle-or-select)
(define-key claude-gravity-question-action-mode-map (kbd "C-c C-c") #'claude-gravity-question-submit)
(define-key claude-gravity-question-action-mode-map (kbd "v") #'claude-gravity-question-preview-markdown)


(define-minor-mode claude-gravity-question-action-mode
  "Minor mode for question action buffers.
\\{claude-gravity-question-action-mode-map}"
  :lighter " QuestionAction"
  :keymap claude-gravity-question-action-mode-map)


;; Buffer-local state for multi-question support

(defvar-local claude-gravity--question-choices nil
  "List of (label . description) choices for the current question tab.")

(defvar-local claude-gravity--question-questions nil
  "Vector of question alists from AskUserQuestion tool_input.")

(defvar-local claude-gravity--question-current-idx 0
  "Zero-based index of the currently displayed question tab.")

(defvar-local claude-gravity--question-answers nil
  "Hash table: question-index -> answer.
Single-select: string.  Multi-select: list of strings.")

(defvar-local claude-gravity--question-focus 0
  "Zero-based index of the focused option (for n/p navigation and preview).")

(defvar-local claude-gravity--question-label nil
  "Session label for header display.")


;; --- Header-line (tab bar) ---

(defun claude-gravity--question-header-line ()
  "Compute header-line showing question tabs with [done] badges.
Only meaningful when more than one question."
  (let ((questions claude-gravity--question-questions)
        (idx claude-gravity--question-current-idx)
        (answers claude-gravity--question-answers)
        parts)
    (when (and questions (> (length questions) 1))
      (dotimes (i (length questions))
        (let* ((q (aref questions i))
               (header (or (alist-get 'header q) (format "Q%d" (1+ i))))
               (done-p (gethash i answers))
               (active-p (= i idx))
               (label (concat " " header
                               (if done-p " [done]" "")
                               " ")))
          (push (if active-p
                    (propertize label 'face '(:inverse-video t :weight bold))
                  (propertize label 'face 'claude-gravity-detail-label))
                parts)
          (unless (= i (1- (length questions)))
            (push (propertize " | " 'face 'claude-gravity-divider) parts))))
      (apply #'concat (nreverse parts)))))


;; --- Core rendering ---

(defun claude-gravity--question-current ()
  "Return the alist for the current question tab."
  (when (and claude-gravity--question-questions
             (< claude-gravity--question-current-idx
                (length claude-gravity--question-questions)))
    (aref claude-gravity--question-questions
          claude-gravity--question-current-idx)))


(defun claude-gravity--question-current-options ()
  "Return the options vector for the current question, or nil."
  (let ((q (claude-gravity--question-current)))
    (when q
      (let ((opts (alist-get 'options q)))
        (when (vectorp opts) opts)))))


(defun claude-gravity--question-multi-select-p ()
  "Return non-nil if the current question allows multi-select."
  (let ((q (claude-gravity--question-current)))
    (and q (eq (alist-get 'multiSelect q) t))))


(defun claude-gravity--question-render-body ()
  "Render the current question tab into the buffer.
Erases and redraws the body while preserving buffer-local state."
  (let* ((inhibit-read-only t)
         (q (claude-gravity--question-current))
         (q-text (and q (alist-get 'question q)))
         (header (and q (alist-get 'header q)))
         (options (claude-gravity--question-current-options))
         (multi-p (claude-gravity--question-multi-select-p))
         (focus claude-gravity--question-focus)
         (n-questions (length claude-gravity--question-questions))
         (idx claude-gravity--question-current-idx)
         (label claude-gravity--question-label)
         (choices nil))
    (erase-buffer)
    ;; Title line
    (if (> n-questions 1)
        (insert (propertize (format "Question %d/%d — %s\n" (1+ idx) n-questions (or label ""))
                            'face 'claude-gravity-header-title))
      (insert (propertize (format "Question — %s\n" (or label ""))
                          'face 'claude-gravity-header-title)))
    (insert (make-string 60 ?─) "\n\n")
    ;; Header badge
    (when header
      (insert (propertize (format "[%s]\n" header) 'face 'claude-gravity-detail-label)))
    ;; Question text
    (insert (propertize (or q-text "?") 'face 'default) "\n\n")
    ;; Options
    (when (and options (> (length options) 0))
      (dotimes (i (length options))
        (let* ((opt (aref options i))
               (opt-label (alist-get 'label opt))
               (desc (alist-get 'description opt))
               (focused-p (= i focus)))
          (push (cons opt-label desc) choices)
          (if multi-p
              ;; Checkbox mode
              (let* ((answer-set (gethash idx claude-gravity--question-answers))
                     (checked-p (and (listp answer-set) (member opt-label answer-set)))
                     (prefix (if checked-p "  [x] " "  [ ] "))
                     (line (concat prefix opt-label
                                   (if desc (format "  (%s)" desc) ""))))
                (insert (if focused-p
                            (propertize line 'face 'claude-gravity-running-bg)
                          line)
                        "\n"))
            ;; Single-select: numbered
            (let ((line (concat (propertize (format "  %d" (1+ i))
                                            'face 'claude-gravity-tool-name)
                                " " opt-label
                                (if desc (format "  (%s)" desc) ""))))
              (insert (if focused-p
                          (propertize line 'face 'claude-gravity-running-bg)
                        line)
                      "\n"))))))
    ;; Footer
    (insert "\n" (make-string 60 ?─) "\n")
    (let ((tab-hint (if (> n-questions 1)
                       (concat (propertize "  ]/[" 'face 'claude-gravity-tool-name) " Tab  ")
                     "")))
      (if multi-p
          (insert (propertize "  RET" 'face 'claude-gravity-tool-name) " Toggle  "
                  (propertize "  n/p" 'face 'claude-gravity-tool-name) " Navigate  "
                  (propertize "  C-c C-c" 'face 'claude-gravity-tool-name) " Submit  "
                  tab-hint
                  (propertize "  v" 'face 'claude-gravity-tool-name) " Preview  "
                  (propertize "  o" 'face 'claude-gravity-tool-name) " Free text  "
                  (propertize "  q" 'face 'claude-gravity-tool-name) " Close\n")
        (insert (propertize "  1-4" 'face 'claude-gravity-tool-name) " Select  "
                (propertize "  n/p" 'face 'claude-gravity-tool-name) " Navigate  "
                (propertize "  RET" 'face 'claude-gravity-tool-name) " Select focused  "
                tab-hint
                (propertize "  v" 'face 'claude-gravity-tool-name) " Preview  "
                (propertize "  o" 'face 'claude-gravity-tool-name) " Free text  "
                (propertize "  q" 'face 'claude-gravity-tool-name) " Close\n")))
    ;; Update backward-compat choices list
    (setq-local claude-gravity--question-choices (nreverse choices))
    ;; Keep cursor near top
    (goto-char (point-min))
    ;; Update header-line for multi-question
    (when (> n-questions 1)
      (setq header-line-format '(:eval (claude-gravity--question-header-line))))
    ;; Auto-update preview if visible
    (claude-gravity--question-update-preview-if-visible)))


(defun claude-gravity--inbox-act-question (item)
  "Open a question action buffer for inbox ITEM."
  (let* ((data (alist-get 'data item))
         (label (alist-get 'label item))
         (tool-input (alist-get 'tool_input data))
         (questions (alist-get 'questions tool-input))
         (item-id (alist-get 'id item))
         (buf (get-buffer-create (format "*Claude Action: Question #%d*" item-id))))
    (with-current-buffer buf
      (claude-gravity-question-action-mode 1)
      (setq-local claude-gravity--action-inbox-item item)
      (setq-local claude-gravity--question-questions
                  (if (vectorp questions) questions (vector)))
      (setq-local claude-gravity--question-current-idx 0)
      (setq-local claude-gravity--question-answers (make-hash-table :test 'eql))
      (setq-local claude-gravity--question-focus 0)
      (setq-local claude-gravity--question-label label)
      (setq buffer-read-only t)
      (claude-gravity--question-render-body))
    (puthash item-id buf claude-gravity--inbox-action-buffers)
    (display-buffer-in-side-window buf '((side . bottom) (window-height . 0.35)))
    (select-window (get-buffer-window buf))))


;; --- Tab navigation ---

(defun claude-gravity-question-next-tab ()
  "Switch to the next question tab (wraps around)."
  (interactive)
  (let ((n (length claude-gravity--question-questions)))
    (when (> n 1)
      (setq-local claude-gravity--question-current-idx
                  (mod (1+ claude-gravity--question-current-idx) n))
      (setq-local claude-gravity--question-focus 0)
      (claude-gravity--question-render-body))))


(defun claude-gravity-question-prev-tab ()
  "Switch to the previous question tab (wraps around)."
  (interactive)
  (let ((n (length claude-gravity--question-questions)))
    (when (> n 1)
      (setq-local claude-gravity--question-current-idx
                  (mod (+ claude-gravity--question-current-idx (1- n)) n))
      (setq-local claude-gravity--question-focus 0)
      (claude-gravity--question-render-body))))


;; --- Focus navigation ---

(defun claude-gravity-question-focus-next ()
  "Move focus to the next option."
  (interactive)
  (let ((opts (claude-gravity--question-current-options)))
    (when (and opts (> (length opts) 0))
      (setq-local claude-gravity--question-focus
                  (mod (1+ claude-gravity--question-focus) (length opts)))
      (claude-gravity--question-render-body))))


(defun claude-gravity-question-focus-prev ()
  "Move focus to the previous option."
  (interactive)
  (let ((opts (claude-gravity--question-current-options)))
    (when (and opts (> (length opts) 0))
      (setq-local claude-gravity--question-focus
                  (mod (+ claude-gravity--question-focus (1- (length opts)))
                       (length opts)))
      (claude-gravity--question-render-body))))


;; --- Toggle / Select ---

(defun claude-gravity-question-toggle-or-select ()
  "RET/SPC: multi-select toggles checkbox; single-select picks focused option."
  (interactive)
  (let ((opts (claude-gravity--question-current-options))
        (focus claude-gravity--question-focus)
        (idx claude-gravity--question-current-idx))
    (when (and opts (< focus (length opts)))
      (if (claude-gravity--question-multi-select-p)
          ;; Multi-select: toggle checkbox
          (let* ((opt-label (alist-get 'label (aref opts focus)))
                 (current (gethash idx claude-gravity--question-answers))
                 (current-list (if (listp current) current nil)))
            (if (member opt-label current-list)
                (puthash idx (remove opt-label current-list)
                         claude-gravity--question-answers)
              (puthash idx (append current-list (list opt-label))
                       claude-gravity--question-answers))
            (claude-gravity--question-render-body))
        ;; Single-select: pick focused option
        (let ((opt-label (alist-get 'label (aref opts focus))))
          (claude-gravity--question-answer-single idx opt-label))))))


(defun claude-gravity--question-answer-single (question-idx answer-label)
  "Record ANSWER-LABEL for single-select QUESTION-IDX and auto-advance."
  (puthash question-idx answer-label claude-gravity--question-answers)
  ;; Auto-advance: find next unanswered question, or submit if all done
  (let ((n (length claude-gravity--question-questions))
        (next nil))
    (cl-loop for offset from 1 below n
             for candidate = (mod (+ question-idx offset) n)
             unless (gethash candidate claude-gravity--question-answers)
             do (setq next candidate) and return nil)
    (if next
        (progn
          (setq-local claude-gravity--question-current-idx next)
          (setq-local claude-gravity--question-focus 0)
          (claude-gravity--question-render-body))
      ;; All answered — auto-submit
      (claude-gravity--question-do-submit))))


;; --- Number key handlers ---

(defun claude-gravity--question-action-select (n)
  "Select option N (1-based) from the current question."
  (let ((opts (claude-gravity--question-current-options))
        (idx claude-gravity--question-current-idx))
    (when (and opts (>= n 1) (<= n (length opts)))
      (let ((opt-label (alist-get 'label (aref opts (1- n)))))
        (if (claude-gravity--question-multi-select-p)
            ;; Multi-select: toggle
            (let* ((current (gethash idx claude-gravity--question-answers))
                   (current-list (if (listp current) current nil)))
              (if (member opt-label current-list)
                  (puthash idx (remove opt-label current-list)
                           claude-gravity--question-answers)
                (puthash idx (append current-list (list opt-label))
                         claude-gravity--question-answers))
              (setq-local claude-gravity--question-focus (1- n))
              (claude-gravity--question-render-body))
          ;; Single-select: answer + advance
          (claude-gravity--question-answer-single idx opt-label))))))


(defun claude-gravity-question-action-1 () "Select option 1." (interactive) (claude-gravity--question-action-select 1))
(defun claude-gravity-question-action-2 () "Select option 2." (interactive) (claude-gravity--question-action-select 2))
(defun claude-gravity-question-action-3 () "Select option 3." (interactive) (claude-gravity--question-action-select 3))
(defun claude-gravity-question-action-4 () "Select option 4." (interactive) (claude-gravity--question-action-select 4))


;; --- Submit ---

(defun claude-gravity-question-submit ()
  "Submit all answers.  Warns if unanswered questions remain."
  (interactive)
  (let ((n (length claude-gravity--question-questions))
        (answers claude-gravity--question-answers)
        unanswered)
    ;; For multi-select current question, store empty list if nothing checked
    (when (claude-gravity--question-multi-select-p)
      (let ((idx claude-gravity--question-current-idx))
        (unless (gethash idx answers)
          (puthash idx nil answers))))
    ;; Check for unanswered questions
    (dotimes (i n)
      (unless (gethash i answers)
        (push i unanswered)))
    (if unanswered
        (let ((labels (mapcar (lambda (i)
                                (let ((q (aref claude-gravity--question-questions i)))
                                  (or (alist-get 'header q) (format "Q%d" (1+ i)))))
                              (nreverse unanswered))))
          (message "Unanswered: %s  (use ]/[ to switch tabs)" (string-join labels ", ")))
      (claude-gravity--question-do-submit))))


(defun claude-gravity--question-do-submit ()
  "Send all answers and clean up."
  (let* ((item claude-gravity--action-inbox-item)
         (proc (alist-get 'socket-proc item))
         (data (alist-get 'data item))
         (tid (alist-get 'tool_use_id data))
         (session-id (alist-get 'session-id item))
         (questions claude-gravity--question-questions)
         (answers claude-gravity--question-answers)
         (n (length questions))
         ;; Build answers vector: string for single-select, vector for multi-select
         (answers-vec (let ((v (make-vector n nil)))
                        (dotimes (i n)
                          (let ((a (gethash i answers)))
                            (aset v i (if (listp a) (vconcat a) a))))
                        v))
         ;; First question's answer (backward compat)
         (first-answer (if (> n 0)
                           (let ((a (aref answers-vec 0)))
                             (if (vectorp a)
                                 (mapconcat #'identity (append a nil) ", ")
                               (or a "")))
                         ""))
         ;; Build human-readable summary
         (summary (let (parts)
                    (dotimes (i n)
                      (let* ((q (aref questions i))
                             (header (or (alist-get 'header q) (format "Q%d" (1+ i))))
                             (a (gethash i answers)))
                        (push (format "%s: %s" header
                                      (if (listp a)
                                          (mapconcat #'identity a ", ")
                                        (or a "")))
                              parts)))
                    (mapconcat #'identity (nreverse parts) "\n"))))
    ;; Send response
    (let ((response `((hookSpecificOutput
                       . ((hookEventName . "PreToolUse")
                          (permissionDecision . "deny")
                          (permissionDecisionReason
                           . ,(format "User answered from Emacs:\n%s" summary))))
                      (answer . ,first-answer)
                      (answers . ,answers-vec))))
      (claude-gravity--send-bidirectional-response proc response))
    ;; Update prompt entry with first answer
    (let ((session (claude-gravity--get-session session-id)))
      (when (and session tid)
        (claude-gravity-model-update-prompt-answer session tid first-answer)))
    ;; Clean up
    (remhash (alist-get 'id item) claude-gravity--inbox-action-buffers)
    (claude-gravity--inbox-remove (alist-get 'id item))
    ;; Kill preview buffer if exists
    (let ((preview-buf (get-buffer "*Claude Question Preview*")))
      (when (and preview-buf (buffer-live-p preview-buf))
        (kill-buffer preview-buf)))
    (let ((buf (current-buffer)))
      (quit-window)
      (kill-buffer buf))
    (claude-gravity--log 'debug "Answered: %s" summary)
    (claude-gravity--inbox-pop-next session-id)))


;; --- Free text / quit ---

(defun claude-gravity-question-action-other ()
  "Enter free text answer for the current question."
  (interactive)
  (let ((answer (read-string "Your answer: "))
        (idx claude-gravity--question-current-idx))
    (unless (string-empty-p answer)
      (if (claude-gravity--question-multi-select-p)
          (progn
            (puthash idx (list answer) claude-gravity--question-answers)
            ;; For multi-select with free text, auto-advance
            (claude-gravity--question-advance-or-submit idx))
        (claude-gravity--question-answer-single idx answer)))))


(defun claude-gravity--question-advance-or-submit (from-idx)
  "Advance to next unanswered question from FROM-IDX, or submit if all done."
  (let ((n (length claude-gravity--question-questions))
        (next nil))
    (cl-loop for offset from 1 below n
             for candidate = (mod (+ from-idx offset) n)
             unless (gethash candidate claude-gravity--question-answers)
             do (setq next candidate) and return nil)
    (if next
        (progn
          (setq-local claude-gravity--question-current-idx next)
          (setq-local claude-gravity--question-focus 0)
          (claude-gravity--question-render-body))
      (claude-gravity--question-do-submit))))


(defun claude-gravity-question-action-quit ()
  "Close question action buffer without responding."
  (interactive)
  (let ((preview-buf (get-buffer "*Claude Question Preview*")))
    (when (and preview-buf (buffer-live-p preview-buf))
      (kill-buffer preview-buf)))
  (let ((buf (current-buffer)))
    (quit-window)
    (kill-buffer buf)))


;; --- Markdown preview ---

(defun claude-gravity-question-preview-markdown ()
  "Show markdown preview for the focused option in a side window."
  (interactive)
  (let* ((opts (claude-gravity--question-current-options))
         (focus claude-gravity--question-focus)
         (opt (when (and opts (< focus (length opts))) (aref opts focus)))
         (md (when opt (alist-get 'markdown opt)))
         (opt-label (when opt (alist-get 'label opt))))
    (if (and md (stringp md) (not (string-empty-p md)))
        (let ((buf (get-buffer-create "*Claude Question Preview*")))
          (with-current-buffer buf
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert (propertize (format "Preview: %s\n" (or opt-label ""))
                                  'face 'claude-gravity-header-title))
              (insert (make-string 40 ?─) "\n\n")
              (insert (claude-gravity--fontify-markdown md)))
            (setq buffer-read-only t)
            (goto-char (point-min)))
          (display-buffer buf '((display-buffer-in-side-window)
                                (side . right)
                                (window-width . 0.4))))
      (message "No markdown preview for this option"))))


(defun claude-gravity--question-update-preview-if-visible ()
  "Update the preview window if it is currently visible."
  (let ((preview-buf (get-buffer "*Claude Question Preview*")))
    (when (and preview-buf (get-buffer-window preview-buf))
      (claude-gravity-question-preview-markdown))))


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