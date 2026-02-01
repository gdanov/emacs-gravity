;;; claude-gravity.el --- Claude Code UI for Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  User

;; Author: User <user@example.com>
;; Version: 0.2.0
;; Package-Requires: ((emacs "27.1") (magit-section "3.0.0") (transient "0.3.0"))
;; Keywords: tools, ai, claude

;;; Commentary:
;; A Magit-like interface for Claude Code with multi-session support.

;;; Code:

(require 'magit-section)
(require 'transient)
(require 'json)
(require 'seq)
(require 'cl-lib)

(defgroup claude-gravity nil
  "Claude Code interface."
  :group 'tools)

(defvar claude-gravity-buffer-name "*Claude Gravity*"
  "Name of the overview buffer.")

;;; Comments (Mock)

(defun claude-gravity--make-comment-overlay (beg end text)
  "Create a comment overlay from BEG to END with TEXT."
  (let ((ov (make-overlay beg end)))
    (overlay-put ov 'face '(:underline (:style wave :color "orange")))
    (overlay-put ov 'help-echo text)
    (overlay-put ov 'claude-comment t)))

(defun claude-gravity-comment-at-point ()
  "Mock action: Comment on the item at point."
  (interactive)
  (let ((section (magit-current-section)))
    (if section
        (let* ((beg (magit-section-start section))
               (end (magit-section-end section))
               (text (read-string "Comment: ")))
          (claude-gravity--make-comment-overlay beg end text)
          (message "Added comment: %s" text))
      (message "No section selected"))))

;;; Session Registry

(defvar claude-gravity--sessions (make-hash-table :test 'equal)
  "Hash table mapping session-id -> session plist.
Each session plist has keys:
  :session-id  - string
  :cwd         - string (project working directory)
  :project     - string (basename of cwd)
  :state       - alist with (tools . []) (chat . [])
  :status      - symbol: active or ended
  :start-time  - time value
  :plan        - plist (:content STRING) or nil")

(defvar-local claude-gravity--buffer-session-id nil
  "Session ID for this per-session buffer.")

(defun claude-gravity--get-session (session-id)
  "Return session plist for SESSION-ID, or nil."
  (gethash session-id claude-gravity--sessions))

(defun claude-gravity--session-short-id (session-id)
  "Return first 4 chars of SESSION-ID for display."
  (if (and session-id (> (length session-id) 4))
      (substring session-id 0 4)
    (or session-id "?")))

(defun claude-gravity--session-label (session)
  "Return display label like \"emacs-gravity [a3f2]\"."
  (format "%s [%s]"
          (plist-get session :project)
          (claude-gravity--session-short-id (plist-get session :session-id))))

(defun claude-gravity--ensure-session (session-id cwd)
  "Get or create session for SESSION-ID with CWD.  Returns session plist."
  (or (gethash session-id claude-gravity--sessions)
      (let ((session (list :session-id session-id
                           :cwd (or cwd "")
                           :project (file-name-nondirectory
                                     (directory-file-name (or cwd "")))
                           :state (list (cons 'tools []) (cons 'chat []))
                           :status 'active
                           :claude-status 'idle
                           :start-time (current-time)
                           :plan nil
                           :prompts []
                           :agents []
                           :files (make-hash-table :test 'equal)
                           :tasks (make-hash-table :test 'equal))))
        (puthash session-id session claude-gravity--sessions)
        session)))

(defun claude-gravity-get-state ()
  "Return state for the current session buffer, or first active session."
  (if claude-gravity--buffer-session-id
      (let ((session (claude-gravity--get-session claude-gravity--buffer-session-id)))
        (when session (plist-get session :state)))
    ;; Fallback: first active session
    (let ((result (list (cons 'tools []) (cons 'chat []))))
      (maphash (lambda (_id session)
                 (when (eq (plist-get session :status) 'active)
                   (setq result (plist-get session :state))))
               claude-gravity--sessions)
      result)))

;;; Refresh timers

(defvar claude-gravity--refresh-timer nil
  "Timer for debounced overview UI refresh.")

(defvar claude-gravity--session-refresh-timers (make-hash-table :test 'equal)
  "Per-session debounce timers.")

(defun claude-gravity--schedule-refresh ()
  "Schedule an overview UI refresh after events settle."
  (when claude-gravity--refresh-timer
    (cancel-timer claude-gravity--refresh-timer))
  (setq claude-gravity--refresh-timer
        (run-with-idle-timer 0.1 nil #'claude-gravity--do-refresh)))

(defun claude-gravity--do-refresh ()
  "Perform the actual debounced overview refresh."
  (setq claude-gravity--refresh-timer nil)
  (when (get-buffer claude-gravity-buffer-name)
    (claude-gravity--render-overview)))

(defun claude-gravity--schedule-session-refresh (session-id)
  "Schedule a refresh for the session buffer of SESSION-ID."
  (let ((existing (gethash session-id claude-gravity--session-refresh-timers)))
    (when existing (cancel-timer existing))
    (puthash session-id
             (run-with-idle-timer 0.1 nil
                                  #'claude-gravity--do-session-refresh session-id)
             claude-gravity--session-refresh-timers)))

(defun claude-gravity--do-session-refresh (session-id)
  "Refresh the buffer for SESSION-ID if it exists."
  (remhash session-id claude-gravity--session-refresh-timers)
  (let* ((session (claude-gravity--get-session session-id))
         (buf-name (when session (claude-gravity--session-buffer-name session))))
    (when (and buf-name (get-buffer buf-name))
      (claude-gravity--render-session-buffer session))))

;;; Tool helpers

(defun claude-gravity--find-tool-by-id (tools tool-use-id)
  "Find index of tool in TOOLS vector matching TOOL-USE-ID."
  (let ((idx nil))
    (dotimes (i (length tools))
      (when (equal (alist-get 'tool_use_id (aref tools i)) tool-use-id)
        (setq idx i)))
    idx))

;;; File tracking

(defun claude-gravity--track-file (session tool-name tool-input)
  "Track file from TOOL-NAME with TOOL-INPUT in SESSION's :files hash table."
  (let ((files (plist-get session :files))
        (path nil)
        (op nil))
    (pcase tool-name
      ("Read"  (setq path (alist-get 'file_path tool-input) op "read"))
      ("Edit"  (setq path (alist-get 'file_path tool-input) op "edit"))
      ("Write" (setq path (alist-get 'file_path tool-input) op "write")))
    (when (and path op files)
      (let ((entry (gethash path files)))
        (if entry
            (progn
              (unless (member op (alist-get 'ops entry))
                (setf (alist-get 'ops entry) (cons op (alist-get 'ops entry))))
              (setf (alist-get 'last-touched entry) (current-time)))
          (puthash path (list (cons 'ops (list op))
                              (cons 'last-touched (current-time)))
                   files))))))

;;; Task tracking

(defun claude-gravity--track-task (session event tool-name tool-input tool-use-id &optional tool-response)
  "Track task operations in SESSION from EVENT with TOOL-NAME, TOOL-INPUT, TOOL-USE-ID and TOOL-RESPONSE."
  (let ((tasks (or (plist-get session :tasks)
                   (let ((ht (make-hash-table :test 'equal)))
                     (plist-put session :tasks ht)
                     ht))))
    (pcase event
      ("PreToolUse"
       (pcase tool-name
         ("TaskCreate"
          ;; Store with tool_use_id as temp key; we'll re-key on PostToolUse
          (puthash (concat "_pending_" tool-use-id)
                   (list (cons 'subject (alist-get 'subject tool-input))
                         (cons 'description (alist-get 'description tool-input))
                         (cons 'activeForm (alist-get 'activeForm tool-input))
                         (cons 'status "pending"))
                   tasks))
         ("TaskUpdate"
          (let* ((task-id (alist-get 'taskId tool-input))
                 (entry (gethash task-id tasks)))
            (when entry
              (let ((new-status (alist-get 'status tool-input))
                    (new-subject (alist-get 'subject tool-input))
                    (new-desc (alist-get 'description tool-input))
                    (new-active (alist-get 'activeForm tool-input)))
                (when new-status (setf (alist-get 'status entry) new-status))
                (when new-subject (setf (alist-get 'subject entry) new-subject))
                (when new-desc (setf (alist-get 'description entry) new-desc))
                (when new-active (setf (alist-get 'activeForm entry) new-active))
                (puthash task-id entry tasks)))))))
      ("PostToolUse"
       (pcase tool-name
         ("TaskCreate"
          ;; Re-key from temp ID to real taskId
          (let* ((temp-key (concat "_pending_" tool-use-id))
                 (entry (gethash temp-key tasks))
                 (task-id (alist-get 'taskId tool-response)))
            (when (and entry task-id)
              (setf (alist-get 'taskId entry) task-id)
              (puthash task-id entry tasks)
              (remhash temp-key tasks))))
         ("TaskList"
          ;; Reconcile with authoritative list
          (let ((task-list (alist-get 'tasks tool-response)))
            (when (and task-list (listp task-list))
              (dolist (task task-list)
                (let* ((task-id (alist-get 'id task))
                       (existing (gethash task-id tasks))
                       (new-entry (list (cons 'taskId task-id)
                                        (cons 'subject (alist-get 'subject task))
                                        (cons 'description (alist-get 'description task))
                                        (cons 'status (alist-get 'status task))
                                        (cons 'activeForm (or (alist-get 'activeForm task)
                                                              (and existing (alist-get 'activeForm existing)))))))
                  (puthash task-id new-entry tasks)))))))))))

;;; Agent helpers

(defun claude-gravity--find-agent-by-id (agents agent-id)
  "Find index of agent in AGENTS vector matching AGENT-ID."
  (let ((idx nil))
    (dotimes (i (length agents))
      (when (equal (alist-get 'agent_id (aref agents i)) agent-id)
        (setq idx i)))
    idx))

;;; Event handling

(defun claude-gravity-handle-event (event session-id cwd data)
  "Handle EVENT for SESSION-ID (with CWD) carrying DATA."
  (unless session-id
    (setq session-id "legacy")
    (setq cwd (or cwd "")))
  (message "Handling event: %s for session: %s" event session-id)
  (pcase event
    ("SessionStart"
     (claude-gravity--ensure-session session-id cwd))

    ("SessionEnd"
     (let ((session (claude-gravity--get-session session-id)))
       (when session
         (plist-put session :status 'ended))))

    ("UserPromptSubmit"
     (let* ((session (claude-gravity--ensure-session session-id cwd))
            (prompt-text (alist-get 'prompt data)))
       (when prompt-text
         (plist-put session :prompts
                    (vconcat (plist-get session :prompts) (vector prompt-text))))
       (plist-put session :claude-status 'responding)))

    ("Stop"
     (let ((session (claude-gravity--get-session session-id)))
       (when session
         (plist-put session :claude-status 'idle))))

    ("SubagentStart"
     (let* ((session (claude-gravity--ensure-session session-id cwd))
            (agents (plist-get session :agents))
            (new-agent (list (cons 'agent_id (alist-get 'agent_id data))
                             (cons 'type (alist-get 'agent_type data))
                             (cons 'status "running")
                             (cons 'timestamp (current-time)))))
       (plist-put session :agents (vconcat agents (vector new-agent)))))

    ("SubagentStop"
     (let* ((session (claude-gravity--ensure-session session-id cwd))
            (agents (plist-get session :agents))
            (agent-id (alist-get 'agent_id data))
            (idx (claude-gravity--find-agent-by-id agents agent-id)))
       (when idx
         (let ((agent (aref agents idx)))
           (setf (alist-get 'status agent) "done")
           (aset agents idx agent)))))

    ("PreToolUse"
     (let* ((session (claude-gravity--ensure-session session-id cwd))
            (state (plist-get session :state))
            (tools (alist-get 'tools state))
            (new-tool (list (cons 'tool_use_id (alist-get 'tool_use_id data))
                          (cons 'name (alist-get 'tool_name data))
                          (cons 'input (alist-get 'tool_input data))
                          (cons 'status "running")
                          (cons 'timestamp (current-time)))))
       (setf (alist-get 'tools state) (vconcat tools (vector new-tool)))
       (plist-put session :claude-status 'responding)
       (claude-gravity--track-file session (alist-get 'tool_name data) (alist-get 'tool_input data))
       (claude-gravity--track-task session "PreToolUse" (alist-get 'tool_name data)
                                   (alist-get 'tool_input data) (alist-get 'tool_use_id data))))

    ("PostToolUse"
     (let* ((session (claude-gravity--ensure-session session-id cwd))
            (state (plist-get session :state))
            (tools (alist-get 'tools state))
            (tool-use-id (alist-get 'tool_use_id data))
            (idx (claude-gravity--find-tool-by-id tools tool-use-id)))
       (when idx
         (let ((tool (aref tools idx)))
           (setf (alist-get 'status tool) "done")
           (setf (alist-get 'result tool) (alist-get 'tool_response data))
           (aset tools idx tool)))
       ;; Track file operations
       (claude-gravity--track-file session (alist-get 'tool_name data) (alist-get 'tool_input data))
       ;; Track task operations
       (claude-gravity--track-task session "PostToolUse" (alist-get 'tool_name data)
                                   (alist-get 'tool_input data) (alist-get 'tool_use_id data)
                                   (alist-get 'tool_response data))
       ;; Detect plan presentation
       (let ((tool-name (alist-get 'tool_name data)))
         (when (equal tool-name "ExitPlanMode")
           (let ((plan-content (alist-get 'plan (alist-get 'tool_input data))))
             (when plan-content
               (plist-put session :plan (list :content plan-content)))))))))

  (claude-gravity--schedule-refresh)
  (when session-id
    (claude-gravity--schedule-session-refresh session-id)))

;;; Faces

(defface claude-gravity-tool-done
  '((t :foreground "green"))
  "Face for completed tool status indicator."
  :group 'claude-gravity)

(defface claude-gravity-tool-running
  '((t :foreground "yellow"))
  "Face for running tool status indicator."
  :group 'claude-gravity)

(defface claude-gravity-tool-name
  '((t :weight bold))
  "Face for tool name."
  :group 'claude-gravity)

(defface claude-gravity-detail-label
  '((t :foreground "gray50"))
  "Face for detail labels in expanded tool view."
  :group 'claude-gravity)

(defface claude-gravity-stderr
  '((t :foreground "red"))
  "Face for stderr output."
  :group 'claude-gravity)

(defface claude-gravity-session-ended
  '((t :foreground "gray50"))
  "Face for ended session indicator."
  :group 'claude-gravity)

(defface claude-gravity-prompt
  '((t :foreground "cyan"))
  "Face for user prompt text."
  :group 'claude-gravity)

(defface claude-gravity-task-done
  '((t :foreground "green"))
  "Face for completed task checkbox."
  :group 'claude-gravity)

(defface claude-gravity-task-in-progress
  '((t :foreground "yellow"))
  "Face for in-progress task checkbox."
  :group 'claude-gravity)

(defface claude-gravity-task-pending
  '((t :foreground "gray50"))
  "Face for pending task checkbox."
  :group 'claude-gravity)

(defface claude-gravity-task-active-form
  '((t :foreground "gray50" :slant italic))
  "Face for task activeForm text."
  :group 'claude-gravity)

(defface claude-gravity-status-responding
  '((t :foreground "yellow"))
  "Face for responding status."
  :group 'claude-gravity)

(defface claude-gravity-status-idle
  '((t :foreground "green"))
  "Face for idle status."
  :group 'claude-gravity)

(defface claude-gravity-file-ops
  '((t :foreground "gray60"))
  "Face for file operation labels."
  :group 'claude-gravity)

;;; Tool display helpers

(defun claude-gravity--truncate (str max-len)
  "Truncate STR to MAX-LEN chars, adding ellipsis if needed."
  (if (and str (> (length str) max-len))
      (concat (substring str 0 (- max-len 1)) "\u2026")
    (or str "")))

(defun claude-gravity--shorten-path (path)
  "Shorten PATH to project-relative or basename."
  (if (null path) ""
    (let ((name (file-name-nondirectory path)))
      (if (string-empty-p name) path name))))

(defun claude-gravity--tool-summary (name input)
  "Produce a short one-line summary for tool NAME with INPUT alist."
  (pcase name
    ("Bash"
     (let* ((cmd (or (alist-get 'command input) ""))
            (parts (split-string cmd " " t))
            (prog (file-name-nondirectory (or (car parts) "")))
            (args (string-join (cdr parts) " ")))
       (claude-gravity--truncate (concat prog " " args) 60)))
    ("Read"
     (claude-gravity--shorten-path (alist-get 'file_path input)))
    ((or "Edit" "Write")
     (claude-gravity--shorten-path (alist-get 'file_path input)))
    ((or "Grep" "Glob")
     (let ((pattern (alist-get 'pattern input)))
       (if pattern (format "\"%s\"" (claude-gravity--truncate pattern 40)) "")))
    (_
     (let ((first-val (cdar input)))
       (if (stringp first-val)
           (claude-gravity--truncate first-val 50)
         "")))))

(defun claude-gravity--tool-description (input)
  "Return the description field from INPUT if present."
  (alist-get 'description input))

(defun claude-gravity--insert-tool-detail (name input result)
  "Insert expanded detail for tool NAME with INPUT and RESULT."
  (pcase name
    ("Bash"
     (let ((cmd (alist-get 'command input)))
       (when cmd
         (insert (propertize "    Command: " 'face 'claude-gravity-detail-label))
         (insert cmd "\n"))))
    ("Read"
     (let ((path (alist-get 'file_path input)))
       (when path
         (insert (propertize "    File: " 'face 'claude-gravity-detail-label))
         (insert path "\n"))))
    ((or "Edit" "Write")
     (let ((path (alist-get 'file_path input)))
       (when path
         (insert (propertize "    File: " 'face 'claude-gravity-detail-label))
         (insert path "\n"))))
    ((or "Grep" "Glob")
     (let ((pattern (alist-get 'pattern input))
           (path (alist-get 'path input)))
       (when pattern
         (insert (propertize "    Pattern: " 'face 'claude-gravity-detail-label))
         (insert pattern "\n"))
       (when path
         (insert (propertize "    Path: " 'face 'claude-gravity-detail-label))
         (insert path "\n")))))
  ;; Result section
  (when result
    (let ((stdout (alist-get 'stdout result))
          (stderr (alist-get 'stderr result))
          (file-data (alist-get 'file result)))
      ;; Bash-style stdout
      (when (and stdout (not (string-empty-p stdout)))
        (let* ((lines (split-string stdout "\n" t))
               (nlines (length lines))
               (preview (seq-take lines 8)))
          (insert (propertize (format "    Output (%d lines):\n" nlines)
                              'face 'claude-gravity-detail-label))
          (dolist (line preview)
            (insert "      " (claude-gravity--truncate line 80) "\n"))
          (when (> nlines 8)
            (insert (propertize "      ...\n" 'face 'claude-gravity-detail-label)))))
      ;; Read-style file content
      (when file-data
        (let* ((content (alist-get 'content file-data))
               (num-lines (alist-get 'numLines file-data))
               (total-lines (alist-get 'totalLines file-data)))
          (when content
            (let* ((lines (split-string content "\n"))
                   (preview (seq-take lines 6)))
              (insert (propertize (format "    Content (%d/%d lines):\n"
                                         (or num-lines (length lines))
                                         (or total-lines (length lines)))
                                  'face 'claude-gravity-detail-label))
              (dolist (line preview)
                (insert "      " (claude-gravity--truncate line 80) "\n"))
              (when (> (length lines) 6)
                (insert (propertize "      ...\n" 'face 'claude-gravity-detail-label)))))))
      ;; Stderr
      (when (and stderr (not (string-empty-p stderr)))
        (insert (propertize "    Stderr:\n" 'face 'claude-gravity-stderr))
        (dolist (line (seq-take (split-string stderr "\n" t) 4))
          (insert (propertize (concat "      " line "\n") 'face 'claude-gravity-stderr)))))))

;;; Plan display

(defun claude-gravity--update-plan (session content)
  "Update plan for SESSION from CONTENT string."
  (plist-put session :plan (list :content content))
  (claude-gravity--show-plan-buffer session))

(defun claude-gravity--show-plan-buffer (session)
  "Display the plan for SESSION in a dedicated buffer."
  (let ((plan (plist-get session :plan)))
    (when plan
      (let* ((label (claude-gravity--session-label session))
             (buf (get-buffer-create (format "*Claude Plan: %s*" label)))
             (content (plist-get plan :content)))
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert content)
            (goto-char (point-min)))
          (when (fboundp 'markdown-mode)
            (markdown-mode))
          (setq buffer-read-only t)
          (set-buffer-modified-p nil))
        (display-buffer buf '(display-buffer-in-side-window
                              (side . right)
                              (window-width . 0.4)))))))

(defun claude-gravity-show-plan ()
  "Show the plan for the current session."
  (interactive)
  (let* ((sid (or claude-gravity--buffer-session-id
                  (claude-gravity--current-overview-session-id)))
         (session (when sid (claude-gravity--get-session sid))))
    (if (and session (plist-get session :plan))
        (claude-gravity--show-plan-buffer session)
      (message "No plan available"))))

(defun claude-gravity--current-overview-session-id ()
  "Return session-id at point in the overview buffer, or nil."
  (let ((section (magit-current-section)))
    (when (and section (eq (magit-section-type section) 'session-entry))
      (magit-section-value section))))

(defun claude-gravity-insert-plan-link (session)
  "Insert a link to the plan for SESSION in the gravity buffer."
  (let ((plan (plist-get session :plan)))
    (when plan
      (let* ((content (plist-get plan :content))
             (first-line (car (split-string content "\n" t))))
        (magit-insert-section (plan-link)
          (magit-insert-heading
            (propertize "Current Plan" 'face 'claude-gravity-tool-name))
          (insert (format "  %s\n" (or first-line ""))
                  (propertize "  Press 'P' to view\n\n" 'face 'claude-gravity-detail-label)))))))

;;; Section renderers (used by per-session buffers)

(defun claude-gravity-insert-header (state)
  "Insert header section showing tool count from STATE."
  (magit-insert-section (header)
    (magit-insert-heading "Claude Code Gravity")
    (insert (format "Tools Executed: %d\n" (length (alist-get 'tools state))))
    (insert "\n")))

(defun claude-gravity-insert-tools (state)
  "Insert tool usage section from STATE."
  (let ((tools (alist-get 'tools state)))
    (when (> (length tools) 0)
      (magit-insert-section (tools)
        (magit-insert-heading "Tool Usage")
        (dolist (item (append tools nil))
          (let* ((name (alist-get 'name item))
                 (status (alist-get 'status item))
                 (input (alist-get 'input item))
                 (result (alist-get 'result item))
                 (done-p (equal status "done"))
                 (indicator (propertize (if done-p "[x]" "[/]")
                                        'face (if done-p
                                                   'claude-gravity-tool-done
                                                 'claude-gravity-tool-running)))
                 (tool-face (propertize (or name "?") 'face 'claude-gravity-tool-name))
                 (summary (claude-gravity--tool-summary name input))
                 (desc (claude-gravity--tool-description input)))
            (magit-insert-section (tool item t)
              (magit-insert-heading
                (format "%s %s  %s%s"
                        indicator
                        tool-face
                        summary
                        (if desc (format "  (%s)" desc) "")))
              (claude-gravity--insert-tool-detail name input result))))
        (insert "\n")))))

(defun claude-gravity-insert-tasks (session)
  "Insert tasks section for SESSION."
  (let* ((tasks-ht (plist-get session :tasks))
         (task-list nil)
         (completed 0)
         (total 0))
    (unless tasks-ht (setq tasks-ht (make-hash-table :test 'equal)))
    ;; Collect tasks, skipping pending temp keys
    (maphash (lambda (key val)
               (unless (string-prefix-p "_pending_" key)
                 (push val task-list)
                 (setq total (1+ total))
                 (when (equal (alist-get 'status val) "completed")
                   (setq completed (1+ completed)))))
             tasks-ht)
    (when (> total 0)
      ;; Sort: in_progress first, then pending, then completed
      (setq task-list
            (sort task-list
                  (lambda (a b)
                    (let ((sa (alist-get 'status a))
                          (sb (alist-get 'status b)))
                      (< (claude-gravity--task-sort-key sa)
                         (claude-gravity--task-sort-key sb))))))
      (magit-insert-section (tasks nil t)
        (magit-insert-heading
          (format "Tasks (%d/%d)" completed total))
        (dolist (task task-list)
          (let* ((subject (or (alist-get 'subject task) "(no subject)"))
                 (status (or (alist-get 'status task) "pending"))
                 (active-form (alist-get 'activeForm task))
                 (checkbox (pcase status
                             ("completed"
                              (propertize "[x]" 'face 'claude-gravity-task-done))
                             ("in_progress"
                              (propertize "[/]" 'face 'claude-gravity-task-in-progress))
                             (_
                              (propertize "[ ]" 'face 'claude-gravity-task-pending))))
                 (suffix (if (and (equal status "in_progress") active-form)
                             (concat "  " (propertize active-form 'face 'claude-gravity-task-active-form))
                           "")))
            (magit-insert-section (task task)
              (insert (format "  %s %s%s\n" checkbox subject suffix)))))
        (insert "\n")))))

(defun claude-gravity--task-sort-key (status)
  "Return sort key for task STATUS.  Lower = higher priority."
  (pcase status
    ("in_progress" 0)
    ("pending" 1)
    ("completed" 2)
    (_ 3)))

(defun claude-gravity-insert-prompts (session)
  "Insert prompts section for SESSION."
  (let ((prompts (plist-get session :prompts)))
    (when (and prompts (> (length prompts) 0))
      (magit-insert-section (prompts nil t)
        (magit-insert-heading
          (format "Prompts (%d)" (length prompts)))
        (let* ((all (append prompts nil))
               (shown (last all 10)))
          (dolist (p shown)
            (magit-insert-section (prompt p)
              (insert (propertize "  > " 'face 'claude-gravity-prompt)
                      (claude-gravity--truncate p 80) "\n"))))
        (insert "\n")))))

(defun claude-gravity-insert-agents (session)
  "Insert agents section for SESSION."
  (let ((agents (plist-get session :agents)))
    (when (and agents (> (length agents) 0))
      (magit-insert-section (agents nil t)
        (magit-insert-heading
          (format "Agents (%d)" (length agents)))
        (dolist (agent (append agents nil))
          (let* ((agent-type (alist-get 'type agent))
                 (agent-id (alist-get 'agent_id agent))
                 (status (alist-get 'status agent))
                 (done-p (equal status "done"))
                 (indicator (propertize (if done-p "[x]" "[/]")
                                        'face (if done-p
                                                   'claude-gravity-tool-done
                                                 'claude-gravity-tool-running)))
                 (short-id (if (and agent-id (> (length agent-id) 8))
                               (substring agent-id 0 8)
                             (or agent-id "?"))))
            (magit-insert-section (agent agent)
              (insert (format "  %s %s  (%s)\n"
                              indicator
                              (propertize (or agent-type "?") 'face 'claude-gravity-tool-name)
                              (propertize short-id 'face 'claude-gravity-detail-label))))))
        (insert "\n")))))

(defun claude-gravity-insert-files (session)
  "Insert files section for SESSION."
  (let ((files-ht (plist-get session :files)))
    (when (and files-ht (> (hash-table-count files-ht) 0))
      (let ((file-list nil))
        ;; Collect into list for sorting
        (maphash (lambda (path entry)
                   (push (list path
                               (alist-get 'ops entry)
                               (alist-get 'last-touched entry))
                         file-list))
                 files-ht)
        ;; Sort by last-touched, most recent first
        (setq file-list (sort file-list
                              (lambda (a b)
                                (time-less-p (nth 2 b) (nth 2 a)))))
        (magit-insert-section (files nil t)
          (magit-insert-heading
            (format "Files (%d)" (length file-list)))
          (dolist (entry file-list)
            (let* ((path (nth 0 entry))
                   (ops (nth 1 entry))
                   (basename (file-name-nondirectory path))
                   (ops-str (string-join (reverse ops) ", ")))
              (magit-insert-section (file-entry path t)
                (magit-insert-heading
                  (format "  %-30s %s"
                          (propertize basename 'face 'claude-gravity-tool-name)
                          (propertize ops-str 'face 'claude-gravity-file-ops)))
                (insert (propertize (format "    %s\n" path)
                                    'face 'claude-gravity-detail-label)))))
          (insert "\n"))))))

;;; Overview Buffer

(defun claude-gravity--render-overview ()
  "Render the overview buffer with all sessions grouped by project."
  (let ((buf (get-buffer claude-gravity-buffer-name)))
    (when buf
      (with-current-buffer buf
        (let ((inhibit-read-only t)
              (pos (point))
              (projects (make-hash-table :test 'equal)))
          ;; Group sessions by project
          (maphash (lambda (_id session)
                     (let ((proj (plist-get session :project)))
                       (puthash proj
                                (cons session (gethash proj projects nil))
                                projects)))
                   claude-gravity--sessions)
          (erase-buffer)
          (magit-insert-section (root)
            (magit-insert-section (header)
              (magit-insert-heading "Claude Code Gravity")
              (let ((active-count 0))
                (maphash (lambda (_k s)
                           (when (eq (plist-get s :status) 'active)
                             (cl-incf active-count)))
                         claude-gravity--sessions)
                (insert (format "Active sessions: %d\n\n" active-count))))
            (if (= (hash-table-count claude-gravity--sessions) 0)
                (insert (propertize "  No sessions.\n" 'face 'claude-gravity-detail-label))
              (maphash
               (lambda (proj-name sessions)
                 (magit-insert-section (project proj-name t)
                   (magit-insert-heading
                     (format "%s (%d)" proj-name (length sessions)))
                   (dolist (session (sort sessions
                                         (lambda (a b)
                                           (time-less-p (plist-get b :start-time)
                                                        (plist-get a :start-time)))))
                     (let* ((sid (plist-get session :session-id))
                            (label (claude-gravity--session-label session))
                            (status (plist-get session :status))
                            (claude-st (plist-get session :claude-status))
                            (tools (alist-get 'tools (plist-get session :state)))
                            (n-tools (length tools))
                            (indicator (if (eq status 'active)
                                           (propertize "●" 'face 'claude-gravity-tool-running)
                                         (propertize "○" 'face 'claude-gravity-session-ended)))
                            (status-label
                             (when (eq status 'active)
                               (if (eq claude-st 'responding)
                                   (propertize "responding" 'face 'claude-gravity-status-responding)
                                 (propertize "idle" 'face 'claude-gravity-status-idle)))))
                       (magit-insert-section (session-entry sid)
                         (magit-insert-heading
                           (format "  %s %s  %s  [%d tools]"
                                   indicator label
                                   (or status-label "")
                                   n-tools)))))))
               projects)))
          (goto-char (min pos (point-max))))))))

;;; Per-Session Buffer

(defun claude-gravity--session-buffer-name (session)
  "Return buffer name for SESSION."
  (format "*Claude Gravity: %s*" (claude-gravity--session-label session)))

(defun claude-gravity-open-session (session-id)
  "Open or switch to the buffer for SESSION-ID."
  (interactive)
  (let* ((session (claude-gravity--get-session session-id))
         (buf-name (claude-gravity--session-buffer-name session)))
    (with-current-buffer (get-buffer-create buf-name)
      (claude-gravity-session-mode)
      (setq claude-gravity--buffer-session-id session-id)
      (claude-gravity--render-session-buffer session)
      (switch-to-buffer (current-buffer)))))

(defun claude-gravity--render-session-buffer (session)
  "Render the magit-section UI for SESSION into its buffer."
  (let* ((buf-name (claude-gravity--session-buffer-name session))
         (state (plist-get session :state)))
    (when-let ((buf (get-buffer buf-name)))
      (with-current-buffer buf
        (let ((inhibit-read-only t)
              (pos (point)))
          (erase-buffer)
          (magit-insert-section (root)
            (claude-gravity-insert-header state)
            (claude-gravity-insert-plan-link session)
            (claude-gravity-insert-prompts session)
            (claude-gravity-insert-tools state)
            (claude-gravity-insert-agents session)
            (claude-gravity-insert-files session)
            (claude-gravity-insert-tasks session))
          (goto-char (min pos (point-max))))))))

;;; Modes

(defvar claude-gravity-mode-map (make-sparse-keymap)
  "Keymap for `claude-gravity-mode'.")
(set-keymap-parent claude-gravity-mode-map magit-section-mode-map)
(define-key claude-gravity-mode-map (kbd "g") 'claude-gravity-refresh)
(define-key claude-gravity-mode-map (kbd "c") 'claude-gravity-comment-at-point)
(define-key claude-gravity-mode-map (kbd "P") 'claude-gravity-show-plan)
(define-key claude-gravity-mode-map (kbd "?") 'claude-gravity-menu)
(define-key claude-gravity-mode-map (kbd "TAB") 'magit-section-toggle)
(define-key claude-gravity-mode-map (kbd "<return>") 'claude-gravity-visit-or-toggle)
(define-key claude-gravity-mode-map (kbd "D") 'claude-gravity-cleanup-sessions)

(define-derived-mode claude-gravity-mode magit-section-mode "ClaudeGravity"
  "Major mode for Claude Code Gravity overview.

\\{claude-gravity-mode-map}")

(define-derived-mode claude-gravity-session-mode claude-gravity-mode "ClaudeGravity:Session"
  "Major mode for a single Claude Code session buffer.")

(defun claude-gravity-visit-or-toggle ()
  "If on a session entry, open it.  Otherwise toggle the section."
  (interactive)
  (let ((section (magit-current-section)))
    (if (and section (eq (magit-section-type section) 'session-entry))
        (claude-gravity-open-session (magit-section-value section))
      (magit-section-toggle section))))

(defun claude-gravity-refresh ()
  "Refresh the current buffer."
  (interactive)
  (if claude-gravity--buffer-session-id
      ;; Per-session buffer
      (let ((session (claude-gravity--get-session claude-gravity--buffer-session-id)))
        (when session
          (claude-gravity--render-session-buffer session)))
    ;; Overview buffer
    (claude-gravity--render-overview)))

;;; Commands

;;;###autoload (autoload 'claude-gravity-menu "claude-gravity" nil t)
(transient-define-prefix claude-gravity-menu ()
  "Interactions with Claude Code."
  ["Actions"
   ("c" "Comment" claude-gravity-comment-at-point)
   ("g" "Refresh" claude-gravity-refresh)
   ("P" "Show Plan" claude-gravity-show-plan)]
  ["Sessions"
   ("D" "Remove ended sessions" claude-gravity-cleanup-sessions)]
  ["Manage"
   ("q" "Quit" bury-buffer)])

(defun claude-gravity-cleanup-sessions ()
  "Remove all ended sessions from the registry."
  (interactive)
  (let ((to-remove nil))
    (maphash (lambda (id session)
               (when (eq (plist-get session :status) 'ended)
                 (push id to-remove)))
             claude-gravity--sessions)
    (dolist (id to-remove)
      (let ((session (gethash id claude-gravity--sessions)))
        (when session
          (let ((buf (get-buffer (claude-gravity--session-buffer-name session))))
            (when buf (kill-buffer buf)))))
      (remhash id claude-gravity--sessions))
    (message "Removed %d ended session(s)" (length to-remove))
    (claude-gravity--render-overview)))

(defun claude-gravity-next-step ()
  "Mock action: Tell Claude to proceed to next step."
  (interactive)
  (message "Sending 'Next Step' command to Claude..."))

;;;###autoload
(defun claude-gravity-status ()
  "Show the Claude Code overview buffer."
  (interactive)
  (claude-gravity-setup-buffer))

(defun claude-gravity-setup-buffer ()
  "Create and display the overview buffer."
  (with-current-buffer (get-buffer-create claude-gravity-buffer-name)
    (claude-gravity-mode)
    (claude-gravity--render-overview)
    (switch-to-buffer (current-buffer))))

;;; Socket Server

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
              (let* ((json-object-type 'alist)
                     (data (json-read-from-string line)))
                (message "Claude Gravity received: %s" (alist-get 'event data))
                (let ((event (alist-get 'event data))
                      (session-id (alist-get 'session_id data))
                      (cwd (alist-get 'cwd data))
                      (payload (alist-get 'data data)))
                  (when event
                    (claude-gravity-handle-event event session-id cwd payload))))
            (error
             (message "Claude Gravity JSON error: %s" err))))))
    ;; Store any remaining partial data
    (process-put proc 'claude-gravity--buffer buf)))

(defun claude-gravity-server-start ()
  "Start the Unix socket server for Claude Gravity."
  (interactive)
  (claude-gravity-server-stop)
  (when (file-exists-p claude-gravity-server-sock-path)
    (delete-file claude-gravity-server-sock-path))
  (setq claude-gravity-server-process
        (make-network-process
         :name "claude-gravity-server"
         :server t
         :family 'local
         :service claude-gravity-server-sock-path
         :filter 'claude-gravity--server-filter))
  (message "Claude Gravity server started at %s" claude-gravity-server-sock-path))

(defun claude-gravity-server-stop ()
  "Stop the Claude Gravity server."
  (interactive)
  (when claude-gravity-server-process
    (delete-process claude-gravity-server-process)
    (setq claude-gravity-server-process nil))
  (when (file-exists-p claude-gravity-server-sock-path)
    (delete-file claude-gravity-server-sock-path))
  (message "Claude Gravity server stopped"))

(provide 'claude-gravity)
;;; claude-gravity.el ends here
