;;; claude-gravity.el --- Claude Code UI for Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  User

;; Author: User <user@example.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (magit-section "3.0.0") (transient "0.3.0"))
;; Keywords: tools, ai, claude

;;; Commentary:
;; A Magit-like interface for Claude Code.

;;; Code:

(require 'magit-section)
(require 'transient)
(require 'json)
(require 'seq)

(defgroup claude-gravity nil
  "Claude Code interface."
  :group 'tools)

(defvar claude-gravity-buffer-name "*Claude Gravity*")

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


;;; Data Models (Mock)

;;; State

(defvar claude-gravity--state
  '((tools . [])
    (chat . []))
  "Current state of the Claude Code session.")

(defun claude-gravity-get-state ()
  "Retrieve current state."
  claude-gravity--state)

(defun claude-gravity-handle-event (event data)
  "Handle an incoming EVENT with DATA."
  (message "Handling event: %s" event)
  (pcase event
    ("PreToolUse"
     (let* ((tools (alist-get 'tools claude-gravity--state))
            (new-tool `((name . ,(alist-get 'tool_name data))
                        (input . ,(alist-get 'tool_input data))
                        (status . "running")
                        (timestamp . ,(current-time)))))
       (setf (alist-get 'tools claude-gravity--state)
             (vconcat tools (vector new-tool)))))
    ("PostToolUse"
     (let* ((tools (alist-get 'tools claude-gravity--state))
            (len (length tools)))
       (when (> len 0)
         (let* ((last-idx (1- len))
                (tool (aref tools last-idx)))
           (when (equal (alist-get 'status tool) "running")
             (setf (alist-get 'status tool) "done")
             (setf (alist-get 'result tool) (alist-get 'tool_response data))
             (aset tools last-idx tool)))))))
  (claude-gravity-refresh))

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

;;; Sections

(defun claude-gravity-insert-header (plan)
  (magit-insert-section (header)
    (magit-insert-heading "Claude Code Gravity")
    (insert (format "Tools Executed: %d\n" (length (alist-get 'tools plan))))
    (insert "\n")))

(defun claude-gravity-insert-tools (plan)
  (let ((tools (alist-get 'tools plan)))
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

(defun claude-gravity-insert-steps (plan)
  (let ((steps (alist-get 'steps plan)))
    (magit-insert-section (steps)
      (magit-insert-heading "Plan")
      (seq-do (lambda (step)
                (let ((desc (alist-get 'description step))
                      (status (alist-get 'status step)))
                  (magit-insert-section (step step)
                    (insert (format "[%s] %s\n"
                                    (if (equal status "done") "x"
                                      (if (equal status "in-progress") "/" " "))
                                    desc)))))
              steps)
      (insert "\n"))))

(defun claude-gravity-insert-memory (plan)
  (let* ((mem (alist-get 'memory plan))
         (files (alist-get 'files mem)))
    (magit-insert-section (memory)
      (magit-insert-heading "Working Memory")
      (when files
        (insert "Files:\n")
        (seq-do (lambda (f) (insert (format " - %s\n" f))) files))
      (insert "\n"))))

;;; Mode

(defvar claude-gravity-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-section-mode-map)
    (define-key map (kbd "g") 'claude-gravity-refresh)
    (define-key map (kbd "c") 'claude-gravity-comment-at-point)
    (define-key map (kbd "TAB") 'magit-section-toggle)
    (define-key map (kbd "<return>") 'magit-section-toggle)
    map)
  "Keymap for `claude-gravity-mode'.")

(define-derived-mode claude-gravity-mode magit-section-mode "ClaudeGravity"
  "Major mode for Claude Code Gravity interface.

\\{claude-gravity-mode-map}")

(defun claude-gravity-refresh ()
  "Refresh the status buffer."
  (interactive)
  (message "Refreshing Claude Gravity...")
  (claude-gravity-setup-buffer)
  (message "Refreshing Claude Gravity...done"))

;;; Commands

;;;###autoload (autoload 'claude-gravity-menu "claude-gravity" nil t)
(transient-define-prefix claude-gravity-menu ()
  "Interactions with Claude Code."
  ["Actions"
   ("c" "Comment" claude-gravity-comment-at-point)
   ("r" "Refresh" claude-gravity-refresh)
   ("n" "Next Step" claude-gravity-next-step)]
  ["Manage"
   ("q" "Quit" bury-buffer)])

(defun claude-gravity-next-step ()
  "Mock action: Tell Claude to proceed to next step."
  (interactive)
  (message "Sending 'Next Step' command to Claude..."))

;;;###autoload
(defun claude-gravity-status ()
  "Show the Claude Code status buffer."
  (interactive)
  (claude-gravity-setup-buffer))

(defun claude-gravity-setup-buffer ()
  (with-current-buffer (get-buffer-create claude-gravity-buffer-name)
    (claude-gravity-mode)
    (let ((inhibit-read-only t)
          (plan (claude-gravity-get-state)))
      (erase-buffer)
      (magit-insert-section (root)
        (claude-gravity-insert-header plan)
        (claude-gravity-insert-tools plan)
        (claude-gravity-insert-steps plan)
        (claude-gravity-insert-memory plan)))
    (switch-to-buffer (current-buffer))))

;;; Socket Server

(defvar claude-gravity-server-sock-path
  (expand-file-name "claude-gravity.sock" (file-name-directory (or load-file-name buffer-file-name)))
  "Path to the Unix socket for Claude Gravity communication.")

(defvar claude-gravity-server-process nil
  "The current Claude Gravity server process.")

(defun claude-gravity--server-filter (proc string)
  "Process incoming JSON events from PROC in STRING."
  (let ((lines (split-string string "\n" t)))
    (dolist (line lines)
      (condition-case err
          (let* ((json-object-type 'alist)
                 (data (json-read-from-string line)))
            (message "Claude Gravity received: %s" data)
            (let ((event (alist-get 'event data))
                  (payload (alist-get 'data data)))
              (when event
                (claude-gravity-handle-event event payload))))
        (error
         (message "Claude Gravity JSON error: %s" err))))))

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
