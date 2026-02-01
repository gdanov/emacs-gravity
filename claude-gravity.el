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

(defvar claude-gravity--mock-plan
  '((goal . "Refactor database schema")
    (status . "planning")
    (steps . [((id . 1) (description . "Analyze dependencies") (status . "done"))
              ((id . 2) (description . "Draft migration script") (status . "in-progress"))
              ((id . 3) (description . "Run tests") (status . "todo"))])
    (memory . ((files . ["db/schema.sql" "models/user.js"])
               (notes . "Remember to back up the DB!")))))

;;; Utils

(defun claude-gravity-get-state ()
  "Retrieve current state (mock)."
  claude-gravity--mock-plan)

;;; Sections

(defun claude-gravity-insert-header (plan)
  (magit-insert-section (header)
    (magit-insert-heading "Claude Code Gravity")
    (magit-insert-section-body
      (insert (format "Goal: %s\n" (alist-get 'goal plan)))
      (insert (format "Status: %s\n" (alist-get 'status plan)))
      (insert "\n"))))

(defun claude-gravity-insert-steps (plan)
  (let ((steps (alist-get 'steps plan)))
    (magit-insert-section (steps)
      (magit-insert-heading "Plan")
      (magit-insert-section-body
        (seq-do (lambda (step)
                  (let ((desc (alist-get 'description step))
                        (status (alist-get 'status step)))
                    (magit-insert-section (step step)
                      (insert (format "[%s] %s\n" 
                                      (if (equal status "done") "x" 
                                        (if (equal status "in-progress") "/" " "))
                                      desc)))))
                steps)
        (insert "\n")))))

(defun claude-gravity-insert-memory (plan)
  (let* ((mem (alist-get 'memory plan))
         (files (alist-get 'files mem)))
    (magit-insert-section (memory)
      (magit-insert-heading "Working Memory")
      (magit-insert-section-body
        (when files
          (insert "Files:\n")
          (seq-do (lambda (f) (insert (format " - %s\n" f))) files))
        (insert "\n")))))

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

\\{claude-gravity-mode-map}"
  (magit-section-mode))

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
        (claude-gravity-insert-steps plan)
        (claude-gravity-insert-memory plan)))
    (magit-section-show-level-2)
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
            ;; TODO: Dispatch to handler based on event type
            (when-let ((plan (alist-get 'plan data)))
              ;; Update state if plan is present
              (setq claude-gravity--mock-plan plan)
              (claude-gravity-refresh)))
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
