;;; claude-gravity-test.el --- ERT tests for plan review -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'claude-gravity)

;;; Test helpers

(defun cg-test--fresh-session (sid)
  "Create a fresh session for SID and return SID."
  (clrhash claude-gravity--sessions)
  (claude-gravity--ensure-session sid "/tmp/test")
  sid)

(defun cg-test--get (sid)
  "Get session for SID."
  (claude-gravity--get-session sid))

;;; Plan review tests

(ert-deftest cg-test-plan-review-buffer-creation ()
  "Plan review creates buffer with correct content and modes."
  (let ((event-data '((tool_name . "ExitPlanMode")
                      (tool_input . ((plan . "1. Explore code\n2. Fix bug")
                                     (allowedPrompts . [])))))
        (sid "plan-test-1"))
    (cg-test--fresh-session sid)
    (unwind-protect
        (progn
          ;; Mock display-buffer to avoid needing windows
          (cl-letf (((symbol-function 'claude-gravity--display-buffer) #'ignore))
            (claude-gravity--handle-plan-review event-data sid))
          (let ((buf (get-buffer (format "*Claude Plan Review: %s*"
                                         (claude-gravity--session-label
                                          (cg-test--get sid))))))
            (should buf)
            (with-current-buffer buf
              (should claude-gravity-plan-review-mode)
              (should (string-match-p "Explore code" (buffer-string)))
              (should (string-match-p "Fix bug" (buffer-string)))
              (should (equal claude-gravity--plan-review-session-id sid))
              (should claude-gravity--plan-review-original))))
      ;; Cleanup: kill any plan review buffers
      (dolist (buf (buffer-list))
        (when (string-match-p "\\*Claude Plan Review:" (buffer-name buf))
          (with-current-buffer buf
            (remove-hook 'kill-buffer-hook #'claude-gravity--plan-review-on-kill t))
          (kill-buffer buf))))))

(ert-deftest cg-test-plan-review-diff-detection ()
  "Editing plan content produces a diff."
  (let ((event-data '((tool_name . "ExitPlanMode")
                      (tool_input . ((plan . "1. Step one\n2. Step two")))))
        (sid "plan-test-2"))
    (cg-test--fresh-session sid)
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'claude-gravity--display-buffer) #'ignore))
            (claude-gravity--handle-plan-review event-data sid))
          (let ((buf (cl-find-if
                      (lambda (b) (string-match-p "\\*Claude Plan Review:" (buffer-name b)))
                      (buffer-list))))
            (should buf)
            (with-current-buffer buf
              ;; Edit the plan
              (goto-char (point-min))
              (search-forward "Step two")
              (replace-match "Step two (updated)")
              ;; Verify diff
              (let ((diff (claude-gravity--plan-review-compute-diff)))
                (should diff)
                (should (string-match-p "updated" diff))))))
      (dolist (buf (buffer-list))
        (when (string-match-p "\\*Claude Plan Review:" (buffer-name buf))
          (with-current-buffer buf
            (remove-hook 'kill-buffer-hook #'claude-gravity--plan-review-on-kill t))
          (kill-buffer buf))))))

(ert-deftest cg-test-plan-review-marker-scanning ()
  "@claude markers are detected in plan content."
  (let ((event-data '((tool_name . "ExitPlanMode")
                      (tool_input . ((plan . "1. Step one\n@claude: also fix tests\n2. Step two")))))
        (sid "plan-test-3"))
    (cg-test--fresh-session sid)
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'claude-gravity--display-buffer) #'ignore))
            (claude-gravity--handle-plan-review event-data sid))
          (let ((buf (cl-find-if
                      (lambda (b) (string-match-p "\\*Claude Plan Review:" (buffer-name b)))
                      (buffer-list))))
            (should buf)
            (with-current-buffer buf
              ;; scan-markers returns a formatted markdown string
              (let ((markers (claude-gravity--plan-review-scan-markers)))
                (should markers)
                (should (string-match-p "fix tests" markers))
                (should (string-match-p "@claude markers" markers))))))
      (dolist (buf (buffer-list))
        (when (string-match-p "\\*Claude Plan Review:" (buffer-name buf))
          (with-current-buffer buf
            (remove-hook 'kill-buffer-hook #'claude-gravity--plan-review-on-kill t))
          (kill-buffer buf))))))

(ert-deftest cg-test-plan-review-feedback-message ()
  "Feedback message builder produces structured markdown."
  (let ((diff "--- a\n+++ b\n-old\n+new")
        (comments "## Inline comments:\n- Line 5 (near \"Step one\"): \"needs detail\"\n")
        (markers "## @claude markers:\n- Line 3 (near \"fix\"): \"also update docs\"\n")
        (general "Please reconsider step 2"))
    (let ((msg (claude-gravity--plan-review-build-feedback-message
                diff comments markers general)))
      (should (string-match-p "Inline comments" msg))
      (should (string-match-p "needs detail" msg))
      (should (string-match-p "@claude markers" msg))
      (should (string-match-p "update docs" msg))
      (should (string-match-p "Changes requested" msg))
      (should (string-match-p "General comment" msg))
      (should (string-match-p "reconsider" msg)))))

(ert-deftest cg-test-plan-review-approve-sends-allow ()
  "Clean approve (no edits) sends allow response."
  (let ((event-data '((tool_name . "ExitPlanMode")
                      (tool_input . ((plan . "1. Do stuff")))))
        (sid "plan-test-4")
        (sent-responses nil))
    (cg-test--fresh-session sid)
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'claude-gravity--display-buffer) #'ignore))
            (claude-gravity--handle-plan-review event-data sid))
          (let ((buf (cl-find-if
                      (lambda (b) (string-match-p "\\*Claude Plan Review:" (buffer-name b)))
                      (buffer-list))))
            (should buf)
            (with-current-buffer buf
              (setq-local claude-gravity--plan-review-inbox-id 42)
              ;; Mock the send function and cleanup helpers
              (cl-letf (((symbol-function 'claude-gravity--send-plan-review-response)
                         (lambda (id decision &optional msg)
                           (push (list :id id :decision decision :msg msg) sent-responses)))
                        ((symbol-function 'claude-gravity--inbox-remove) #'ignore)
                        ((symbol-function 'claude-gravity--enable-session-follow-mode) #'ignore)
                        ((symbol-function 'quit-window) #'ignore))
                (claude-gravity-plan-review-approve)))))
      (dolist (buf (buffer-list))
        (when (string-match-p "\\*Claude Plan Review:" (buffer-name buf))
          (with-current-buffer buf
            (remove-hook 'kill-buffer-hook #'claude-gravity--plan-review-on-kill t))
          (kill-buffer buf))))
    (should (= 1 (length sent-responses)))
    (should (equal "allow" (plist-get (car sent-responses) :decision)))))

(ert-deftest cg-test-plan-review-approve-with-edits-sends-deny ()
  "Approve with edits auto-converts to deny with feedback."
  (let ((event-data '((tool_name . "ExitPlanMode")
                      (tool_input . ((plan . "1. Do stuff")))))
        (sid "plan-test-5")
        (sent-responses nil))
    (cg-test--fresh-session sid)
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'claude-gravity--display-buffer) #'ignore))
            (claude-gravity--handle-plan-review event-data sid))
          (let ((buf (cl-find-if
                      (lambda (b) (string-match-p "\\*Claude Plan Review:" (buffer-name b)))
                      (buffer-list))))
            (should buf)
            (with-current-buffer buf
              (setq-local claude-gravity--plan-review-inbox-id 42)
              ;; Make an edit
              (goto-char (point-max))
              (insert "3. Also run tests\n")
              ;; Mock the send function and cleanup helpers
              (cl-letf (((symbol-function 'claude-gravity--send-plan-review-response)
                         (lambda (id decision &optional msg)
                           (push (list :id id :decision decision :msg msg) sent-responses)))
                        ((symbol-function 'claude-gravity--inbox-remove) #'ignore)
                        ((symbol-function 'claude-gravity--enable-session-follow-mode) #'ignore)
                        ((symbol-function 'quit-window) #'ignore))
                (claude-gravity-plan-review-approve)))))
      (dolist (buf (buffer-list))
        (when (string-match-p "\\*Claude Plan Review:" (buffer-name buf))
          (with-current-buffer buf
            (remove-hook 'kill-buffer-hook #'claude-gravity--plan-review-on-kill t))
          (kill-buffer buf))))
    (should (= 1 (length sent-responses)))
    (should (equal "deny" (plist-get (car sent-responses) :decision)))
    (should (plist-get (car sent-responses) :msg))))

(ert-deftest cg-test-inbox-plan-review-dispatch ()
  "Inbox item with type plan-review dispatches to plan review handler."
  (let ((dispatched nil)
        (item '((id . 999)
                (type . plan-review)
                (session-id . "plan-dispatch-test")
                (label . "test-label")
                (data . ((tool_name . "ExitPlanMode")
                         (tool_input . ((plan . "test plan"))))))))
    (cg-test--fresh-session "plan-dispatch-test")
    (unwind-protect
        (cl-letf (((symbol-function 'claude-gravity--inbox-act-plan-review)
                   (lambda (it) (setq dispatched it))))
          ;; Simulate inbox dispatch
          (pcase (alist-get 'type item)
            ('plan-review (claude-gravity--inbox-act-plan-review item))
            ('permission (claude-gravity--inbox-act-permission item)))
          (should dispatched)
          (should (= 999 (alist-get 'id dispatched))))
      nil)))

;;; Per-turn edited-files tests

(defun cg-test--file-diff-patch (turn path &rest overrides)
  "Build an `update_turn_file' patch alist for TURN and PATH.
OVERRIDES is a plist merged into the `file' object (keys: ops added
removed status editCount hunks truncated)."
  `((op . "update_turn_file")
    (turnNumber . ,turn)
    (file . ((path . ,path)
             (ops . ,(or (plist-get overrides :ops) ["edit"]))
             (editCount . ,(or (plist-get overrides :editCount) 1))
             (status . ,(or (plist-get overrides :status) "modified"))
             (added . ,(or (plist-get overrides :added) 0))
             (removed . ,(or (plist-get overrides :removed) 0))
             (hunks . ,(if (plist-member overrides :hunks)
                           (plist-get overrides :hunks)
                         :null))
             (truncated . ,(if (plist-get overrides :truncated) t :false))))))

(ert-deftest cg-test-update-turn-file-insert-and-replace ()
  "`update_turn_file' inserts a file entry, then replaces by path."
  (let ((sid "turn-file-test-1"))
    (cg-test--fresh-session sid)
    (let ((session (cg-test--get sid)))
      ;; Need a turn for turnNumber 1
      (let ((turn (claude-gravity--make-turn-node 1)))
        (claude-gravity--tlist-append (plist-get session :turns) turn)
        (claude-gravity--index-turn session turn))
      ;; First patch — inserts
      (claude-gravity--apply-patch
       session (cg-test--file-diff-patch 1 "a.el" :added 10 :removed 2))
      (let* ((turn (claude-gravity--get-turn-node session 1))
             (files (alist-get 'edited-files turn)))
        (should (= 1 (length files)))
        (should (equal "a.el" (alist-get 'path (car files))))
        (should (= 10 (alist-get 'added (car files)))))
      ;; Second patch, same path — replaces (not append)
      (claude-gravity--apply-patch
       session (cg-test--file-diff-patch 1 "a.el" :added 30 :removed 5
                                         :editCount 3))
      (let* ((turn (claude-gravity--get-turn-node session 1))
             (files (alist-get 'edited-files turn)))
        (should (= 1 (length files)))
        (should (= 30 (alist-get 'added (car files))))
        (should (= 3 (alist-get 'editCount (car files)))))
      ;; Third patch, different path — appends
      (claude-gravity--apply-patch
       session (cg-test--file-diff-patch 1 "b.el" :added 1 :removed 1))
      (let* ((turn (claude-gravity--get-turn-node session 1))
             (files (alist-get 'edited-files turn)))
        (should (= 2 (length files)))
        (should (equal '("a.el" "b.el")
                       (mapcar (lambda (f) (alist-get 'path f)) files)))))))

(defun cg-test--render-edited-files (turn)
  "Render TURN's edited-files section into a temp buffer; return plain text."
  (require 'claude-gravity-render)
  (with-temp-buffer
    (magit-insert-section (root)
      (claude-gravity--insert-turn-edited-files turn))
    (buffer-substring-no-properties (point-min) (point-max))))

(ert-deftest cg-test-render-turn-edited-files-heading ()
  "Rendering a turn's edited files emits the heading with aggregate counts."
  (let ((turn (claude-gravity--make-turn-node 1)))
    (setf (alist-get 'edited-files turn)
          (list '((path . "claude-gravity-ui.el")
                  (ops . ("edit" "edit"))
                  (editCount . 2) (status . "modified")
                  (added . 88) (removed . 20)
                  (hunks . nil) (truncated . nil))
                '((path . "session.ts")
                  (ops . ("write"))
                  (editCount . 1) (status . "created")
                  (added . 43) (removed . 22)
                  (hunks . nil) (truncated . nil))))
    (let ((rendered (cg-test--render-edited-files turn)))
      (should (string-match-p "Edited Files (2)" rendered))
      ;; Aggregate: 88+43 added, 20+22 removed
      (should (string-match-p "+131" rendered))
      (should (string-match-p "−42" rendered))
      ;; Per-file: created status shown instead of edit count
      (should (string-match-p "created" rendered))
      (should (string-match-p "2 edits" rendered))
      (should (string-match-p "claude-gravity-ui.el" rendered))
      (should (string-match-p "session.ts" rendered)))))

(ert-deftest cg-test-render-turn-edited-files-nil-hunks ()
  "A file-diff with nil hunks renders path-only without error."
  (let ((turn (claude-gravity--make-turn-node 1)))
    (setf (alist-get 'edited-files turn)
          (list '((path . "no-diff.el")
                  (ops . ("edit"))
                  (editCount . 1) (status . "modified")
                  (added . 0) (removed . 0)
                  (hunks . nil) (truncated . nil))))
    (let ((rendered (cg-test--render-edited-files turn)))
      (should (string-match-p "no-diff.el" rendered))
      (should (string-match-p "diff unavailable" rendered))))
  ;; truncated with nil hunks → "diff too large"
  (let ((turn2 (claude-gravity--make-turn-node 2)))
    (setf (alist-get 'edited-files turn2)
          (list '((path . "huge.el")
                  (ops . ("edit"))
                  (editCount . 1) (status . "modified")
                  (added . 9999) (removed . 8888)
                  (hunks . nil) (truncated . t))))
    (let ((rendered2 (cg-test--render-edited-files turn2)))
      (should (string-match-p "diff too large" rendered2)))))

(ert-deftest cg-test-render-turn-edited-files-with-hunks ()
  "A file-diff with hunks renders the structured patch."
  (let ((turn (claude-gravity--make-turn-node 1)))
    (setf (alist-get 'edited-files turn)
          (list `((path . "patched.el")
                  (ops . ("edit"))
                  (editCount . 1) (status . "modified")
                  (added . 1) (removed . 1)
                  (hunks . (((oldStart . 10) (oldLines . 2)
                             (newStart . 10) (newLines . 2)
                             (lines . (" context"
                                       "-old line"
                                       "+new line")))))
                  (truncated . nil))))
    (let ((rendered (cg-test--render-edited-files turn)))
      (should (string-match-p "patched.el" rendered))
      (should (string-match-p (regexp-quote "@@ -10,2 +10,2 @@") rendered))
      (should (string-match-p "Diff:" rendered)))))

(provide 'claude-gravity-test)
;;; claude-gravity-test.el ends here
