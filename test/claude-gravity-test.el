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

(provide 'claude-gravity-test)
;;; claude-gravity-test.el ends here
