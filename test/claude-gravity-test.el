;;; claude-gravity-test.el --- ERT tests for turn demarcation -*- lexical-binding: t; -*-

(require 'ert)
(require 'claude-gravity)

;;; Test helpers

(defun cg-test--fresh-session (sid)
  "Clear all sessions and return SID for use in tests."
  (clrhash claude-gravity--sessions)
  sid)

(defun cg-test--get (sid)
  "Get session for SID."
  (claude-gravity--get-session sid))

(defun cg-test--prompt-submit (sid text)
  "Send a UserPromptSubmit event for SID with TEXT."
  (claude-gravity-handle-event
   "UserPromptSubmit" sid "/tmp/test"
   (list (cons 'prompt text))))

(defun cg-test--pre-tool (sid name id &optional extra-data)
  "Send a PreToolUse event for SID with tool NAME and ID.
EXTRA-DATA is an alist merged into the event data."
  (let ((data (append (list (cons 'tool_name name)
                            (cons 'tool_use_id id)
                            (cons 'tool_input (or (alist-get 'tool_input extra-data) '())))
                      extra-data)))
    (claude-gravity-handle-event "PreToolUse" sid "/tmp/test" data)))

(defun cg-test--post-tool (sid name id &optional extra-data)
  "Send a PostToolUse event for SID with tool NAME and ID.
EXTRA-DATA is an alist merged into the event data."
  (let ((data (append (list (cons 'tool_name name)
                            (cons 'tool_use_id id)
                            (cons 'tool_input (or (alist-get 'tool_input extra-data) '()))
                            (cons 'tool_response nil))
                      extra-data)))
    (claude-gravity-handle-event "PostToolUse" sid "/tmp/test" data)))

(defun cg-test--stop (sid &optional extra-data)
  "Send a Stop event for SID.
EXTRA-DATA is an alist merged into the event data."
  (claude-gravity-handle-event "Stop" sid "/tmp/test" (or extra-data '())))

(defun cg-test--session-start (sid)
  "Send a SessionStart event for SID."
  (claude-gravity-handle-event "SessionStart" sid "/tmp/test" '()))

(defun cg-test--tools (sid)
  "Get the tools vector for SID."
  (let* ((session (cg-test--get sid))
         (state (plist-get session :state)))
    (alist-get 'tools state)))

(defun cg-test--tool-by-id (sid id)
  "Get the tool entry with ID from SID's tools."
  (let* ((tools (cg-test--tools sid))
         (idx (claude-gravity--find-tool-by-id tools id)))
    (when idx (aref tools idx))))

;;; Tests

(ert-deftest cg-test-user-prompt-advances-turn ()
  "UserPromptSubmit creates a prompt entry and increments current-turn."
  (let ((sid (cg-test--fresh-session "test-1")))
    (cg-test--prompt-submit sid "hello")
    (let ((session (cg-test--get sid)))
      (should (= 1 (plist-get session :current-turn)))
      (should (= 1 (length (plist-get session :prompts))))
      (should (equal "hello"
                     (alist-get 'text (aref (plist-get session :prompts) 0)))))))

(ert-deftest cg-test-tools-stamped-with-current-turn ()
  "Tools get the turn number at creation time."
  (let ((sid (cg-test--fresh-session "test-2")))
    (cg-test--prompt-submit sid "do work")
    (cg-test--pre-tool sid "Read" "t1")
    (cg-test--pre-tool sid "Edit" "t2")
    (should (= 1 (alist-get 'turn (cg-test--tool-by-id sid "t1"))))
    (should (= 1 (alist-get 'turn (cg-test--tool-by-id sid "t2"))))))

(ert-deftest cg-test-exit-plan-mode-advances-turn ()
  "ExitPlanMode PostToolUse creates a phase-boundary prompt and advances turn."
  (let ((sid (cg-test--fresh-session "test-3")))
    (cg-test--prompt-submit sid "plan something")
    (cg-test--pre-tool sid "Grep" "t1")
    (cg-test--pre-tool sid "ExitPlanMode" "t2")
    (cg-test--post-tool sid "ExitPlanMode" "t2")
    (cg-test--pre-tool sid "Edit" "t3")
    (let* ((session (cg-test--get sid))
           (prompts (plist-get session :prompts)))
      ;; Two prompts: original + phase-boundary
      (should (= 2 (length prompts)))
      (should (equal "[Plan approved]" (alist-get 'text (aref prompts 1))))
      (should (eq 'phase-boundary (alist-get 'type (aref prompts 1))))
      ;; Turn advanced
      (should (= 2 (plist-get session :current-turn)))
      ;; Tools before ExitPlanMode are turn 1, after are turn 2
      (should (= 1 (alist-get 'turn (cg-test--tool-by-id sid "t1"))))
      (should (= 1 (alist-get 'turn (cg-test--tool-by-id sid "t2"))))
      (should (= 2 (alist-get 'turn (cg-test--tool-by-id sid "t3")))))))

(ert-deftest cg-test-ask-user-question-advances-turn ()
  "AskUserQuestion PreToolUse creates a question prompt and advances turn."
  (let ((sid (cg-test--fresh-session "test-4")))
    (cg-test--prompt-submit sid "investigate")
    (cg-test--pre-tool sid "Grep" "t1")
    (cg-test--pre-tool sid "AskUserQuestion" "t2"
                       (list (cons 'tool_input
                                   (list (cons 'questions
                                               (vector (list (cons 'question "Which approach?"))))))))
    (cg-test--pre-tool sid "Read" "t3")
    (let* ((session (cg-test--get sid))
           (prompts (plist-get session :prompts)))
      ;; Two prompts: original + question
      (should (= 2 (length prompts)))
      (should (eq 'question (alist-get 'type (aref prompts 1))))
      (should (equal "Which approach?" (alist-get 'text (aref prompts 1))))
      ;; Turn advanced
      (should (= 2 (plist-get session :current-turn)))
      ;; Tool after question is turn 2
      (should (= 1 (alist-get 'turn (cg-test--tool-by-id sid "t1"))))
      (should (= 2 (alist-get 'turn (cg-test--tool-by-id sid "t3")))))))

(ert-deftest cg-test-multiple-boundaries-in-sequence ()
  "Multiple ExitPlanMode cycles accumulate correctly."
  (let ((sid (cg-test--fresh-session "test-5")))
    ;; First cycle
    (cg-test--prompt-submit sid "first")
    (cg-test--pre-tool sid "ExitPlanMode" "t1")
    (cg-test--post-tool sid "ExitPlanMode" "t1")
    (cg-test--pre-tool sid "Edit" "t2")
    ;; Second cycle
    (cg-test--prompt-submit sid "second")
    (cg-test--pre-tool sid "ExitPlanMode" "t3")
    (cg-test--post-tool sid "ExitPlanMode" "t3")
    (cg-test--pre-tool sid "Edit" "t4")
    (let* ((session (cg-test--get sid))
           (prompts (plist-get session :prompts)))
      ;; 4 prompts: first, [Plan approved], second, [Plan approved]
      (should (= 4 (length prompts)))
      (should (= 4 (plist-get session :current-turn)))
      ;; Tools get correct turns
      (should (= 2 (alist-get 'turn (cg-test--tool-by-id sid "t2"))))
      (should (= 4 (alist-get 'turn (cg-test--tool-by-id sid "t4")))))))

(ert-deftest cg-test-stop-attaches-to-last-prompt ()
  "Stop handler attaches stop_text to the last prompt, including phase-boundary."
  (let ((sid (cg-test--fresh-session "test-6")))
    (cg-test--prompt-submit sid "plan")
    (cg-test--pre-tool sid "ExitPlanMode" "t1")
    (cg-test--post-tool sid "ExitPlanMode" "t1")
    (cg-test--stop sid (list (cons 'stop_text "Done")))
    (let* ((session (cg-test--get sid))
           (prompts (plist-get session :prompts)))
      ;; stop_text on the phase-boundary prompt (last one)
      (should (equal "Done" (alist-get 'stop_text (aref prompts 1)))))))

(ert-deftest cg-test-permission-mode-stored-on-tool ()
  "PreToolUse stores permission_mode on the tool entry and session."
  (let ((sid (cg-test--fresh-session "test-7")))
    (cg-test--prompt-submit sid "do work")
    (cg-test--pre-tool sid "Read" "t1"
                       (list (cons 'permission_mode "plan")))
    (let ((tool (cg-test--tool-by-id sid "t1"))
          (session (cg-test--get sid)))
      (should (equal "plan" (alist-get 'permission_mode tool)))
      (should (equal "plan" (plist-get session :permission-mode))))))

(ert-deftest cg-test-session-reset-clears-turn-state ()
  "SessionStart on existing session resets turns and permission-mode."
  (let ((sid (cg-test--fresh-session "test-8")))
    (cg-test--prompt-submit sid "work")
    (cg-test--pre-tool sid "Read" "t1"
                       (list (cons 'permission_mode "plan")))
    ;; Verify state exists
    (should (= 1 (plist-get (cg-test--get sid) :current-turn)))
    ;; Reset via SessionStart
    (cg-test--session-start sid)
    (let ((session (cg-test--get sid)))
      (should (= 0 (plist-get session :current-turn)))
      (should (= 0 (length (plist-get session :prompts))))
      (should (null (plist-get session :permission-mode))))))

(provide 'claude-gravity-test)
;;; claude-gravity-test.el ends here
