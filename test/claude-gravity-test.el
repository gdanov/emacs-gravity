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

(defun cg-test--tool-by-id (sid id)
  "Get the tool entry with ID from SID's tool-index."
  (gethash id (plist-get (cg-test--get sid) :tool-index)))

(defun cg-test--turn-nodes (sid)
  "Get the list of turn nodes for SID."
  (claude-gravity--tlist-items (plist-get (cg-test--get sid) :turns)))

(defun cg-test--turn-prompt (sid turn-number)
  "Get prompt entry for TURN-NUMBER in session SID."
  (let ((turn-node (claude-gravity--get-turn-node (cg-test--get sid) turn-number)))
    (when turn-node (alist-get 'prompt turn-node))))

(defun cg-test--all-prompts (sid)
  "Get list of all prompt entries from the turn tree for SID."
  (let ((result nil))
    (dolist (tn (cg-test--turn-nodes sid))
      (let ((p (alist-get 'prompt tn)))
        (when p (push p result))))
    (nreverse result)))

(defun cg-test--all-root-tools (sid)
  "Collect all root tools across all turns in SID's tree."
  (let ((result nil))
    (dolist (tn (cg-test--turn-nodes sid))
      (dolist (cycle (claude-gravity--tlist-items (alist-get 'cycles tn)))
        (dolist (tool (claude-gravity--tlist-items (alist-get 'tools cycle)))
          (push tool result))))
    (nreverse result)))

(defun cg-test--total-tool-count (sid)
  "Get total tool count from tree for SID."
  (claude-gravity--tree-total-tool-count (cg-test--get sid)))

;;; Tests

(ert-deftest cg-test-user-prompt-advances-turn ()
  "UserPromptSubmit creates a prompt entry and increments current-turn."
  (let ((sid (cg-test--fresh-session "test-1")))
    (cg-test--prompt-submit sid "hello")
    (let ((session (cg-test--get sid)))
      (should (= 1 (plist-get session :current-turn)))
      (let ((prompts (cg-test--all-prompts sid)))
        (should (= 1 (length prompts)))
        (should (equal "hello" (alist-get 'text (car prompts))))))))

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
    (let ((prompts (cg-test--all-prompts sid))
          (session (cg-test--get sid)))
      ;; Two prompts: original + phase-boundary
      (should (= 2 (length prompts)))
      (should (equal "[Plan approved]" (alist-get 'text (nth 1 prompts))))
      (should (eq 'phase-boundary (alist-get 'type (nth 1 prompts))))
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
    (let ((prompts (cg-test--all-prompts sid))
          (session (cg-test--get sid)))
      ;; Two prompts: original + question
      (should (= 2 (length prompts)))
      (should (eq 'question (alist-get 'type (nth 1 prompts))))
      (should (equal "Which approach?" (alist-get 'text (nth 1 prompts))))
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
    (let ((prompts (cg-test--all-prompts sid))
          (session (cg-test--get sid)))
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
    (let ((prompts (cg-test--all-prompts sid)))
      ;; stop_text on the phase-boundary prompt (last one)
      (should (equal "Done" (alist-get 'stop_text (nth 1 prompts)))))))

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
      (should (= 0 (length (cg-test--all-prompts sid))))
      (should (null (plist-get session :permission-mode))))))

;;; Replay tests — feed JSONL transcripts through managed process filter

(defvar cg-test--dir
  (file-name-directory (or load-file-name buffer-file-name
                           "/Users/gdanov/work/playground/emacs-gravity/test/claude-gravity-test.el"))
  "Directory containing test files, captured at load time.")

(require 'cg-test-replay (expand-file-name "replay.el" cg-test--dir))

;;; stop_text.json replay tests

(defun cg-test--replay-stop-text ()
  "Replay test/stop_text.json and return the session-id."
  (clrhash claude-gravity--sessions)
  (clrhash claude-gravity--tmux-sessions)
  (cg-test-replay-transcript (expand-file-name "stop_text.json" cg-test--dir)))

(ert-deftest cg-test-replay-stop-text-session ()
  "stop_text replay creates a session with correct id and idle status."
  (let* ((sid (cg-test--replay-stop-text))
         (session (claude-gravity--get-session sid)))
    (should (equal "test-stop-text" sid))
    (should session)
    (should (eq 'idle (plist-get session :claude-status)))))

(ert-deftest cg-test-replay-stop-text-on-prompt ()
  "Stop event attaches stop_text and stop_thinking to the last prompt."
  (let* ((sid (cg-test--replay-stop-text))
         (prompts (cg-test--all-prompts sid)))
    (should (= 1 (length prompts)))
    (should (equal "All verified, nothing to implement."
                   (alist-get 'stop_text (car prompts))))
    (should (equal "The code looks correct, no changes needed."
                   (alist-get 'stop_thinking (car prompts))))))

(ert-deftest cg-test-replay-stop-text-tool-count ()
  "stop_text replay produces 2 Read tools, both completed."
  (let* ((sid (cg-test--replay-stop-text))
         (tools (cg-test--all-root-tools sid)))
    (should (= 2 (length tools)))
    (should (equal "done" (alist-get 'status (nth 0 tools))))
    (should (equal "done" (alist-get 'status (nth 1 tools))))))

(ert-deftest cg-test-replay-stop-text-assistant-text ()
  "PreToolUse carries assistant_text through to tool entries."
  (let* ((sid (cg-test--replay-stop-text))
         (tools (cg-test--all-root-tools sid)))
    ;; First tool's assistant_text may be cleared by cycle dedup
    ;; but should be accessible via the cycle's text field
    (let* ((turn-node (claude-gravity--get-turn-node (cg-test--get sid) 1))
           (cycles (claude-gravity--tlist-items (alist-get 'cycles turn-node))))
      ;; At least one cycle should have text
      (should (cl-some (lambda (c) (alist-get 'text c)) cycles)))))

(ert-deftest cg-test-replay-stop-text-post-tool-text ()
  "PostToolUse carries post_tool_text through to tool entry."
  (let* ((sid (cg-test--replay-stop-text))
         (tools (cg-test--all-root-tools sid))
         (tool2 (nth 1 tools)))
    (should (equal "Everything checks out."
                   (alist-get 'post_text tool2)))))

(ert-deftest cg-test-replay-stop-text-turn-count ()
  "stop_text replay: single prompt means turn 1, all tools on turn 1."
  (let* ((sid (cg-test--replay-stop-text))
         (session (claude-gravity--get-session sid)))
    (should (= 1 (plist-get session :current-turn)))
    (let ((tools (cg-test--all-root-tools sid)))
      (should (= 1 (alist-get 'turn (nth 0 tools))))
      (should (= 1 (alist-get 'turn (nth 1 tools)))))))

(provide 'claude-gravity-test)
;;; claude-gravity-test.el ends here
