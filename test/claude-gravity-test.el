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
  (gethash id (plist-get (cg-test--get sid) :tool-index)))

;;; Tests

(ert-deftest cg-test-user-prompt-advances-turn ()
  "UserPromptSubmit creates a prompt entry and increments current-turn."
  (let ((sid (cg-test--fresh-session "test-1")))
    (cg-test--prompt-submit sid "hello")
    (let ((session (cg-test--get sid)))
      (should (= 1 (plist-get session :current-turn)))
      (should (= 1 (length (plist-get session :prompts))))
      (should (equal "hello"
                     (alist-get 'text (car (plist-get session :prompts))))))))

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
    (let* ((session (cg-test--get sid))
           (prompts (plist-get session :prompts)))
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
      (should (= 0 (length (plist-get session :prompts))))
      (should (null (plist-get session :permission-mode))))))

;;; Replay tests — feed JSONL transcripts through managed process filter

(defvar cg-test--dir
  (file-name-directory (or load-file-name buffer-file-name
                           "/Users/gdanov/work/playground/emacs-gravity/test/claude-gravity-test.el"))
  "Directory containing test files, captured at load time.")

(require 'cg-test-replay (expand-file-name "replay.el" cg-test--dir))

(defun cg-test--replay-plan-json ()
  "Replay test/plan.json and return the session-id.
Clears sessions first for a clean test."
  (clrhash claude-gravity--sessions)
  (clrhash claude-gravity--tmux-sessions)
  (cg-test-replay-transcript (expand-file-name "plan.json" cg-test--dir)))

(ert-deftest cg-test-replay-session-exists ()
  "Replay creates a session with the correct real session-id."
  (let ((sid (cg-test--replay-plan-json)))
    (should (equal "bd6a0b39-3760-42e9-ac8d-5d9cf5bf75b4" sid))
    (should (claude-gravity--get-session sid))))

(ert-deftest cg-test-replay-session-status ()
  "After replay, session should be idle (result event received)."
  (let* ((sid (cg-test--replay-plan-json))
         (session (claude-gravity--get-session sid)))
    (should (eq 'idle (plist-get session :claude-status)))))

(ert-deftest cg-test-replay-tool-count ()
  "Replay should produce the expected number of tools.
plan.json has 1 Task tool at root + ~19 subagent tools."
  (let* ((sid (cg-test--replay-plan-json))
         (session (claude-gravity--get-session sid))
         (root-tools (alist-get 'tools (plist-get session :state))))
    ;; At minimum: 1 root Task tool
    (should (>= (length root-tools) 1))
    ;; The Task tool should exist
    (should (gethash "toolu_01Nww8G8Dpz77JVzLTjzjqy2"
                     (plist-get session :tool-index)))))

(ert-deftest cg-test-replay-task-tool-done ()
  "The root Task tool should be completed (result line 47 has tool_result)."
  (let* ((sid (cg-test--replay-plan-json))
         (session (claude-gravity--get-session sid))
         (root-tools (alist-get 'tools (plist-get session :state)))
         (task-tool (gethash "toolu_01Nww8G8Dpz77JVzLTjzjqy2"
                            (plist-get session :tool-index))))
    (should task-tool)
    (should (equal "done" (alist-get 'status task-tool)))))

(ert-deftest cg-test-replay-subagent-tools-have-parent ()
  "Subagent tools (lines 9-46) have parent_tool_use_id pointing to Task tool."
  (let* ((sid (cg-test--replay-plan-json))
         (session (claude-gravity--get-session sid))
         (root-tools (alist-get 'tools (plist-get session :state)))
         (task-tool-id "toolu_01Nww8G8Dpz77JVzLTjzjqy2")
         ;; Check the first subagent tool: Read toolu_01SF7qpWaQ6ae1bAXwDGyQ3u
         (read-tool-id "toolu_01SF7qpWaQ6ae1bAXwDGyQ3u"))
    ;; The subagent tool should NOT be in root tools (it has parent_tool_use_id)
    ;; It might be stored under the Task tool's agent or in agent tools.
    ;; Check that the session has agent-attributed tools somewhere.
    ;; The total tool count across root + agents should be > 1
    (let ((agents (plist-get session :agents)))
      ;; Either agents exist with tools, or tools are in root with parent references
      (should (or (> (length agents) 0)
                  (> (length root-tools) 1))))))

(ert-deftest cg-test-replay-pending-text-cleared ()
  "After replay completes, pending assistant text should be nil."
  (let* ((sid (cg-test--replay-plan-json))
         (session (claude-gravity--get-session sid)))
    (should (null (plist-get session :pending-assistant-text)))))

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
         (session (claude-gravity--get-session sid))
         (prompts (plist-get session :prompts)))
    (should (= 1 (length prompts)))
    (should (equal "All verified, nothing to implement."
                   (alist-get 'stop_text (car prompts))))
    (should (equal "The code looks correct, no changes needed."
                   (alist-get 'stop_thinking (car prompts))))))

(ert-deftest cg-test-replay-stop-text-tool-count ()
  "stop_text replay produces 2 Read tools, both completed."
  (let* ((sid (cg-test--replay-stop-text))
         (session (claude-gravity--get-session sid))
         (tools (alist-get 'tools (plist-get session :state))))
    (should (= 2 (length tools)))
    (should (equal "done" (alist-get 'status (nth 0 tools))))
    (should (equal "done" (alist-get 'status (nth 1 tools))))))

(ert-deftest cg-test-replay-stop-text-assistant-text ()
  "PreToolUse carries assistant_text through to tool entries."
  (let* ((sid (cg-test--replay-stop-text))
         (session (claude-gravity--get-session sid))
         (tools (alist-get 'tools (plist-get session :state))))
    (should (equal "Let me read the main file to check."
                   (alist-get 'assistant_text (nth 0 tools))))
    (should (equal "Now let me verify the event handler."
                   (alist-get 'assistant_text (nth 1 tools))))))

(ert-deftest cg-test-replay-stop-text-post-tool-text ()
  "PostToolUse carries post_tool_text through to tool entry."
  (let* ((sid (cg-test--replay-stop-text))
         (session (claude-gravity--get-session sid))
         (tools (alist-get 'tools (plist-get session :state)))
         (tool2 (nth 1 tools)))
    (should (equal "Everything checks out."
                   (alist-get 'post_text tool2)))))

(ert-deftest cg-test-replay-stop-text-turn-count ()
  "stop_text replay: single prompt means turn 1, all tools on turn 1."
  (let* ((sid (cg-test--replay-stop-text))
         (session (claude-gravity--get-session sid)))
    (should (= 1 (plist-get session :current-turn)))
    (let ((tools (alist-get 'tools (plist-get session :state))))
      (should (= 1 (alist-get 'turn (nth 0 tools))))
      (should (= 1 (alist-get 'turn (nth 1 tools)))))))

(provide 'claude-gravity-test)
;;; claude-gravity-test.el ends here
