;;; claude-gravity-test.el --- ERT tests for turn demarcation -*- lexical-binding: t; -*-

(require 'ert)
(require 'claude-gravity)

;; Forward declaration for replay tests
(defvar cg-test--dir nil "Directory containing test files.")

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

;;; Tmux session turn tracking tests

(ert-deftest cg-test-tmux-manual-prompt-creates-new-turn ()
  "Manual user prompt in tmux session creates a new turn.
This test simulates the flow:
1. Emacs starts a tmux session and sends first prompt (via tmux send-keys)
2. First turn completes (UserPromptSubmit + tools + Stop)
3. User manually sends second prompt in attached tmux session
4. Verify second turn is created with correct turn number and tool attribution."
  (let ((sid (cg-test--fresh-session "test-tmux-manual")))
    ;; === Phase 1: Setup ===
    ;; Session starts (would be via SessionStart hook in real scenario)
    (cg-test--session-start sid)
    (should (= 0 (plist-get (cg-test--get sid) :current-turn)))

    ;; === Phase 2: First Turn (Emacs-initiated) ===
    ;; Simulate: claude-gravity-send-prompt sends prompt via tmux send-keys
    ;; which eventually fires UserPromptSubmit hook
    (cg-test--prompt-submit sid "List files in current directory")
    (should (= 1 (plist-get (cg-test--get sid) :current-turn)))

    ;; Tool execution for first turn
    (cg-test--pre-tool sid "Bash" "t1"
                       (list (cons 'tool_input '((command . "ls")))))
    (should (= 1 (alist-get 'turn (cg-test--tool-by-id sid "t1"))))

    (cg-test--post-tool sid "Bash" "t1")
    (cg-test--stop sid (list (cons 'stop_text "Listed files successfully")))

    ;; Verify first turn state
    (let ((session (cg-test--get sid)))
      (should (= 1 (plist-get session :current-turn)))
      (should (eq 'idle (plist-get session :claude-status))))

    ;; Verify first turn tools
    (let ((tools (cg-test--all-root-tools sid)))
      (should (= 1 (length tools)))
      (should (= 1 (alist-get 'turn (car tools))))
      (should (equal "t1" (alist-get 'tool_use_id (car tools)))))

    ;; === Phase 3: Second Turn (Manual user input) ===
    ;; User attaches to tmux session and manually sends a prompt
    ;; This also fires UserPromptSubmit hook, but from manual input
    (cg-test--prompt-submit sid "What is 2 + 2?")

    ;; CRITICAL: Verify turn counter advanced to 2
    (should (= 2 (plist-get (cg-test--get sid) :current-turn)))

    ;; Tool execution for second turn
    (cg-test--pre-tool sid "AskUserQuestion" "t2"
                       (list (cons 'tool_input
                                   (list (cons 'questions
                                               (vector (list (cons 'question "2+2="))))))))
    (should (= 2 (alist-get 'turn (cg-test--tool-by-id sid "t2"))))

    (cg-test--post-tool sid "AskUserQuestion" "t2"
                        (list (cons 'tool_response "4")))
    (cg-test--stop sid (list (cons 'stop_text "The answer is 4")))

    ;; === Phase 4: Verify Turn Separation ===
    (let ((session (cg-test--get sid)))
      ;; Verify final turn counter
      (should (= 2 (plist-get session :current-turn)))
      (should (eq 'idle (plist-get session :claude-status))))

    ;; Verify all tools and their turn attribution
    (let ((all-tools (cg-test--all-root-tools sid)))
      (should (= 2 (length all-tools)))
      ;; First tool from first turn
      (should (= 1 (alist-get 'turn (nth 0 all-tools))))
      (should (equal "t1" (alist-get 'tool_use_id (nth 0 all-tools))))
      ;; Second tool from second turn
      (should (= 2 (alist-get 'turn (nth 1 all-tools))))
      (should (equal "t2" (alist-get 'tool_use_id (nth 1 all-tools)))))

    ;; Verify turn nodes exist and are separate
    (let ((turn-nodes (cg-test--turn-nodes sid)))
      (should (= 2 (length turn-nodes)))
      ;; Turn 1
      (let ((turn1 (nth 0 turn-nodes)))
        (should (= 1 (alist-get 'turn-number turn1)))
        (should (alist-get 'prompt turn1)))
      ;; Turn 2
      (let ((turn2 (nth 1 turn-nodes)))
        (should (= 2 (alist-get 'turn-number turn2)))
        (should (alist-get 'prompt turn2))))

    ;; Verify prompts are correct
    (let ((prompts (cg-test--all-prompts sid)))
      (should (= 2 (length prompts)))
      (should (equal "List files in current directory" (alist-get 'text (nth 0 prompts))))
      (should (equal "What is 2 + 2?" (alist-get 'text (nth 1 prompts)))))))

(ert-deftest cg-test-tmux-manual-prompt-rapid-succession ()
  "Multiple manual prompts in rapid succession track turns correctly.
Edge case: What if user sends multiple prompts without waiting for completion?"
  (let ((sid (cg-test--fresh-session "test-tmux-rapid")))
    (cg-test--session-start sid)

    ;; Send multiple prompts without full turn completion
    (cg-test--prompt-submit sid "First question")
    (should (= 1 (plist-get (cg-test--get sid) :current-turn)))

    (cg-test--pre-tool sid "Read" "t1")
    (cg-test--post-tool sid "Read" "t1")

    ;; Send second prompt before Stop fires
    (cg-test--prompt-submit sid "Second question")
    (should (= 2 (plist-get (cg-test--get sid) :current-turn)))

    ;; Now send second Stop
    (cg-test--stop sid)

    ;; Send third prompt
    (cg-test--prompt-submit sid "Third question")
    (should (= 3 (plist-get (cg-test--get sid) :current-turn)))

    ;; Verify all tools are correctly attributed
    (let ((tools (cg-test--all-root-tools sid)))
      (should (= 1 (length tools)))
      (should (= 1 (alist-get 'turn (car tools)))))

    ;; Verify all prompts exist
    (let ((prompts (cg-test--all-prompts sid)))
      (should (= 3 (length prompts)))
      (should (equal "First question" (alist-get 'text (nth 0 prompts))))
      (should (equal "Second question" (alist-get 'text (nth 1 prompts))))
      (should (equal "Third question" (alist-get 'text (nth 2 prompts)))))))

(ert-deftest cg-test-tmux-manual-prompt-no-duplicate-prompts ()
  "When UserPromptSubmit fires for manual input, no duplicate prompts created.
The tmux-prompt-sent flag should prevent duplicates."
  (let ((sid (cg-test--fresh-session "test-tmux-dedup")))
    (cg-test--session-start sid)

    ;; First prompt (simulating Emacs-initiated via send-prompt)
    ;; In real code, this sets tmux-prompt-sent flag
    (let ((session (cg-test--get sid)))
      (plist-put session :tmux-prompt-sent nil))
    (cg-test--prompt-submit sid "From Emacs")
    (should (= 1 (plist-get (cg-test--get sid) :current-turn)))

    ;; Second prompt (simulating manual user input)
    ;; This should also create exactly one new prompt
    (cg-test--prompt-submit sid "From manual")
    (should (= 2 (plist-get (cg-test--get sid) :current-turn)))

    ;; Verify exactly 2 prompts (no duplicates)
    (let ((prompts (cg-test--all-prompts sid)))
      (should (= 2 (length prompts)))
      (should (equal "From Emacs" (alist-get 'text (nth 0 prompts))))
      (should (equal "From manual" (alist-get 'text (nth 1 prompts)))))))

;;; Replay tests — feed JSONL transcripts through managed process filter

(defvar cg-test--dir
  (file-name-directory (or load-file-name buffer-file-name
                           "/Users/gdanov/work/playground/emacs-gravity/test/claude-gravity-test.el"))
  "Directory containing test files, captured at load time.")

(require 'cg-test-replay nil :noerror)

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
