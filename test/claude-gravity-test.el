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

(ert-deftest cg-test-stop-attaches-to-last-turn ()
  "Stop handler attaches stop_text to the last turn node."
  (let ((sid (cg-test--fresh-session "test-6")))
    (cg-test--prompt-submit sid "plan")
    (cg-test--pre-tool sid "ExitPlanMode" "t1")
    (cg-test--post-tool sid "ExitPlanMode" "t1")
    (cg-test--stop sid (list (cons 'stop_text "Done")))
    ;; stop_text is stored on the turn node (not the prompt)
    (let* ((session (cg-test--get sid))
           (last-turn (claude-gravity--current-turn-node session)))
      (should (equal "Done" (alist-get 'stop_text last-turn))))))

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
      ;; Final turn counter: 3 because AskUserQuestion creates a question
      ;; prompt which advances the turn (prompt-1=1, prompt-2=2, question=3)
      (should (= 3 (plist-get session :current-turn)))
      (should (eq 'idle (plist-get session :claude-status))))

    ;; Verify all tools and their turn attribution
    (let ((all-tools (cg-test--all-root-tools sid)))
      (should (= 2 (length all-tools)))
      ;; First tool from first turn
      (should (= 1 (alist-get 'turn (nth 0 all-tools))))
      (should (equal "t1" (alist-get 'tool_use_id (nth 0 all-tools))))
      ;; AskUserQuestion tool on turn 2 (before it advances to turn 3)
      (should (= 2 (alist-get 'turn (nth 1 all-tools))))
      (should (equal "t2" (alist-get 'tool_use_id (nth 1 all-tools)))))

    ;; Verify turn nodes: 4 total (turn 0 pre-prompt + 3 turns)
    (let ((turn-nodes (cg-test--turn-nodes sid)))
      (should (= 4 (length turn-nodes)))
      ;; Turn 0: pre-prompt activity (SessionStart creates it)
      (let ((turn0 (nth 0 turn-nodes)))
        (should (= 0 (alist-get 'turn-number turn0))))
      (let ((turn1 (nth 1 turn-nodes)))
        (should (= 1 (alist-get 'turn-number turn1)))
        (should (alist-get 'prompt turn1)))
      (let ((turn2 (nth 2 turn-nodes)))
        (should (= 2 (alist-get 'turn-number turn2)))
        (should (alist-get 'prompt turn2)))
      (let ((turn3 (nth 3 turn-nodes)))
        (should (= 3 (alist-get 'turn-number turn3)))
        ;; Turn 3 is the AskUserQuestion question prompt
        (should (eq 'question (alist-get 'type (alist-get 'prompt turn3))))))

    ;; Verify prompts: user prompt, user prompt, question prompt
    (let ((prompts (cg-test--all-prompts sid)))
      (should (= 3 (length prompts)))
      (should (equal "List files in current directory" (alist-get 'text (nth 0 prompts))))
      (should (equal "What is 2 + 2?" (alist-get 'text (nth 1 prompts))))
      (should (equal "2+2=" (alist-get 'text (nth 2 prompts)))))))

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

(setq cg-test--dir
  (file-name-directory (or load-file-name buffer-file-name
                           (expand-file-name "test/claude-gravity-test.el" default-directory))))

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

(ert-deftest cg-test-replay-stop-text-on-turn ()
  "Stop event attaches stop_text and stop_thinking to the last turn node."
  (let* ((sid (cg-test--replay-stop-text))
         (session (cg-test--get sid))
         (last-turn (claude-gravity--current-turn-node session)))
    (should last-turn)
    (should (equal "All verified, nothing to implement."
                   (alist-get 'stop_text last-turn)))
    (should (equal "The code looks correct, no changes needed."
                   (alist-get 'stop_thinking last-turn)))))

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

;;; Multi-agent tests

(defun cg-test--subagent-start (sid agent-id agent-type)
  "Send a SubagentStart event for SID with AGENT-ID and AGENT-TYPE."
  (claude-gravity-handle-event
   "SubagentStart" sid "/tmp/test"
   (list (cons 'agent_id agent-id)
         (cons 'agent_type agent-type))))

(defun cg-test--subagent-stop (sid agent-id)
  "Send a SubagentStop event for SID with AGENT-ID."
  (claude-gravity-handle-event
   "SubagentStop" sid "/tmp/test"
   (list (cons 'agent_id agent-id))))

(defun cg-test--agent-tools (sid agent-id)
  "Collect all tools in AGENT-ID's cycles for session SID."
  (let* ((session (cg-test--get sid))
         (agent (claude-gravity--find-agent session agent-id))
         (result nil))
    (when agent
      (dolist (cycle (claude-gravity--tlist-items (alist-get 'cycles agent)))
        (dolist (tool (claude-gravity--tlist-items (alist-get 'tools cycle)))
          (push tool result))))
    (nreverse result)))

(ert-deftest cg-test-multi-agent-separate-tool-streams ()
  "Two agents in same turn get separate tool streams in the model."
  (let ((sid (cg-test--fresh-session "test-multi-agent")))
    (cg-test--session-start sid)
    (cg-test--prompt-submit sid "Do two things in parallel")

    ;; Claude calls two Task tools (root level, no agents active yet)
    (cg-test--pre-tool sid "Task" "task1"
                       (list (cons 'tool_input
                                   (list (cons 'subagent_type "Explore")
                                         (cons 'description "find X")
                                         (cons 'prompt "find X")))))
    (cg-test--pre-tool sid "Task" "task2"
                       (list (cons 'tool_input
                                   (list (cons 'subagent_type "Explore")
                                         (cons 'description "find Y")
                                         (cons 'prompt "find Y")))))

    ;; Both Task tools should be in root cycles
    (let ((root-tools (cg-test--all-root-tools sid)))
      (should (= 2 (length root-tools)))
      (should (equal "Task" (alist-get 'name (nth 0 root-tools))))
      (should (equal "Task" (alist-get 'name (nth 1 root-tools)))))

    ;; SubagentStart for both agents
    (cg-test--subagent-start sid "agent-aaa" "Explore")
    (cg-test--subagent-start sid "agent-bbb" "Explore")

    ;; Verify agents exist
    (let ((session (cg-test--get sid)))
      (should (claude-gravity--find-agent session "agent-aaa"))
      (should (claude-gravity--find-agent session "agent-bbb")))

    ;; Verify Task tools are linked to agents (bidirectional)
    (let* ((session (cg-test--get sid))
           (agent-a (claude-gravity--find-agent session "agent-aaa"))
           (agent-b (claude-gravity--find-agent session "agent-bbb")))
      ;; Both agents should have task-tool links
      (should (alist-get 'task-tool agent-a))
      (should (alist-get 'task-tool agent-b))
      ;; The task tools should point to different tools
      (should-not (eq (alist-get 'task-tool agent-a)
                      (alist-get 'task-tool agent-b)))
      ;; Each task tool should have an agent pointer back
      (should (alist-get 'agent (alist-get 'task-tool agent-a)))
      (should (alist-get 'agent (alist-get 'task-tool agent-b))))

    ;; Agent A runs tools (parent_agent_id = "agent-aaa")
    (cg-test--pre-tool sid "Glob" "a-t1"
                       (list (cons 'parent_agent_id "agent-aaa")
                             (cons 'tool_input '((pattern . "**/*.ts")))))
    (cg-test--post-tool sid "Glob" "a-t1"
                        (list (cons 'parent_agent_id "agent-aaa")))
    (cg-test--pre-tool sid "Read" "a-t2"
                       (list (cons 'parent_agent_id "agent-aaa")
                             (cons 'tool_input '((file_path . "/src/foo.ts")))))
    (cg-test--post-tool sid "Read" "a-t2"
                        (list (cons 'parent_agent_id "agent-aaa")))

    ;; Agent B runs tools (parent_agent_id = "agent-bbb")
    (cg-test--pre-tool sid "Grep" "b-t1"
                       (list (cons 'parent_agent_id "agent-bbb")
                             (cons 'tool_input '((pattern . "error")))))
    (cg-test--post-tool sid "Grep" "b-t1"
                        (list (cons 'parent_agent_id "agent-bbb")))
    (cg-test--pre-tool sid "Read" "b-t2"
                       (list (cons 'parent_agent_id "agent-bbb")
                             (cons 'tool_input '((file_path . "/src/bar.ts")))))
    (cg-test--post-tool sid "Read" "b-t2"
                        (list (cons 'parent_agent_id "agent-bbb")))

    ;; CRITICAL: Verify tools went to the correct agent, not root
    (let ((agent-a-tools (cg-test--agent-tools sid "agent-aaa"))
          (agent-b-tools (cg-test--agent-tools sid "agent-bbb"))
          (root-tools (cg-test--all-root-tools sid)))
      ;; Root should still have only the 2 Task tools
      (should (= 2 (length root-tools)))
      ;; Agent A should have 2 tools (Glob + Read)
      (should (= 2 (length agent-a-tools)))
      (should (equal "Glob" (alist-get 'name (nth 0 agent-a-tools))))
      (should (equal "Read" (alist-get 'name (nth 1 agent-a-tools))))
      ;; Agent B should have 2 tools (Grep + Read)
      (should (= 2 (length agent-b-tools)))
      (should (equal "Grep" (alist-get 'name (nth 0 agent-b-tools))))
      (should (equal "Read" (alist-get 'name (nth 1 agent-b-tools))))

      ;; Verify agents have correct tool counts
      (let* ((session (cg-test--get sid))
             (agent-a (claude-gravity--find-agent session "agent-aaa"))
             (agent-b (claude-gravity--find-agent session "agent-bbb")))
        (should (= 2 (alist-get 'tool-count agent-a)))
        (should (= 2 (alist-get 'tool-count agent-b)))))))

(ert-deftest cg-test-multi-agent-rendering-has-separate-branches ()
  "Rendering of multi-agent turn produces separate agent branches."
  (let ((sid (cg-test--fresh-session "test-multi-render")))
    (cg-test--session-start sid)
    (cg-test--prompt-submit sid "Parallel tasks")

    ;; Two Task tools
    (cg-test--pre-tool sid "Task" "task1"
                       (list (cons 'tool_input
                                   (list (cons 'subagent_type "Explore")
                                         (cons 'description "find X")))))
    (cg-test--pre-tool sid "Task" "task2"
                       (list (cons 'tool_input
                                   (list (cons 'subagent_type "general-purpose")
                                         (cons 'description "research Y")))))

    ;; SubagentStart for both
    (cg-test--subagent-start sid "agent-exp" "Explore")
    (cg-test--subagent-start sid "agent-gp" "general-purpose")

    ;; Agent tools
    (cg-test--pre-tool sid "Glob" "e-t1"
                       (list (cons 'parent_agent_id "agent-exp")))
    (cg-test--post-tool sid "Glob" "e-t1"
                        (list (cons 'parent_agent_id "agent-exp")))
    (cg-test--pre-tool sid "Read" "g-t1"
                       (list (cons 'parent_agent_id "agent-gp")))
    (cg-test--post-tool sid "Read" "g-t1"
                        (list (cons 'parent_agent_id "agent-gp")))

    ;; Complete agents
    (cg-test--subagent-stop sid "agent-exp")
    (cg-test--subagent-stop sid "agent-gp")
    (cg-test--post-tool sid "Task" "task1")
    (cg-test--post-tool sid "Task" "task2")

    ;; Render to a buffer and inspect output
    (let* ((session (cg-test--get sid))
           (buf (generate-new-buffer " *test-multi-render*")))
      (unwind-protect
          (with-current-buffer buf
            (let ((inhibit-read-only t))
              (magit-insert-section (root)
                (claude-gravity-insert-turns session)))
            (let ((content (buffer-string)))
              ;; Both agent types should appear in the rendered output
              (should (string-match-p "Explore" content))
              (should (string-match-p "general-purpose" content))
              ;; Should see the robot emoji for agent branches
              (should (string-match-p "🤖" content))))
        (kill-buffer buf)))))

(ert-deftest cg-test-multi-agent-ambiguous-goes-to-root ()
  "Tools with parent_agent_id='ambiguous' go to root, not agent cycles."
  (let ((sid (cg-test--fresh-session "test-ambiguous")))
    (cg-test--session-start sid)
    (cg-test--prompt-submit sid "work")

    ;; Task tool and agent
    (cg-test--pre-tool sid "Task" "task1"
                       (list (cons 'tool_input
                                   (list (cons 'subagent_type "Explore")))))
    (cg-test--subagent-start sid "agent-x" "Explore")

    ;; Tool with ambiguous attribution
    (cg-test--pre-tool sid "Glob" "amb-t1"
                       (list (cons 'parent_agent_id "ambiguous")
                             (cons 'candidate_agent_ids '("agent-x"))))

    ;; Ambiguous tool should be in root, not agent
    (let ((root-tools (cg-test--all-root-tools sid))
          (agent-tools (cg-test--agent-tools sid "agent-x")))
      ;; Root: Task + ambiguous Glob = 2
      (should (= 2 (length root-tools)))
      ;; Agent: empty
      (should (= 0 (length agent-tools))))))

(provide 'claude-gravity-test)
;;; claude-gravity-test.el ends here
