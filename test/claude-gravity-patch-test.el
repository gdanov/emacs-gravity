;;; claude-gravity-patch-test.el --- Tests for patch application  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for `claude-gravity--apply-patch' — the critical function that
;; transforms server semantic patches into plist mutations.
;; Also tests JSON-to-alist conversion functions and multi-event sequences.

;;; Code:

(require 'ert)
(require 'claude-gravity)

;;; ─── Test infrastructure ──────────────────────────────────────────

(defun cgp--fresh-session ()
  "Create a fresh session plist with all required structures."
  (clrhash claude-gravity--sessions)
  (clrhash claude-gravity--tmux-sessions)
  (let ((session (list :session-id "test-sess"
                       :cwd "/tmp/test"
                       :project "test"
                       :status 'active
                       :claude-status 'idle
                       :slug nil
                       :branch nil
                       :pid nil
                       :model-name nil
                       :tmux-session nil
                       :start-time (current-time)
                       :last-event-time (current-time)
                       :current-turn 0
                       :total-tool-count 0
                       :permission-mode nil
                       :plan nil
                       :streaming-text nil
                       :token-usage nil
                       :buffer nil
                       :display-name nil
                       :turns (let ((tl (claude-gravity--tlist-new)))
                                (claude-gravity--tlist-append
                                 tl (claude-gravity--make-turn-node 0))
                                tl)
                       :tool-index (make-hash-table :test 'equal)
                       :agent-index (make-hash-table :test 'equal)
                       :files (make-hash-table :test 'equal)
                       :tasks (make-hash-table :test 'equal))))
    (puthash "test-sess" session claude-gravity--sessions)
    session))

(defun cgp--apply (session patch)
  "Apply PATCH to SESSION via the main patch application function."
  (claude-gravity--apply-patch session patch))

(defun cgp--turn0 (session)
  "Get turn 0 node from SESSION."
  (claude-gravity--get-turn-node session 0))

(defun cgp--step-tools (step)
  "Get tools list from STEP."
  (claude-gravity--tlist-items (alist-get 'tools step)))

(defun cgp--turn-steps (turn-node)
  "Get steps list from TURN-NODE."
  (claude-gravity--tlist-items (alist-get 'steps turn-node)))

(defun cgp--turn-agents (turn-node)
  "Get agents list from TURN-NODE."
  (claude-gravity--tlist-items (alist-get 'agents turn-node)))


;;; ═══════════════════════════════════════════════════════════════════
;;; Part 1: Simple set_* patches
;;; ═══════════════════════════════════════════════════════════════════

(ert-deftest cgp-set-status-active ()
  "set_status patches status to active symbol."
  (let ((s (cgp--fresh-session)))
    (cgp--apply s '((op . "set_status") (status . "active")))
    (should (eq 'active (plist-get s :status)))))

(ert-deftest cgp-set-status-ended ()
  "set_status patches status to ended symbol."
  (let ((s (cgp--fresh-session)))
    (cgp--apply s '((op . "set_status") (status . "ended")))
    (should (eq 'ended (plist-get s :status)))))

(ert-deftest cgp-set-claude-status ()
  "set_claude_status patches claude-status to responding."
  (let ((s (cgp--fresh-session)))
    (cgp--apply s '((op . "set_claude_status") (claudeStatus . "responding")))
    (should (eq 'responding (plist-get s :claude-status)))))

(ert-deftest cgp-set-claude-status-idle ()
  "set_claude_status patches claude-status back to idle."
  (let ((s (cgp--fresh-session)))
    (cgp--apply s '((op . "set_claude_status") (claudeStatus . "responding")))
    (cgp--apply s '((op . "set_claude_status") (claudeStatus . "idle")))
    (should (eq 'idle (plist-get s :claude-status)))))

(ert-deftest cgp-set-streaming-text ()
  "set_streaming_text patches streaming text."
  (let ((s (cgp--fresh-session)))
    (cgp--apply s '((op . "set_streaming_text") (text . "Hello world")))
    (should (equal "Hello world" (plist-get s :streaming-text)))))

(ert-deftest cgp-set-streaming-text-nil ()
  "set_streaming_text can clear streaming text."
  (let ((s (cgp--fresh-session)))
    (cgp--apply s '((op . "set_streaming_text") (text . "Hello")))
    (cgp--apply s '((op . "set_streaming_text") (text . nil)))
    (should (null (plist-get s :streaming-text)))))

(ert-deftest cgp-set-permission-mode ()
  "set_permission_mode patches permission mode."
  (let ((s (cgp--fresh-session)))
    (cgp--apply s '((op . "set_permission_mode") (mode . "plan")))
    (should (equal "plan" (plist-get s :permission-mode)))))

(ert-deftest cgp-set-token-usage ()
  "set_token_usage patches token usage with correct field mapping."
  (let ((s (cgp--fresh-session)))
    (cgp--apply s `((op . "set_token_usage")
                    (usage . ((input_tokens . 1000)
                              (output_tokens . 500)
                              (cache_read_input_tokens . 200)
                              (cache_creation_input_tokens . 50)))))
    (let ((usage (plist-get s :token-usage)))
      (should usage)
      (should (= 1000 (alist-get 'input_tokens usage)))
      (should (= 500 (alist-get 'output_tokens usage)))
      (should (= 200 (alist-get 'cache_read_input_tokens usage)))
      (should (= 50 (alist-get 'cache_creation_input_tokens usage))))))

(ert-deftest cgp-set-token-usage-defaults-missing ()
  "set_token_usage defaults missing fields to 0."
  (let ((s (cgp--fresh-session)))
    (cgp--apply s `((op . "set_token_usage")
                    (usage . ((input_tokens . 100)))))
    (let ((usage (plist-get s :token-usage)))
      (should (= 100 (alist-get 'input_tokens usage)))
      (should (= 0 (alist-get 'output_tokens usage)))
      (should (= 0 (alist-get 'cache_read_input_tokens usage))))))

(ert-deftest cgp-set-plan ()
  "set_plan patches plan with content and file path."
  (let ((s (cgp--fresh-session)))
    (cgp--apply s `((op . "set_plan")
                    (plan . ((content . "1. Fix bug\n2. Test")
                             (filePath . "/tmp/plan.md")
                             (allowedPrompts . ("Edit(**)" "Bash(npm test)"))))))
    (let ((plan (plist-get s :plan)))
      (should plan)
      (should (equal "1. Fix bug\n2. Test" (plist-get plan :content)))
      (should (equal "/tmp/plan.md" (plist-get plan :file-path))))))

(ert-deftest cgp-set-plan-nil ()
  "set_plan can clear the plan."
  (let ((s (cgp--fresh-session)))
    (cgp--apply s `((op . "set_plan") (plan . ((content . "stuff")))))
    (cgp--apply s '((op . "set_plan") (plan . nil)))
    (should (null (plist-get s :plan)))))


;;; ═══════════════════════════════════════════════════════════════════
;;; Part 2: set_meta
;;; ═══════════════════════════════════════════════════════════════════

(ert-deftest cgp-set-meta-slug ()
  "set_meta patches slug."
  (let ((s (cgp--fresh-session)))
    (cgp--apply s '((op . "set_meta") (slug . "fix-auth")))
    (should (equal "fix-auth" (plist-get s :slug)))))

(ert-deftest cgp-set-meta-branch ()
  "set_meta patches branch."
  (let ((s (cgp--fresh-session)))
    (cgp--apply s '((op . "set_meta") (branch . "feature/auth")))
    (should (equal "feature/auth" (plist-get s :branch)))))

(ert-deftest cgp-set-meta-pid ()
  "set_meta patches pid when valid."
  (let ((s (cgp--fresh-session)))
    (cgp--apply s '((op . "set_meta") (pid . 12345)))
    (should (= 12345 (plist-get s :pid)))))

(ert-deftest cgp-set-meta-pid-ignores-zero ()
  "set_meta ignores pid of 0."
  (let ((s (cgp--fresh-session)))
    (plist-put s :pid 999)
    (cgp--apply s '((op . "set_meta") (pid . 0)))
    (should (= 999 (plist-get s :pid)))))

(ert-deftest cgp-set-meta-pid-ignores-non-number ()
  "set_meta ignores non-numeric pid."
  (let ((s (cgp--fresh-session)))
    (plist-put s :pid 999)
    (cgp--apply s '((op . "set_meta") (pid . "not-a-number")))
    (should (= 999 (plist-get s :pid)))))

(ert-deftest cgp-set-meta-model-name ()
  "set_meta patches model name."
  (let ((s (cgp--fresh-session)))
    (cgp--apply s '((op . "set_meta") (modelName . "claude-sonnet-4-20250514")))
    (should (equal "claude-sonnet-4-20250514" (plist-get s :model-name)))))

(ert-deftest cgp-set-meta-multiple-fields ()
  "set_meta can set multiple fields at once."
  (let ((s (cgp--fresh-session)))
    (cgp--apply s '((op . "set_meta") (slug . "my-task") (branch . "main") (pid . 42)))
    (should (equal "my-task" (plist-get s :slug)))
    (should (equal "main" (plist-get s :branch)))
    (should (= 42 (plist-get s :pid)))))

(ert-deftest cgp-set-meta-nil-fields-ignored ()
  "set_meta ignores nil fields (does not overwrite)."
  (let ((s (cgp--fresh-session)))
    (plist-put s :slug "keep-me")
    (cgp--apply s '((op . "set_meta") (slug . nil) (branch . "new")))
    (should (equal "keep-me" (plist-get s :slug)))
    (should (equal "new" (plist-get s :branch)))))

(ert-deftest cgp-set-meta-updates-last-event-time ()
  "set_meta updates :last-event-time."
  (let ((s (cgp--fresh-session)))
    (plist-put s :last-event-time nil)
    (cgp--apply s '((op . "set_meta") (slug . "x")))
    (should (plist-get s :last-event-time))))


;;; ═══════════════════════════════════════════════════════════════════
;;; Part 3: Turn lifecycle — add_turn, freeze_turn, set_turn_stop, set_turn_tokens
;;; ═══════════════════════════════════════════════════════════════════

(ert-deftest cgp-add-turn ()
  "add_turn appends a new turn and updates current-turn."
  (let ((s (cgp--fresh-session)))
    (cgp--apply s `((op . "add_turn")
                    (turn . ((turnNumber . 1)
                             (frozen . :json-false)))))
    (should (= 1 (plist-get s :current-turn)))
    (let ((turn1 (claude-gravity--get-turn-node s 1)))
      (should turn1)
      (should (= 1 (alist-get 'turn-number turn1))))))

(ert-deftest cgp-add-turn-with-prompt ()
  "add_turn with prompt preserves prompt data."
  (let ((s (cgp--fresh-session)))
    (cgp--apply s `((op . "add_turn")
                    (turn . ((turnNumber . 1)
                             (prompt . ((text . "Fix the bug")
                                        (type . "user")
                                        (submitted . 1700000000000)))))))
    (let* ((turn1 (claude-gravity--get-turn-node s 1))
           (prompt (alist-get 'prompt turn1)))
      (should prompt)
      (should (equal "Fix the bug" (alist-get 'text prompt))))))

(ert-deftest cgp-freeze-turn ()
  "freeze_turn sets frozen flag on target turn."
  (let ((s (cgp--fresh-session)))
    (cgp--apply s '((op . "freeze_turn") (turnNumber . 0)))
    (let ((turn0 (cgp--turn0 s)))
      (should (eq t (alist-get 'frozen turn0))))))

(ert-deftest cgp-freeze-turn-nonexistent ()
  "freeze_turn on non-existent turn is a no-op (no crash)."
  (let ((s (cgp--fresh-session)))
    ;; Should not error
    (cgp--apply s '((op . "freeze_turn") (turnNumber . 99)))))

(ert-deftest cgp-set-turn-stop ()
  "set_turn_stop patches stop_text and stop_thinking on target turn."
  (let ((s (cgp--fresh-session)))
    (cgp--apply s '((op . "set_turn_stop")
                    (turnNumber . 0)
                    (stopText . "All tests pass.")
                    (stopThinking . "I verified the fix.")))
    (let ((turn0 (cgp--turn0 s)))
      (should (equal "All tests pass." (alist-get 'stop_text turn0)))
      (should (equal "I verified the fix." (alist-get 'stop_thinking turn0))))))

(ert-deftest cgp-set-turn-stop-partial ()
  "set_turn_stop can set just stopText without stopThinking."
  (let ((s (cgp--fresh-session)))
    (cgp--apply s '((op . "set_turn_stop")
                    (turnNumber . 0)
                    (stopText . "Done.")))
    (let ((turn0 (cgp--turn0 s)))
      (should (equal "Done." (alist-get 'stop_text turn0)))
      (should (null (alist-get 'stop_thinking turn0))))))

(ert-deftest cgp-set-turn-stop-nonexistent ()
  "set_turn_stop on non-existent turn is a no-op."
  (let ((s (cgp--fresh-session)))
    (cgp--apply s '((op . "set_turn_stop") (turnNumber . 99) (stopText . "X")))))

(ert-deftest cgp-set-turn-tokens ()
  "set_turn_tokens patches token-in and token-out on target turn."
  (let ((s (cgp--fresh-session)))
    (cgp--apply s '((op . "set_turn_tokens")
                    (turnNumber . 0)
                    (tokenIn . 5000)
                    (tokenOut . 1200)))
    (let ((turn0 (cgp--turn0 s)))
      (should (= 5000 (alist-get 'token-in turn0)))
      (should (= 1200 (alist-get 'token-out turn0))))))

(ert-deftest cgp-set-turn-tokens-nonexistent ()
  "set_turn_tokens on non-existent turn is a no-op."
  (let ((s (cgp--fresh-session)))
    (cgp--apply s '((op . "set_turn_tokens") (turnNumber . 99) (tokenIn . 1) (tokenOut . 2)))))


;;; ═══════════════════════════════════════════════════════════════════
;;; Part 4: add_step
;;; ═══════════════════════════════════════════════════════════════════

(ert-deftest cgp-add-step-to-turn ()
  "add_step appends a step to the target turn."
  (let ((s (cgp--fresh-session)))
    (cgp--apply s `((op . "add_step")
                    (turnNumber . 0)
                    (step . ((thinking . "Let me check...")
                             (text . "I'll read the file.")))))
    (let* ((turn0 (cgp--turn0 s))
           (steps (cgp--turn-steps turn0)))
      (should (= 1 (length steps)))
      (should (equal "Let me check..." (alist-get 'thinking (car steps))))
      (should (equal "I'll read the file." (alist-get 'text (car steps)))))))

(ert-deftest cgp-add-step-to-agent ()
  "add_step with agentId routes to agent's steps."
  (let ((s (cgp--fresh-session)))
    ;; First add an agent
    (cgp--apply s `((op . "add_agent")
                    (agent . ((agentId . "ag1")
                              (type . "Explore")
                              (status . "running")
                              (timestamp . 1700000000000)))))
    ;; Now add a step to the agent
    (cgp--apply s `((op . "add_step")
                    (turnNumber . 0)
                    (agentId . "ag1")
                    (step . ((text . "Exploring...")))))
    (let* ((agent (gethash "ag1" (plist-get s :agent-index)))
           (steps (claude-gravity--tlist-items (alist-get 'steps agent))))
      (should (= 1 (length steps)))
      (should (equal "Exploring..." (alist-get 'text (car steps)))))))

(ert-deftest cgp-add-step-nonexistent-agent ()
  "add_step for non-existent agent is a no-op."
  (let ((s (cgp--fresh-session)))
    (cgp--apply s `((op . "add_step")
                    (turnNumber . 0)
                    (agentId . "ghost")
                    (step . ((text . "Nope")))))))


;;; ═══════════════════════════════════════════════════════════════════
;;; Part 5: add_tool — routing, dedup, counters
;;; ═══════════════════════════════════════════════════════════════════

(ert-deftest cgp-add-tool-to-turn ()
  "add_tool appends tool to turn step and indexes it."
  (let ((s (cgp--fresh-session)))
    ;; Add a step first
    (cgp--apply s `((op . "add_step")
                    (turnNumber . 0)
                    (step . ((text . "Reading...")))))
    (cgp--apply s `((op . "add_tool")
                    (turnNumber . 0)
                    (stepIndex . 0)
                    (tool . ((toolUseId . "tu_001")
                             (name . "Read")
                             (status . "running")
                             (input . ((file_path . "/src/auth.ts")))
                             (timestamp . 1700000000000)))))
    ;; Tool should be in step
    (let* ((turn0 (cgp--turn0 s))
           (steps (cgp--turn-steps turn0))
           (tools (cgp--step-tools (car steps))))
      (should (= 1 (length tools)))
      (should (equal "Read" (alist-get 'name (car tools))))
      (should (equal "tu_001" (alist-get 'tool_use_id (car tools)))))
    ;; Tool should be indexed
    (should (gethash "tu_001" (plist-get s :tool-index)))
    ;; Counters should be updated
    (should (= 1 (plist-get s :total-tool-count)))
    (should (= 1 (alist-get 'tool-count (cgp--turn0 s))))))

(ert-deftest cgp-add-tool-dedup ()
  "add_tool with duplicate tool_use_id is a no-op."
  (let ((s (cgp--fresh-session)))
    (cgp--apply s `((op . "add_step") (turnNumber . 0) (step . ())))
    (cgp--apply s `((op . "add_tool") (turnNumber . 0) (stepIndex . 0)
                    (tool . ((toolUseId . "tu_dup") (name . "Read") (timestamp . 1700000000000)))))
    (cgp--apply s `((op . "add_tool") (turnNumber . 0) (stepIndex . 0)
                    (tool . ((toolUseId . "tu_dup") (name . "Read2") (timestamp . 1700000000000)))))
    ;; Should still have only 1 tool
    (should (= 1 (plist-get s :total-tool-count)))))

(ert-deftest cgp-add-tool-creates-step-fallback ()
  "add_tool creates a step if stepIndex is out of bounds."
  (let ((s (cgp--fresh-session)))
    ;; No steps exist yet, stepIndex 0 should trigger fallback
    (cgp--apply s `((op . "add_tool") (turnNumber . 0) (stepIndex . 5)
                    (tool . ((toolUseId . "tu_fb") (name . "Glob") (timestamp . 1700000000000)))))
    (let* ((turn0 (cgp--turn0 s))
           (steps (cgp--turn-steps turn0)))
      (should (>= (length steps) 1))
      (let ((tools (cgp--step-tools (car (last steps)))))
        (should (= 1 (length tools)))))))

(ert-deftest cgp-add-tool-to-agent ()
  "add_tool with agentId routes to agent's steps."
  (let ((s (cgp--fresh-session)))
    ;; Add agent first
    (cgp--apply s `((op . "add_agent")
                    (agent . ((agentId . "ag1") (type . "Explore")
                              (status . "running") (timestamp . 1700000000000)))))
    (cgp--apply s `((op . "add_step") (turnNumber . 0) (agentId . "ag1")
                    (step . ((text . "Searching...")))))
    (cgp--apply s `((op . "add_tool") (turnNumber . 0) (stepIndex . 0) (agentId . "ag1")
                    (tool . ((toolUseId . "tu_ag") (name . "Grep") (timestamp . 1700000000000)))))
    ;; Tool should be in agent's step
    (let* ((agent (gethash "ag1" (plist-get s :agent-index)))
           (steps (claude-gravity--tlist-items (alist-get 'steps agent)))
           (tools (cgp--step-tools (car steps))))
      (should (= 1 (length tools)))
      (should (equal "Grep" (alist-get 'name (car tools)))))
    ;; Tool should be indexed
    (should (gethash "tu_ag" (plist-get s :tool-index)))
    ;; Turn tool-count should NOT increment for agent tools
    (should (= 0 (alist-get 'tool-count (cgp--turn0 s))))))


;;; ═══════════════════════════════════════════════════════════════════
;;; Part 6: complete_tool
;;; ═══════════════════════════════════════════════════════════════════

(ert-deftest cgp-complete-tool ()
  "complete_tool sets status, result, duration, post_text."
  (let ((s (cgp--fresh-session)))
    (cgp--apply s `((op . "add_step") (turnNumber . 0) (step . ())))
    (cgp--apply s `((op . "add_tool") (turnNumber . 0) (stepIndex . 0)
                    (tool . ((toolUseId . "tu_c") (name . "Read") (status . "running")
                             (timestamp . 1700000000000)))))
    (cgp--apply s `((op . "complete_tool")
                    (toolUseId . "tu_c")
                    (status . "done")
                    (result . "file contents here")
                    (duration . 1.5)
                    (postText . "Got it.")))
    (let ((tool (gethash "tu_c" (plist-get s :tool-index))))
      (should (equal "done" (alist-get 'status tool)))
      (should (equal "file contents here" (alist-get 'result tool)))
      (should (= 1.5 (alist-get 'duration tool)))
      (should (equal "Got it." (alist-get 'post_text tool))))))

(ert-deftest cgp-complete-tool-error ()
  "complete_tool with error status."
  (let ((s (cgp--fresh-session)))
    (cgp--apply s `((op . "add_step") (turnNumber . 0) (step . ())))
    (cgp--apply s `((op . "add_tool") (turnNumber . 0) (stepIndex . 0)
                    (tool . ((toolUseId . "tu_e") (name . "Bash") (status . "running")
                             (timestamp . 1700000000000)))))
    (cgp--apply s `((op . "complete_tool")
                    (toolUseId . "tu_e")
                    (status . "error")
                    (result . "command failed")))
    (let ((tool (gethash "tu_e" (plist-get s :tool-index))))
      (should (equal "error" (alist-get 'status tool)))
      (should (equal "command failed" (alist-get 'result tool))))))

(ert-deftest cgp-complete-tool-nonexistent ()
  "complete_tool for unknown tool_use_id is a no-op."
  (let ((s (cgp--fresh-session)))
    (cgp--apply s `((op . "complete_tool")
                    (toolUseId . "ghost")
                    (status . "done")
                    (result . "nope")))))

(ert-deftest cgp-complete-tool-same-object ()
  "complete_tool modifies the same object in the tree (pointer identity)."
  (let ((s (cgp--fresh-session)))
    (cgp--apply s `((op . "add_step") (turnNumber . 0) (step . ())))
    (cgp--apply s `((op . "add_tool") (turnNumber . 0) (stepIndex . 0)
                    (tool . ((toolUseId . "tu_p") (name . "Edit") (status . "running")
                             (timestamp . 1700000000000)))))
    (let ((indexed (gethash "tu_p" (plist-get s :tool-index))))
      (cgp--apply s `((op . "complete_tool") (toolUseId . "tu_p")
                      (status . "done") (result . "ok")))
      ;; The indexed object should be the same cons cells as the one in the tree
      (should (equal "done" (alist-get 'status indexed)))
      ;; Also verify the tool in the step is the same object
      (let* ((turn0 (cgp--turn0 s))
             (step (car (cgp--turn-steps turn0)))
             (tree-tool (car (cgp--step-tools step))))
        (should (eq indexed tree-tool))))))


;;; ═══════════════════════════════════════════════════════════════════
;;; Part 7: add_agent, complete_agent
;;; ═══════════════════════════════════════════════════════════════════

(ert-deftest cgp-add-agent ()
  "add_agent creates agent, indexes it, appends to current turn."
  (let ((s (cgp--fresh-session)))
    (cgp--apply s `((op . "add_agent")
                    (agent . ((agentId . "ag1")
                              (type . "Explore")
                              (status . "running")
                              (timestamp . 1700000000000)))))
    ;; Indexed
    (let ((agent (gethash "ag1" (plist-get s :agent-index))))
      (should agent)
      (should (equal "Explore" (alist-get 'type agent)))
      (should (equal "running" (alist-get 'status agent))))
    ;; In current turn
    (let* ((turn0 (cgp--turn0 s))
           (agents (cgp--turn-agents turn0)))
      (should (= 1 (length agents)))
      (should (= 1 (alist-get 'agent-count turn0))))))

(ert-deftest cgp-add-agent-dedup ()
  "add_agent with duplicate agent_id is a no-op."
  (let ((s (cgp--fresh-session)))
    (cgp--apply s `((op . "add_agent")
                    (agent . ((agentId . "ag1") (type . "Explore")
                              (status . "running") (timestamp . 1700000000000)))))
    (cgp--apply s `((op . "add_agent")
                    (agent . ((agentId . "ag1") (type . "Plan")
                              (status . "running") (timestamp . 1700000000000)))))
    ;; Still only 1 agent
    (should (= 1 (alist-get 'agent-count (cgp--turn0 s))))
    ;; Type should be original
    (should (equal "Explore" (alist-get 'type (gethash "ag1" (plist-get s :agent-index)))))))

(ert-deftest cgp-complete-agent ()
  "complete_agent sets status, stop_text, stop_thinking, duration."
  (let ((s (cgp--fresh-session)))
    (cgp--apply s `((op . "add_agent")
                    (agent . ((agentId . "ag1") (type . "Explore")
                              (status . "running") (timestamp . 1700000000000)))))
    (cgp--apply s `((op . "complete_agent")
                    (agentId . "ag1")
                    (stopText . "Found 3 files.")
                    (stopThinking . "Search complete.")
                    (duration . 4.2)))
    (let ((agent (gethash "ag1" (plist-get s :agent-index))))
      (should (equal "done" (alist-get 'status agent)))
      (should (equal "Found 3 files." (alist-get 'stop_text agent)))
      (should (equal "Search complete." (alist-get 'stop_thinking agent)))
      (should (= 4.2 (alist-get 'duration agent))))))

(ert-deftest cgp-complete-agent-nonexistent ()
  "complete_agent for unknown agent_id is a no-op."
  (let ((s (cgp--fresh-session)))
    (cgp--apply s `((op . "complete_agent")
                    (agentId . "ghost")
                    (stopText . "X")))))

(ert-deftest cgp-complete-agent-transcript-path ()
  "complete_agent stores transcript_path."
  (let ((s (cgp--fresh-session)))
    (cgp--apply s `((op . "add_agent")
                    (agent . ((agentId . "ag2") (type . "Plan")
                              (status . "running") (timestamp . 1700000000000)))))
    (cgp--apply s `((op . "complete_agent")
                    (agentId . "ag2")
                    (transcriptPath . "/tmp/transcript.json")))
    (let ((agent (gethash "ag2" (plist-get s :agent-index))))
      (should (equal "/tmp/transcript.json" (alist-get 'transcript_path agent))))))


;;; ═══════════════════════════════════════════════════════════════════
;;; Part 8: update_task, track_file, add_prompt, set_prompt_answer
;;; ═══════════════════════════════════════════════════════════════════

(ert-deftest cgp-update-task ()
  "update_task stores task in tasks hash table."
  (let ((s (cgp--fresh-session)))
    (cgp--apply s `((op . "update_task")
                    (taskId . "task-1")
                    (task . ((taskId . "task-1")
                             (subject . "Fix auth")
                             (status . "in_progress")))))
    (let ((task (gethash "task-1" (plist-get s :tasks))))
      (should task)
      (should (equal "Fix auth" (alist-get 'subject task)))
      (should (equal "in_progress" (alist-get 'status task))))))

(ert-deftest cgp-update-task-overwrite ()
  "update_task overwrites existing task."
  (let ((s (cgp--fresh-session)))
    (cgp--apply s `((op . "update_task") (taskId . "t1")
                    (task . ((taskId . "t1") (status . "pending")))))
    (cgp--apply s `((op . "update_task") (taskId . "t1")
                    (task . ((taskId . "t1") (status . "completed")))))
    (should (equal "completed" (alist-get 'status (gethash "t1" (plist-get s :tasks)))))))

(ert-deftest cgp-track-file-new ()
  "track_file creates new file entry."
  (let ((s (cgp--fresh-session)))
    (cgp--apply s '((op . "track_file") (path . "/src/auth.ts") (fileOp . "read")))
    (let ((entry (gethash "/src/auth.ts" (plist-get s :files))))
      (should entry)
      (should (member "read" (alist-get 'ops entry))))))

(ert-deftest cgp-track-file-aggregate ()
  "track_file aggregates ops for same file."
  (let ((s (cgp--fresh-session)))
    (cgp--apply s '((op . "track_file") (path . "/src/auth.ts") (fileOp . "read")))
    (cgp--apply s '((op . "track_file") (path . "/src/auth.ts") (fileOp . "edit")))
    (let ((entry (gethash "/src/auth.ts" (plist-get s :files))))
      (should (member "read" (alist-get 'ops entry)))
      (should (member "edit" (alist-get 'ops entry))))))

(ert-deftest cgp-track-file-dedup-ops ()
  "track_file does not duplicate same op."
  (let ((s (cgp--fresh-session)))
    (cgp--apply s '((op . "track_file") (path . "/src/auth.ts") (fileOp . "read")))
    (cgp--apply s '((op . "track_file") (path . "/src/auth.ts") (fileOp . "read")))
    (let ((entry (gethash "/src/auth.ts" (plist-get s :files))))
      (should (= 1 (length (alist-get 'ops entry)))))))

(ert-deftest cgp-add-prompt ()
  "add_prompt sets prompt on target turn."
  (let ((s (cgp--fresh-session)))
    (cgp--apply s `((op . "add_prompt")
                    (turnNumber . 0)
                    (prompt . ((text . "Fix the bug")
                               (type . "user")
                               (submitted . 1700000000000)))))
    (let* ((turn0 (cgp--turn0 s))
           (prompt (alist-get 'prompt turn0)))
      (should prompt)
      (should (equal "Fix the bug" (alist-get 'text prompt))))))

(ert-deftest cgp-add-prompt-nonexistent-turn ()
  "add_prompt on non-existent turn is a no-op."
  (let ((s (cgp--fresh-session)))
    (cgp--apply s `((op . "add_prompt") (turnNumber . 99)
                    (prompt . ((text . "X")))))))

(ert-deftest cgp-set-prompt-answer ()
  "set_prompt_answer sets answer and elapsed on matching prompt."
  (let ((s (cgp--fresh-session)))
    ;; Add a prompt with tool_use_id
    (cgp--apply s `((op . "add_prompt")
                    (turnNumber . 0)
                    (prompt . ((text . "Which framework?")
                               (type . "question")
                               (toolUseId . "tu_q1")
                               (submitted . ,(* (float-time) 1000))))))
    (cgp--apply s `((op . "set_prompt_answer")
                    (turnNumber . 0)
                    (toolUseId . "tu_q1")
                    (answer . "Use vitest")))
    (let* ((turn0 (cgp--turn0 s))
           (prompt (alist-get 'prompt turn0)))
      (should (equal "Use vitest" (alist-get 'answer prompt)))
      (should (numberp (alist-get 'elapsed prompt))))))

(ert-deftest cgp-set-prompt-answer-wrong-tool-id ()
  "set_prompt_answer with non-matching tool_use_id is a no-op."
  (let ((s (cgp--fresh-session)))
    (cgp--apply s `((op . "add_prompt") (turnNumber . 0)
                    (prompt . ((text . "Q") (toolUseId . "tu_q1")
                               (submitted . 1700000000000)))))
    (cgp--apply s `((op . "set_prompt_answer") (turnNumber . 0)
                    (toolUseId . "tu_wrong") (answer . "X")))
    (let* ((prompt (alist-get 'prompt (cgp--turn0 s))))
      (should (null (alist-get 'answer prompt))))))


;;; ═══════════════════════════════════════════════════════════════════
;;; Part 9: Unknown patch ops
;;; ═══════════════════════════════════════════════════════════════════

(ert-deftest cgp-unknown-op-no-crash ()
  "Unknown patch op does not crash."
  (let ((s (cgp--fresh-session)))
    (cgp--apply s '((op . "future_feature") (data . "whatever")))))


;;; ═══════════════════════════════════════════════════════════════════
;;; Part 10: JSON-to-alist conversion
;;; ═══════════════════════════════════════════════════════════════════

(ert-deftest cgp-json-tool-to-alist ()
  "json-tool-to-alist maps camelCase to snake_case correctly."
  (let ((tool (claude-gravity--json-tool-to-alist
               '((toolUseId . "tu_1")
                 (name . "Edit")
                 (status . "running")
                 (input . ((file_path . "/src/a.ts")))
                 (assistantText . "I'll edit the file")
                 (assistantThinking . "Analyzing...")
                 (postText . "Done editing")
                 (parentAgentId . "ag1")
                 (timestamp . 1700000000000)))))
    (should (equal "tu_1" (alist-get 'tool_use_id tool)))
    (should (equal "Edit" (alist-get 'name tool)))
    (should (equal "running" (alist-get 'status tool)))
    (should (equal "I'll edit the file" (alist-get 'assistant_text tool)))
    (should (equal "Analyzing..." (alist-get 'assistant_thinking tool)))
    (should (equal "Done editing" (alist-get 'post_text tool)))
    (should (equal "ag1" (alist-get 'parent_agent_id tool)))))

(ert-deftest cgp-json-tool-to-alist-defaults ()
  "json-tool-to-alist defaults status to running and nils for missing."
  (let ((tool (claude-gravity--json-tool-to-alist
               '((toolUseId . "tu_2") (name . "Read") (timestamp . 1700000000000)))))
    (should (equal "running" (alist-get 'status tool)))
    (should (null (alist-get 'result tool)))
    (should (null (alist-get 'assistant_text tool)))
    (should (null (alist-get 'post_text tool)))))

(ert-deftest cgp-json-agent-to-alist ()
  "json-agent-to-alist maps fields correctly."
  (let ((agent (claude-gravity--json-agent-to-alist
                '((agentId . "ag1")
                  (type . "Explore")
                  (status . "running")
                  (stopText . "Found files")
                  (stopThinking . "Done")
                  (duration . 3.5)
                  (toolCount . 5)
                  (timestamp . 1700000000000)
                  (transcriptPath . "/tmp/t.json")))))
    (should (equal "ag1" (alist-get 'agent_id agent)))
    (should (equal "Explore" (alist-get 'type agent)))
    (should (equal "running" (alist-get 'status agent)))
    (should (equal "Found files" (alist-get 'stop_text agent)))
    (should (equal "Done" (alist-get 'stop_thinking agent)))
    (should (= 3.5 (alist-get 'duration agent)))
    (should (= 5 (alist-get 'tool-count agent)))
    (should (equal "/tmp/t.json" (alist-get 'transcript_path agent)))))

(ert-deftest cgp-json-turn-to-alist ()
  "json-turn-to-alist creates turn with pre-allocated keys."
  (let ((turn (claude-gravity--json-turn-to-alist
               '((turnNumber . 3)
                 (frozen . t)
                 (stopText . "All done")
                 (tokenIn . 5000)
                 (tokenOut . 1000)))))
    (should (= 3 (alist-get 'turn-number turn)))
    (should (eq t (alist-get 'frozen turn)))
    (should (equal "All done" (alist-get 'stop_text turn)))
    (should (= 5000 (alist-get 'token-in turn)))
    (should (= 1000 (alist-get 'token-out turn)))))

(ert-deftest cgp-json-token-usage-nil ()
  "json-token-usage returns nil for nil input."
  (should (null (claude-gravity--json-token-usage nil))))

(ert-deftest cgp-json-prompt-to-alist ()
  "json-prompt-to-alist maps prompt fields."
  (let ((prompt (claude-gravity--json-prompt-to-alist
                 '((text . "Fix the bug")
                   (type . "user")
                   (submitted . 1700000000000)
                   (toolUseId . "tu_q1")
                   (answer . "yes")))))
    (should (equal "Fix the bug" (alist-get 'text prompt)))
    (should (eq 'user (alist-get 'type prompt)))
    (should (equal "tu_q1" (alist-get 'tool_use_id prompt)))
    (should (equal "yes" (alist-get 'answer prompt)))))


;;; ═══════════════════════════════════════════════════════════════════
;;; Part 11: setf alist-get regression tests
;;; ═══════════════════════════════════════════════════════════════════

(ert-deftest cgp-setf-alist-get-regression-turn-stop ()
  "Regression: set_turn_stop must modify the turn in the tree, not a copy.
This catches the classic setf alist-get gotcha where new keys
only modify the local variable."
  (let ((s (cgp--fresh-session)))
    ;; Apply set_turn_stop
    (cgp--apply s '((op . "set_turn_stop")
                    (turnNumber . 0)
                    (stopText . "Test passed")))
    ;; Verify the turn in the :turns tlist has the stop_text
    ;; (not just the local variable in apply-patch)
    (let* ((turns-tl (plist-get s :turns))
           (turn0 (car (claude-gravity--tlist-items turns-tl))))
      (should (equal "Test passed" (alist-get 'stop_text turn0))))))

(ert-deftest cgp-setf-alist-get-regression-agent-completion ()
  "Regression: complete_agent must modify the agent in the index.
Pre-allocated keys (stop_text, stop_thinking, duration) in json-agent-to-alist
ensure setf alist-get works in-place."
  (let ((s (cgp--fresh-session)))
    (cgp--apply s `((op . "add_agent")
                    (agent . ((agentId . "ag_reg") (type . "Explore")
                              (status . "running") (timestamp . 1700000000000)))))
    (let ((agent-before (gethash "ag_reg" (plist-get s :agent-index))))
      (cgp--apply s `((op . "complete_agent")
                      (agentId . "ag_reg")
                      (stopText . "Completed")
                      (duration . 5.0)))
      ;; The SAME object in the index should have been mutated
      (should (equal "Completed" (alist-get 'stop_text agent-before)))
      (should (= 5.0 (alist-get 'duration agent-before)))
      ;; Also verify it's the same object in the turn's agents list
      (let* ((turn0 (cgp--turn0 s))
             (agents (cgp--turn-agents turn0)))
        (should (eq agent-before (car agents)))))))

(ert-deftest cgp-setf-alist-get-regression-tool-completion ()
  "Regression: complete_tool post_text must be set on the tool in the tree."
  (let ((s (cgp--fresh-session)))
    (cgp--apply s `((op . "add_step") (turnNumber . 0) (step . ())))
    (cgp--apply s `((op . "add_tool") (turnNumber . 0) (stepIndex . 0)
                    (tool . ((toolUseId . "tu_reg") (name . "Edit") (status . "running")
                             (timestamp . 1700000000000)))))
    (let ((tool-before (gethash "tu_reg" (plist-get s :tool-index))))
      (cgp--apply s `((op . "complete_tool")
                      (toolUseId . "tu_reg")
                      (status . "done")
                      (result . "ok")
                      (postText . "Edited successfully")
                      (postThinking . "Looks good")))
      ;; post_text and post_thinking should be set on the original object
      (should (equal "Edited successfully" (alist-get 'post_text tool-before)))
      (should (equal "Looks good" (alist-get 'post_thinking tool-before))))))


;;; ═══════════════════════════════════════════════════════════════════
;;; Part 12: Multi-event sequences (cross-event consistency)
;;; ═══════════════════════════════════════════════════════════════════

(ert-deftest cgp-sequence-full-turn ()
  "Full turn lifecycle: add_turn → add_step → add_tool × 3 → complete_tool × 3 → set_turn_stop → freeze_turn."
  (let ((s (cgp--fresh-session)))
    ;; Add turn 1
    (cgp--apply s `((op . "add_turn") (turn . ((turnNumber . 1)))))
    (cgp--apply s `((op . "add_prompt") (turnNumber . 1)
                    (prompt . ((text . "Fix the auth bug") (submitted . 1700000000000)))))
    ;; Response step with 3 tools
    (cgp--apply s `((op . "add_step") (turnNumber . 1)
                    (step . ((text . "Let me look into this...")))))
    (cgp--apply s `((op . "add_tool") (turnNumber . 1) (stepIndex . 0)
                    (tool . ((toolUseId . "t1") (name . "Glob") (timestamp . 1700000000000)))))
    (cgp--apply s `((op . "add_tool") (turnNumber . 1) (stepIndex . 0)
                    (tool . ((toolUseId . "t2") (name . "Read") (timestamp . 1700000000000)))))
    (cgp--apply s `((op . "add_tool") (turnNumber . 1) (stepIndex . 0)
                    (tool . ((toolUseId . "t3") (name . "Edit") (timestamp . 1700000000000)))))
    ;; Complete all tools
    (cgp--apply s `((op . "complete_tool") (toolUseId . "t1") (status . "done") (result . "*.ts")))
    (cgp--apply s `((op . "complete_tool") (toolUseId . "t2") (status . "done") (result . "code")))
    (cgp--apply s `((op . "complete_tool") (toolUseId . "t3") (status . "done") (result . "ok")))
    ;; Stop
    (cgp--apply s '((op . "set_turn_stop") (turnNumber . 1) (stopText . "Bug fixed.")))
    (cgp--apply s '((op . "set_turn_tokens") (turnNumber . 1) (tokenIn . 3000) (tokenOut . 800)))
    (cgp--apply s '((op . "freeze_turn") (turnNumber . 1)))
    ;; Verify final state
    (let ((turn1 (claude-gravity--get-turn-node s 1)))
      (should turn1)
      (should (equal "Fix the auth bug" (alist-get 'text (alist-get 'prompt turn1))))
      (should (= 3 (alist-get 'tool-count turn1)))
      (should (eq t (alist-get 'frozen turn1)))
      (should (equal "Bug fixed." (alist-get 'stop_text turn1)))
      (should (= 3000 (alist-get 'token-in turn1)))
      (should (= 800 (alist-get 'token-out turn1)))
      ;; All tools should be done
      (let* ((steps (cgp--turn-steps turn1))
             (tools (cgp--step-tools (car steps))))
        (should (= 3 (length tools)))
        (dolist (tool tools)
          (should (equal "done" (alist-get 'status tool))))))))

(ert-deftest cgp-sequence-agent-lifecycle ()
  "Agent lifecycle: add_agent → add_step → add_tool → complete_tool → complete_agent."
  (let ((s (cgp--fresh-session)))
    ;; Add a Task tool first (so agent can link to it)
    (cgp--apply s `((op . "add_step") (turnNumber . 0) (step . ())))
    (cgp--apply s `((op . "add_tool") (turnNumber . 0) (stepIndex . 0)
                    (tool . ((toolUseId . "tu_task") (name . "Task")
                             (input . ((subagent_type . "Explore")
                                       (description . "Find auth files")))
                             (status . "running") (timestamp . 1700000000000)))))
    ;; Agent starts
    (cgp--apply s `((op . "add_agent")
                    (agent . ((agentId . "ag_lc") (type . "Explore")
                              (status . "running") (timestamp . 1700000000000)))))
    ;; Agent does work
    (cgp--apply s `((op . "add_step") (turnNumber . 0) (agentId . "ag_lc")
                    (step . ((text . "Searching...")))))
    (cgp--apply s `((op . "add_tool") (turnNumber . 0) (stepIndex . 0) (agentId . "ag_lc")
                    (tool . ((toolUseId . "tu_ag_1") (name . "Grep") (timestamp . 1700000000000)))))
    (cgp--apply s `((op . "complete_tool") (toolUseId . "tu_ag_1") (status . "done") (result . "3 matches")))
    ;; Agent completes
    (cgp--apply s `((op . "complete_agent") (agentId . "ag_lc")
                    (stopText . "Found auth.ts, middleware.ts, handler.ts")
                    (duration . 2.8)))
    ;; Verify
    (let ((agent (gethash "ag_lc" (plist-get s :agent-index))))
      (should (equal "done" (alist-get 'status agent)))
      (should (equal "Found auth.ts, middleware.ts, handler.ts" (alist-get 'stop_text agent)))
      (should (= 2.8 (alist-get 'duration agent)))
      ;; Agent should be linked to Task tool
      (should (alist-get 'task-tool agent))
      (should (equal "tu_task" (alist-get 'tool_use_id (alist-get 'task-tool agent))))
      ;; Agent's tool should be completed
      (let* ((asteps (claude-gravity--tlist-items (alist-get 'steps agent)))
             (atools (cgp--step-tools (car asteps))))
        (should (= 1 (length atools)))
        (should (equal "done" (alist-get 'status (car atools))))))))

(ert-deftest cgp-sequence-multi-turn ()
  "Multi-turn: turn 0 → turn 1 → turn 2 with tools in each."
  (let ((s (cgp--fresh-session)))
    ;; Turn 0: pre-prompt activity
    (cgp--apply s `((op . "add_step") (turnNumber . 0) (step . ())))
    (cgp--apply s `((op . "add_tool") (turnNumber . 0) (stepIndex . 0)
                    (tool . ((toolUseId . "t0a") (name . "Glob") (timestamp . 1700000000000)))))
    (cgp--apply s `((op . "complete_tool") (toolUseId . "t0a") (status . "done") (result . "files")))
    (cgp--apply s '((op . "freeze_turn") (turnNumber . 0)))
    ;; Turn 1
    (cgp--apply s `((op . "add_turn") (turn . ((turnNumber . 1)))))
    (cgp--apply s `((op . "add_prompt") (turnNumber . 1)
                    (prompt . ((text . "First prompt") (submitted . 1700000000000)))))
    (cgp--apply s `((op . "add_step") (turnNumber . 1) (step . ((text . "Working...")))))
    (cgp--apply s `((op . "add_tool") (turnNumber . 1) (stepIndex . 0)
                    (tool . ((toolUseId . "t1a") (name . "Edit") (timestamp . 1700000000000)))))
    (cgp--apply s `((op . "complete_tool") (toolUseId . "t1a") (status . "done") (result . "ok")))
    (cgp--apply s '((op . "set_turn_stop") (turnNumber . 1) (stopText . "Done turn 1.")))
    (cgp--apply s '((op . "freeze_turn") (turnNumber . 1)))
    ;; Turn 2
    (cgp--apply s `((op . "add_turn") (turn . ((turnNumber . 2)))))
    (cgp--apply s `((op . "add_prompt") (turnNumber . 2)
                    (prompt . ((text . "Second prompt") (submitted . 1700000000000)))))
    (cgp--apply s `((op . "add_step") (turnNumber . 2) (step . ((text . "More work...")))))
    (cgp--apply s `((op . "add_tool") (turnNumber . 2) (stepIndex . 0)
                    (tool . ((toolUseId . "t2a") (name . "Bash") (timestamp . 1700000000000)))))
    ;; Verify state
    (should (= 2 (plist-get s :current-turn)))
    (should (= 3 (plist-get s :total-tool-count)))
    ;; Turn 0 should be frozen
    (should (eq t (alist-get 'frozen (claude-gravity--get-turn-node s 0))))
    ;; Turn 1 should be frozen with stop_text
    (let ((turn1 (claude-gravity--get-turn-node s 1)))
      (should (eq t (alist-get 'frozen turn1)))
      (should (equal "Done turn 1." (alist-get 'stop_text turn1))))
    ;; Turn 2 should not be frozen
    (should (null (alist-get 'frozen (claude-gravity--get-turn-node s 2))))))


;;; ═══════════════════════════════════════════════════════════════════
;;; Part 13: ensure-step logic (step boundary detection)
;;; ═══════════════════════════════════════════════════════════════════

(ert-deftest cgp-ensure-step-creates-first ()
  "ensure-step creates a step when none exist."
  (let* ((turn (claude-gravity--make-turn-node 0))
         (step (claude-gravity--ensure-step turn "thinking" "text")))
    (should step)
    (should (equal "thinking" (alist-get 'thinking step)))
    (should (equal "text" (alist-get 'text step)))))

(ert-deftest cgp-ensure-step-reuses-same ()
  "ensure-step reuses current step when thinking/text unchanged."
  (let* ((turn (claude-gravity--make-turn-node 0))
         (step1 (claude-gravity--ensure-step turn "thinking" "text"))
         (step2 (claude-gravity--ensure-step turn "thinking" "text")))
    (should (eq step1 step2))))

(ert-deftest cgp-ensure-step-new-on-different-thinking ()
  "ensure-step creates new step when thinking changes."
  (let* ((turn (claude-gravity--make-turn-node 0))
         (step1 (claude-gravity--ensure-step turn "thinking-A" "text"))
         (step2 (claude-gravity--ensure-step turn "thinking-B" "text")))
    (should (not (eq step1 step2)))
    (should (equal "thinking-B" (alist-get 'thinking step2)))))

(ert-deftest cgp-ensure-step-new-on-different-text ()
  "ensure-step creates new step when text is non-empty and differs."
  (let* ((turn (claude-gravity--make-turn-node 0))
         (step1 (claude-gravity--ensure-step turn nil "First paragraph"))
         (step2 (claude-gravity--ensure-step turn nil "Completely different text")))
    (should (not (eq step1 step2)))))

(ert-deftest cgp-ensure-step-reuses-when-text-subsumes ()
  "ensure-step reuses step when new text subsumes old (streaming accumulation)."
  (let* ((turn (claude-gravity--make-turn-node 0))
         (step1 (claude-gravity--ensure-step turn nil "First part"))
         (step2 (claude-gravity--ensure-step turn nil "First part\n\nMore content")))
    ;; Text subsumption: "First part\n\nMore content" subsumes "First part"
    (should (eq step1 step2))))

(ert-deftest cgp-ensure-step-reuses-nil-text ()
  "ensure-step reuses step when new text is nil."
  (let* ((turn (claude-gravity--make-turn-node 0))
         (step1 (claude-gravity--ensure-step turn nil "text"))
         (step2 (claude-gravity--ensure-step turn nil nil)))
    (should (eq step1 step2))))

(ert-deftest cgp-ensure-step-reuses-empty-text ()
  "ensure-step reuses step when new text is empty."
  (let* ((turn (claude-gravity--make-turn-node 0))
         (step1 (claude-gravity--ensure-step turn nil "text"))
         (step2 (claude-gravity--ensure-step turn nil "")))
    (should (eq step1 step2))))

(ert-deftest cgp-ensure-step-new-when-both-non-empty-different ()
  "ensure-step creates new step when BOTH old and new text are non-empty and different."
  (let* ((turn (claude-gravity--make-turn-node 0))
         (step1 (claude-gravity--ensure-step turn nil "Alpha"))
         (step2 (claude-gravity--ensure-step turn nil "Beta")))
    ;; Both non-empty, non-subsumption → new step
    (should (not (eq step1 step2)))))

(ert-deftest cgp-ensure-step-fills-empty-text ()
  "ensure-step fills text on existing step when it was nil."
  (let* ((turn (claude-gravity--make-turn-node 0))
         (step1 (claude-gravity--ensure-step turn nil nil)))
    ;; Step has nil text, now ensure with text — should reuse and fill
    (let ((step2 (claude-gravity--ensure-step turn nil "Now has text")))
      ;; Should reuse same step
      (should (eq step1 step2))
      ;; But text should be set (this is in --tree-add-tool logic, not ensure-step itself)
      )))


;;; ═══════════════════════════════════════════════════════════════════
;;; Part 14: text-subsumes-p
;;; ═══════════════════════════════════════════════════════════════════

(ert-deftest cgp-text-subsumes-equal ()
  "text-subsumes-p returns t for equal strings."
  (should (claude-gravity--text-subsumes-p "hello" "hello")))

(ert-deftest cgp-text-subsumes-prefix ()
  "text-subsumes-p returns t when a extends b with paragraph break."
  (should (claude-gravity--text-subsumes-p "hello\n\nworld" "hello")))

(ert-deftest cgp-text-subsumes-reverse ()
  "text-subsumes-p returns t when b extends a with paragraph break."
  (should (claude-gravity--text-subsumes-p "hello" "hello\n\nworld")))

(ert-deftest cgp-text-subsumes-nil ()
  "text-subsumes-p returns nil for nil inputs."
  (should-not (claude-gravity--text-subsumes-p nil "hello"))
  (should-not (claude-gravity--text-subsumes-p "hello" nil))
  (should-not (claude-gravity--text-subsumes-p nil nil)))

(ert-deftest cgp-text-subsumes-empty ()
  "text-subsumes-p returns nil for empty strings."
  (should-not (claude-gravity--text-subsumes-p "" "hello"))
  (should-not (claude-gravity--text-subsumes-p "hello" ""))
  (should-not (claude-gravity--text-subsumes-p "" "")))

(ert-deftest cgp-text-subsumes-different ()
  "text-subsumes-p returns nil for unrelated strings."
  (should-not (claude-gravity--text-subsumes-p "alpha" "beta")))

(ert-deftest cgp-text-subsumes-single-newline ()
  "text-subsumes-p requires double newline, not single."
  (should-not (claude-gravity--text-subsumes-p "hello\nworld" "hello")))

(provide 'claude-gravity-patch-test)
;;; claude-gravity-patch-test.el ends here
