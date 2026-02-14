;;; claude-gravity-contract-test.el --- Socket contract compliance tests  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Compliance tests that verify `claude-gravity-handle-event' processes
;; events consistently regardless of the source bridge (Claude Code or
;; OpenCode).  Mirrors the TypeScript socket-contract.ts schemas.
;;
;; These tests are the Emacs-side complement to emacs-bridge/test/socket-contract.test.ts.
;; Together they form a two-sided contract:
;;   - TS side: bridges produce correct envelope + data shapes
;;   - EL side: Emacs correctly consumes those shapes and mutates session state

;;; Code:

(require 'ert)
(require 'claude-gravity)

;;; ─── Test infrastructure ──────────────────────────────────────────

(defun cgc--fresh ()
  "Clear all sessions and return a clean environment."
  (clrhash claude-gravity--sessions)
  (clrhash claude-gravity--tmux-sessions)
  (setq claude-gravity--inbox nil)
  (setq claude-gravity--inbox-counter 0))

(defun cgc--event (event sid data &optional source)
  "Fire EVENT for session SID with DATA alist.
Optional SOURCE is \"opencode\" or nil (claude-code)."
  (claude-gravity-handle-event event sid "/tmp/test" data nil source))

(defun cgc--get (sid)
  "Get session plist for SID."
  (claude-gravity--get-session sid))

(defun cgc--all-root-tools (sid)
  "Collect all root tools across all turns."
  (let ((result nil)
        (session (cgc--get sid)))
    (dolist (tn (claude-gravity--tlist-items (plist-get session :turns)))
      (dolist (cycle (claude-gravity--tlist-items (alist-get 'cycles tn)))
        (dolist (tool (claude-gravity--tlist-items (alist-get 'tools cycle)))
          (push tool result))))
    (nreverse result)))

(defun cgc--all-prompts (sid)
  "Collect all prompt entries from the turn tree."
  (let ((result nil)
        (session (cgc--get sid)))
    (dolist (tn (claude-gravity--tlist-items (plist-get session :turns)))
      (let ((p (alist-get 'prompt tn)))
        (when p (push p result))))
    (nreverse result)))

;;; ═══════════════════════════════════════════════════════════════════
;;; Part 1: Session lifecycle — both bridges create/end sessions
;;; ═══════════════════════════════════════════════════════════════════

(ert-deftest cgc-session-start-creates-session ()
  "SessionStart creates a new session with correct defaults."
  (cgc--fresh)
  (cgc--event "SessionStart" "s1" '())
  (let ((s (cgc--get "s1")))
    (should s)
    (should (equal "s1" (plist-get s :session-id)))
    (should (eq 'active (plist-get s :status)))
    (should (eq 'idle (plist-get s :claude-status)))
    (should (= 0 (plist-get s :current-turn)))
    (should (equal "claude-code" (plist-get s :source)))))

(ert-deftest cgc-session-start-opencode-sets-source ()
  "SessionStart from OpenCode bridge sets source metadata."
  (cgc--fresh)
  (cgc--event "SessionStart" "oc-1"
              `((slug . "my-session") (instance_port . 8080) (instance_dir . "/proj"))
              "opencode")
  (let ((s (cgc--get "oc-1")))
    (should s)
    (should (equal "opencode" (plist-get s :source)))
    (should (= 8080 (plist-get s :instance-port)))
    (should (equal "/proj" (plist-get s :instance-dir)))))

(ert-deftest cgc-session-start-with-slug ()
  "SessionStart with slug stores it on the session."
  (cgc--fresh)
  (cgc--event "SessionStart" "s2" '((slug . "fancy-slug")))
  (let ((s (cgc--get "s2")))
    (should (equal "fancy-slug" (plist-get s :slug)))))

(ert-deftest cgc-session-start-resets-existing ()
  "SessionStart on existing session resets conversational state."
  (cgc--fresh)
  (cgc--event "SessionStart" "s3" '())
  (cgc--event "UserPromptSubmit" "s3" '((prompt . "hello")))
  (should (= 1 (plist-get (cgc--get "s3") :current-turn)))
  ;; SessionStart again — resets
  (cgc--event "SessionStart" "s3" '())
  (should (= 0 (plist-get (cgc--get "s3") :current-turn))))

(ert-deftest cgc-session-end-marks-ended ()
  "SessionEnd marks the session as ended."
  (cgc--fresh)
  (cgc--event "SessionStart" "s4" '())
  (cgc--event "SessionEnd" "s4" '())
  (let ((s (cgc--get "s4")))
    (should (eq 'ended (plist-get s :status)))))

(ert-deftest cgc-session-end-nonexistent-no-crash ()
  "SessionEnd for unknown session does not crash."
  (cgc--fresh)
  (cgc--event "SessionEnd" "ghost" '())
  (should (null (cgc--get "ghost"))))

;;; ═══════════════════════════════════════════════════════════════════
;;; Part 2: UserPromptSubmit — both bridges create prompts/turns
;;; ═══════════════════════════════════════════════════════════════════

(ert-deftest cgc-prompt-creates-turn ()
  "UserPromptSubmit creates a prompt entry and advances current-turn."
  (cgc--fresh)
  (cgc--event "UserPromptSubmit" "s5" '((prompt . "hello world")))
  (let ((s (cgc--get "s5")))
    (should (= 1 (plist-get s :current-turn)))
    (should (eq 'responding (plist-get s :claude-status)))
    (let ((prompts (cgc--all-prompts "s5")))
      (should (= 1 (length prompts)))
      (should (equal "hello world" (alist-get 'text (car prompts)))))))

(ert-deftest cgc-prompt-nil-text-no-crash ()
  "UserPromptSubmit with nil prompt text does not crash."
  (cgc--fresh)
  (cgc--event "UserPromptSubmit" "s6" '())
  (let ((s (cgc--get "s6")))
    ;; Session created, status set to responding
    (should s)
    (should (eq 'responding (plist-get s :claude-status)))))

(ert-deftest cgc-prompt-slash-command ()
  "UserPromptSubmit with XML-wrapped slash command extracts command name."
  (cgc--fresh)
  (cgc--event "UserPromptSubmit" "s7"
              `((prompt . ,(concat "<command-name>commit</command-name>"
                                   "<command-args>-m fix</command-args>"))))
  (let ((prompts (cgc--all-prompts "s7")))
    (should (= 1 (length prompts)))
    (should (equal "/commit -m fix" (alist-get 'text (car prompts))))))

;;; ═══════════════════════════════════════════════════════════════════
;;; Part 3: Tool lifecycle — PreToolUse / PostToolUse / Failure
;;; ═══════════════════════════════════════════════════════════════════

(ert-deftest cgc-pretool-creates-running-tool ()
  "PreToolUse creates a running tool in the turn tree."
  (cgc--fresh)
  (cgc--event "UserPromptSubmit" "s8" '((prompt . "work")))
  (cgc--event "PreToolUse" "s8"
              '((tool_use_id . "t1") (tool_name . "Read")
                (tool_input . ((file_path . "/foo.el")))))
  (let ((tools (cgc--all-root-tools "s8")))
    (should (= 1 (length tools)))
    (let ((t1 (car tools)))
      (should (equal "t1" (alist-get 'tool_use_id t1)))
      (should (equal "Read" (alist-get 'name t1)))
      (should (equal "running" (alist-get 'status t1)))
      (should (= 1 (alist-get 'turn t1))))))

(ert-deftest cgc-pretool-with-enrichment-fields ()
  "PreToolUse stores optional enrichment fields from Claude Code bridge."
  (cgc--fresh)
  (cgc--event "UserPromptSubmit" "s9" '((prompt . "go")))
  (cgc--event "PreToolUse" "s9"
              '((tool_use_id . "t1") (tool_name . "Bash") (tool_input . nil)
                (assistant_text . "Let me check")
                (assistant_thinking . "I should run ls")
                (model . "opus")
                (permission_mode . "plan")))
  (let ((tool (car (cgc--all-root-tools "s9"))))
    (should tool)
    (should (equal "Let me check" (alist-get 'assistant_text tool)))
    (should (equal "I should run ls" (alist-get 'assistant_thinking tool)))
    (should (equal "opus" (alist-get 'model tool)))
    (should (equal "plan" (alist-get 'permission_mode tool)))))

(ert-deftest cgc-posttool-completes-tool ()
  "PostToolUse marks tool as done with result."
  (cgc--fresh)
  (cgc--event "UserPromptSubmit" "s10" '((prompt . "work")))
  (cgc--event "PreToolUse" "s10"
              '((tool_use_id . "t1") (tool_name . "Read") (tool_input . nil)))
  (cgc--event "PostToolUse" "s10"
              '((tool_use_id . "t1") (tool_name . "Read")
                (tool_response . "file contents here")))
  (let ((tool (car (cgc--all-root-tools "s10"))))
    (should (equal "done" (alist-get 'status tool)))
    (should (equal "file contents here" (alist-get 'result tool)))))

(ert-deftest cgc-posttool-with-post-text ()
  "PostToolUse stores post_tool_text and post_tool_thinking."
  (cgc--fresh)
  (cgc--event "UserPromptSubmit" "s11" '((prompt . "work")))
  (cgc--event "PreToolUse" "s11"
              '((tool_use_id . "t1") (tool_name . "Read") (tool_input . nil)))
  (cgc--event "PostToolUse" "s11"
              '((tool_use_id . "t1") (tool_name . "Read")
                (post_tool_text . "Looks good")
                (post_tool_thinking . "The file is correct")))
  (let ((tool (car (cgc--all-root-tools "s11"))))
    (should (equal "Looks good" (alist-get 'post_text tool)))
    (should (equal "The file is correct" (alist-get 'post_thinking tool)))))

(ert-deftest cgc-posttool-failure-marks-error ()
  "PostToolUseFailure marks tool as error with error message."
  (cgc--fresh)
  (cgc--event "UserPromptSubmit" "s12" '((prompt . "work")))
  (cgc--event "PreToolUse" "s12"
              '((tool_use_id . "t1") (tool_name . "Bash") (tool_input . nil)))
  (cgc--event "PostToolUseFailure" "s12"
              '((tool_use_id . "t1") (tool_name . "Bash")
                (error . "Permission denied")))
  (let ((tool (car (cgc--all-root-tools "s12"))))
    (should (equal "error" (alist-get 'status tool)))
    (should (string-match-p "Permission denied" (alist-get 'result tool)))))

(ert-deftest cgc-pretool-dedup-by-tool-use-id ()
  "PreToolUse with duplicate tool_use_id is ignored (dedup)."
  (cgc--fresh)
  (cgc--event "UserPromptSubmit" "s13" '((prompt . "work")))
  (cgc--event "PreToolUse" "s13"
              '((tool_use_id . "dup1") (tool_name . "Read") (tool_input . nil)))
  (cgc--event "PreToolUse" "s13"
              '((tool_use_id . "dup1") (tool_name . "Read") (tool_input . nil)))
  (should (= 1 (length (cgc--all-root-tools "s13")))))

;;; ═══════════════════════════════════════════════════════════════════
;;; Part 4: Stop event — finalize turn
;;; ═══════════════════════════════════════════════════════════════════

(ert-deftest cgc-stop-sets-idle ()
  "Stop sets claude-status to idle."
  (cgc--fresh)
  (cgc--event "UserPromptSubmit" "s14" '((prompt . "go")))
  (should (eq 'responding (plist-get (cgc--get "s14") :claude-status)))
  (cgc--event "Stop" "s14" '())
  (should (eq 'idle (plist-get (cgc--get "s14") :claude-status))))

(ert-deftest cgc-stop-stores-stop-text ()
  "Stop stores stop_text and stop_thinking on the turn node."
  (cgc--fresh)
  (cgc--event "UserPromptSubmit" "s15" '((prompt . "go")))
  (cgc--event "Stop" "s15"
              '((stop_text . "All done") (stop_thinking . "Task complete")))
  (let* ((session (cgc--get "s15"))
         (turn (claude-gravity--current-turn-node session)))
    (should (equal "All done" (alist-get 'stop_text turn)))
    (should (equal "Task complete" (alist-get 'stop_thinking turn)))))

(ert-deftest cgc-stop-stores-token-usage ()
  "Stop stores token_usage on the session."
  (cgc--fresh)
  (cgc--event "UserPromptSubmit" "s16" '((prompt . "go")))
  (cgc--event "Stop" "s16"
              `((token_usage . ((input_tokens . 100) (output_tokens . 50)))))
  (let* ((session (cgc--get "s16"))
         (usage (plist-get session :token-usage)))
    (should usage)
    (should (= 100 (alist-get 'input_tokens usage)))
    (should (= 50 (alist-get 'output_tokens usage)))))

(ert-deftest cgc-stop-without-data-no-crash ()
  "Stop with empty data still sets idle and doesn't crash."
  (cgc--fresh)
  (cgc--event "UserPromptSubmit" "s17" '((prompt . "go")))
  (cgc--event "Stop" "s17" '())
  (should (eq 'idle (plist-get (cgc--get "s17") :claude-status))))

;;; ═══════════════════════════════════════════════════════════════════
;;; Part 5: Agent lifecycle — SubagentStart / SubagentStop
;;; ═══════════════════════════════════════════════════════════════════

(ert-deftest cgc-agent-start-registers-agent ()
  "SubagentStart creates a running agent."
  (cgc--fresh)
  (cgc--event "UserPromptSubmit" "s18" '((prompt . "research")))
  (cgc--event "SubagentStart" "s18"
              '((agent_id . "a1") (agent_type . "Explore")))
  (let* ((session (cgc--get "s18"))
         (agent (claude-gravity--find-agent session "a1")))
    (should agent)
    (should (equal "a1" (alist-get 'agent_id agent)))
    (should (equal "Explore" (alist-get 'type agent)))
    (should (equal "running" (alist-get 'status agent)))))

(ert-deftest cgc-agent-stop-completes-agent ()
  "SubagentStop marks agent as done with stop_text."
  (cgc--fresh)
  (cgc--event "UserPromptSubmit" "s19" '((prompt . "research")))
  (cgc--event "SubagentStart" "s19"
              '((agent_id . "a1") (agent_type . "Explore")))
  (cgc--event "SubagentStop" "s19"
              '((agent_id . "a1")
                (agent_stop_text . "Found 3 files")
                (agent_stop_thinking . "Checked src/")))
  (let* ((session (cgc--get "s19"))
         (agent (claude-gravity--find-agent session "a1")))
    (should (equal "done" (alist-get 'status agent)))
    (should (equal "Found 3 files" (alist-get 'stop_text agent)))
    (should (equal "Checked src/" (alist-get 'stop_thinking agent)))
    (should (numberp (alist-get 'duration agent)))))

(ert-deftest cgc-agent-stop-unknown-agent-no-crash ()
  "SubagentStop for unknown agent_id does not crash."
  (cgc--fresh)
  (cgc--event "UserPromptSubmit" "s20" '((prompt . "work")))
  (cgc--event "SubagentStop" "s20" '((agent_id . "ghost")))
  ;; No crash — just a no-op
  (should (cgc--get "s20")))

;;; ═══════════════════════════════════════════════════════════════════
;;; Part 6: Notification
;;; ═══════════════════════════════════════════════════════════════════

(ert-deftest cgc-notification-stores-message ()
  "Notification stores message in session notifications list."
  (cgc--fresh)
  (cgc--event "SessionStart" "s21" '())
  (cgc--event "Notification" "s21"
              '((message . "Task complete") (notification_type . "info")))
  (let* ((session (cgc--get "s21"))
         (notifs (plist-get session :notifications)))
    (should (= 1 (length notifs)))
    (should (equal "Task complete" (alist-get 'message (car notifs))))))

(ert-deftest cgc-notification-no-session-no-crash ()
  "Notification for unknown session does not crash."
  (cgc--fresh)
  (cgc--event "Notification" "ghost" '((message . "hi")))
  ;; No crash, session not created
  (should-not (cgc--get "ghost")))

;;; ═══════════════════════════════════════════════════════════════════
;;; Part 7: OpenCode-specific events
;;; ═══════════════════════════════════════════════════════════════════

(ert-deftest cgc-oc-session-status-creates-session ()
  "SessionStatus from OpenCode creates session and sets metadata."
  (cgc--fresh)
  (cgc--event "SessionStatus" "oc-2"
              '((status . "busy") (title . "Fix bug") (slug . "fix-slug")
                (branch . "main") (instance_port . 9090) (instance_dir . "/proj"))
              "opencode")
  (let ((s (cgc--get "oc-2")))
    (should s)
    (should (equal "opencode" (plist-get s :source)))
    (should (equal "Fix bug" (plist-get s :title)))
    (should (equal "fix-slug" (plist-get s :slug)))
    (should (equal "main" (plist-get s :branch)))
    (should (= 9090 (plist-get s :instance-port)))))

(ert-deftest cgc-oc-session-status-nil-fields ()
  "SessionStatus with nil optional fields doesn't overwrite existing data."
  (cgc--fresh)
  (cgc--event "SessionStatus" "oc-3"
              '((status . "busy") (title . "Original") (slug . "orig"))
              "opencode")
  ;; Second update with nil title/slug — should not overwrite
  (cgc--event "SessionStatus" "oc-3"
              '((status . "idle"))
              "opencode")
  (let ((s (cgc--get "oc-3")))
    (should (equal "Original" (plist-get s :title)))
    (should (equal "orig" (plist-get s :slug)))))

(ert-deftest cgc-oc-vcs-branch-update ()
  "VcsBranchUpdate sets branch on existing session."
  (cgc--fresh)
  (cgc--event "SessionStart" "oc-4" '())
  (cgc--event "VcsBranchUpdate" "oc-4" '((branch . "feature/xyz")))
  (should (equal "feature/xyz" (plist-get (cgc--get "oc-4") :branch))))

(ert-deftest cgc-oc-vcs-branch-no-session-no-crash ()
  "VcsBranchUpdate for unknown session does not crash."
  (cgc--fresh)
  (cgc--event "VcsBranchUpdate" "ghost" '((branch . "main")))
  (should-not (cgc--get "ghost")))

(ert-deftest cgc-oc-session-update-sets-title ()
  "SessionUpdate sets title and slug on existing session."
  (cgc--fresh)
  (cgc--event "SessionStart" "oc-5" '())
  (cgc--event "SessionUpdate" "oc-5"
              '((title . "New Title") (slug . "new-slug"))
              "opencode")
  (let ((s (cgc--get "oc-5")))
    (should (equal "New Title" (plist-get s :title)))
    (should (equal "new-slug" (plist-get s :slug)))))

(ert-deftest cgc-oc-message-part-sets-source ()
  "MessagePart sets source to opencode on the session."
  (cgc--fresh)
  (cgc--event "SessionStart" "oc-6" '())
  (should (equal "claude-code" (plist-get (cgc--get "oc-6") :source)))
  (cgc--event "MessagePart" "oc-6"
              '((message_id . "m1") (part_id . "p1") (part_type . "text")
                (instance_port . 8080) (instance_dir . "/proj"))
              "opencode")
  (should (equal "opencode" (plist-get (cgc--get "oc-6") :source))))

(ert-deftest cgc-oc-assistant-message-sets-source ()
  "AssistantMessage sets source to opencode."
  (cgc--fresh)
  (cgc--event "SessionStart" "oc-7" '())
  (cgc--event "AssistantMessage" "oc-7"
              '((message_id . "m1") (instance_port . 8080))
              "opencode")
  (should (equal "opencode" (plist-get (cgc--get "oc-7") :source))))

;;; ═══════════════════════════════════════════════════════════════════
;;; Part 8: Cross-bridge consistency — same event, different sources
;;; ═══════════════════════════════════════════════════════════════════

(ert-deftest cgc-cross-session-start-both-sources ()
  "SessionStart from both bridges creates sessions with same structure."
  (cgc--fresh)
  ;; Claude Code session
  (cgc--event "SessionStart" "cc-1" '((slug . "cc-slug")))
  ;; OpenCode session
  (cgc--event "SessionStart" "oc-10"
              '((slug . "oc-slug") (instance_port . 8080))
              "opencode")
  (let ((cc (cgc--get "cc-1"))
        (oc (cgc--get "oc-10")))
    ;; Both have same structural fields
    (should (= 0 (plist-get cc :current-turn)))
    (should (= 0 (plist-get oc :current-turn)))
    (should (eq 'active (plist-get cc :status)))
    (should (eq 'active (plist-get oc :status)))
    (should (eq 'idle (plist-get cc :claude-status)))
    (should (eq 'idle (plist-get oc :claude-status)))
    ;; Source differs
    (should (equal "claude-code" (plist-get cc :source)))
    (should (equal "opencode" (plist-get oc :source)))))

(ert-deftest cgc-cross-full-turn-both-sources ()
  "Full turn cycle (prompt → tool → stop) works identically from both sources."
  (cgc--fresh)
  ;; Claude Code turn
  (cgc--event "UserPromptSubmit" "cc-2" '((prompt . "hello")))
  (cgc--event "PreToolUse" "cc-2"
              '((tool_use_id . "t1") (tool_name . "Read") (tool_input . nil)))
  (cgc--event "PostToolUse" "cc-2"
              '((tool_use_id . "t1") (tool_name . "Read") (tool_response . "ok")))
  (cgc--event "Stop" "cc-2" '((stop_text . "Done")))
  ;; OpenCode turn (same structure, different session)
  (cgc--event "UserPromptSubmit" "oc-11" '((prompt . "hello")) "opencode")
  (cgc--event "PreToolUse" "oc-11"
              '((tool_use_id . "t1") (tool_name . "Read") (tool_input . nil)))
  (cgc--event "PostToolUse" "oc-11"
              '((tool_use_id . "t1") (tool_name . "Read") (tool_response . "ok")))
  (cgc--event "Stop" "oc-11" '((stop_text . "Done")))
  ;; Both sessions end up in same state
  (let ((cc (cgc--get "cc-2"))
        (oc (cgc--get "oc-11")))
    (should (= 1 (plist-get cc :current-turn)))
    (should (= 1 (plist-get oc :current-turn)))
    (should (eq 'idle (plist-get cc :claude-status)))
    (should (eq 'idle (plist-get oc :claude-status)))
    ;; Same tool count
    (should (= 1 (length (cgc--all-root-tools "cc-2"))))
    (should (= 1 (length (cgc--all-root-tools "oc-11"))))
    ;; Tools have same structure
    (let ((cc-tool (car (cgc--all-root-tools "cc-2")))
          (oc-tool (car (cgc--all-root-tools "oc-11"))))
      (should (equal "done" (alist-get 'status cc-tool)))
      (should (equal "done" (alist-get 'status oc-tool)))
      (should (equal "ok" (alist-get 'result cc-tool)))
      (should (equal "ok" (alist-get 'result oc-tool))))))

;;; ═══════════════════════════════════════════════════════════════════
;;; Part 9: Source preservation — source doesn't leak across sessions
;;; ═══════════════════════════════════════════════════════════════════

(ert-deftest cgc-source-isolation ()
  "Events from different sources don't cross-contaminate sessions."
  (cgc--fresh)
  (cgc--event "SessionStart" "cc-3" '())
  (cgc--event "SessionStart" "oc-12" '((instance_port . 8080)) "opencode")
  ;; Claude Code events don't change OC session
  (cgc--event "UserPromptSubmit" "cc-3" '((prompt . "cc work")))
  (should (equal "claude-code" (plist-get (cgc--get "cc-3") :source)))
  (should (equal "opencode" (plist-get (cgc--get "oc-12") :source)))
  ;; OC session still idle
  (should (eq 'idle (plist-get (cgc--get "oc-12") :claude-status))))

;;; ═══════════════════════════════════════════════════════════════════
;;; Part 10: Edge cases / robustness
;;; ═══════════════════════════════════════════════════════════════════

(ert-deftest cgc-unknown-event-no-crash ()
  "Unknown event type does not crash — handled by pcase fallthrough."
  (cgc--fresh)
  (cgc--event "SessionStart" "s22" '())
  ;; Unknown event — should not error
  (cgc--event "TotallyBogusEvent" "s22" '((foo . "bar")))
  ;; Session still intact
  (should (cgc--get "s22")))

(ert-deftest cgc-nil-session-id-uses-legacy ()
  "nil session-id defaults to 'legacy'."
  (cgc--fresh)
  (claude-gravity-handle-event "SessionStart" nil nil '())
  (should (cgc--get "legacy")))

(ert-deftest cgc-empty-data-alist ()
  "Events with empty data alist don't crash."
  (cgc--fresh)
  (cgc--event "SessionStart" "s23" '())
  (cgc--event "PreToolUse" "s23" '())
  ;; No tool created (missing tool_use_id), but no crash
  (should (cgc--get "s23")))

(ert-deftest cgc-file-tracking-on-tool-events ()
  "PreToolUse and PostToolUse for Read/Edit/Write track files."
  (cgc--fresh)
  (cgc--event "UserPromptSubmit" "s24" '((prompt . "go")))
  (cgc--event "PreToolUse" "s24"
              '((tool_use_id . "t1") (tool_name . "Read")
                (tool_input . ((file_path . "/foo.el")))))
  (cgc--event "PostToolUse" "s24"
              '((tool_use_id . "t2") (tool_name . "Edit")
                (tool_input . ((file_path . "/bar.el")))))
  (let* ((session (cgc--get "s24"))
         (files (plist-get session :files)))
    (should (gethash "/foo.el" files))
    (should (gethash "/bar.el" files))
    (should (member "read" (alist-get 'ops (gethash "/foo.el" files))))
    (should (member "edit" (alist-get 'ops (gethash "/bar.el" files))))))

(ert-deftest cgc-statusline-stores-cost-and-context ()
  "StatusLine event stores cost, context percentage, and model.
Data is nested JSON (not flat dotted keys) — let-alist uses nested access."
  (cgc--fresh)
  (cgc--event "SessionStart" "s25" '())
  (cgc--event "StatusLine" "s25"
              '((cost . ((total_cost_usd . 0.42)))
                (context_window . ((used_percentage . 35.7)))
                (model . ((display_name . "Claude Opus")))))
  (let ((s (cgc--get "s25")))
    (should (= 0.42 (plist-get s :cost)))
    (should (= 35 (plist-get s :context-pct)))
    (should (equal "Claude Opus" (plist-get s :model-name)))))

(ert-deftest cgc-ask-user-question-advances-turn ()
  "AskUserQuestion tool creates question prompt and advances turn."
  (cgc--fresh)
  (cgc--event "UserPromptSubmit" "s26" '((prompt . "investigate")))
  (cgc--event "PreToolUse" "s26"
              `((tool_use_id . "q1") (tool_name . "AskUserQuestion")
                (tool_input . ((questions . [((question . "Which approach?"))])))))
  (let ((s (cgc--get "s26")))
    ;; Turn advanced to 2 (original prompt + question)
    (should (= 2 (plist-get s :current-turn)))
    (let ((prompts (cgc--all-prompts "s26")))
      (should (= 2 (length prompts)))
      (should (eq 'question (alist-get 'type (nth 1 prompts))))
      (should (equal "Which approach?" (alist-get 'text (nth 1 prompts)))))))

(ert-deftest cgc-exit-plan-mode-stores-plan ()
  "ExitPlanMode PostToolUse stores plan content and creates phase boundary."
  (cgc--fresh)
  (cgc--event "UserPromptSubmit" "s27" '((prompt . "plan")))
  (cgc--event "PreToolUse" "s27"
              '((tool_use_id . "epm1") (tool_name . "ExitPlanMode")
                (tool_input . ((plan . "Step 1: Do stuff")))))
  (cgc--event "PostToolUse" "s27"
              '((tool_use_id . "epm1") (tool_name . "ExitPlanMode")
                (tool_input . ((plan . "Step 1: Do stuff")))
                (tool_response . ((filePath . "/plan.md")))))
  (let* ((s (cgc--get "s27"))
         (plan (plist-get s :plan)))
    (should plan)
    (should (equal "Step 1: Do stuff" (plist-get plan :content)))
    ;; Phase boundary prompt created
    (should (= 2 (plist-get s :current-turn)))
    (let ((prompts (cgc--all-prompts "s27")))
      (should (eq 'phase-boundary (alist-get 'type (nth 1 prompts)))))))

(provide 'claude-gravity-contract-test)
;;; claude-gravity-contract-test.el ends here
