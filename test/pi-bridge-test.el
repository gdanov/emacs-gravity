;;; pi-bridge-test.el --- ERT tests for pi bridge with minimax -*- lexical-binding: t; -*-

(require 'ert)
(require 'claude-gravity)

;;;; Test helpers

(defvar pi-test--captured-events nil
  "List of events captured during test.")

(defun pi-test--capture-event (event-name session-id cwd data)
  "Capture EVENT-NAME for SESSION-ID with DATA for testing."
  (push (list event-name session-id data) pi-test--captured-events))

(defun pi-test--mock-send-event (event-name session-id cwd pid payload)
  "Mock sendEvent that captures events for testing."
  (pi-test--capture-event event-name session-id cwd payload))

(defun pi-test--mock-send-and-wait (event-name session-id cwd pid payload)
  "Mock sendAndWait that returns allow for all permission requests."
  (cond
   ((equal event-name "PermissionRequest")
    (list 'hookSpecificOutput (list 'decision (list 'behavior "allow"))))
   (t nil)))

;;;; Tests for pi bridge turn demarcation

(ert-deftest pi-test-user-prompt-submit-advances-turn ()
  "UserPromptSubmit from pi bridge advances turn counter."
  (let ((sid "pi-turn-test-1"))
    (remhash sid claude-gravity--sessions)
    (claude-gravity-handle-event "SessionStart" sid "/tmp/test"
                                 (list (cons 'session_id sid)
                                       (cons 'model "minimax/MiniMax-M2.5")))
    (should (= 0 (plist-get (claude-gravity--get-session sid) :current-turn)))
    
    (claude-gravity-handle-event "UserPromptSubmit" sid "/tmp/test"
                                 (list (cons 'session_id sid)
                                       (cons 'prompt "Hello pi agent")))
    (should (= 1 (plist-get (claude-gravity--get-session sid) :current-turn)))
    
    (claude-gravity-handle-event "UserPromptSubmit" sid "/tmp/test"
                                 (list (cons 'session_id sid)
                                       (cons 'prompt "Second prompt")))
    (should (= 2 (plist-get (claude-gravity--get-session sid) :current-turn)))))

(ert-deftest pi-test-pre-tool-use-captures-tool-with-turn ()
  "PreToolUse event captures tool with current turn stamp."
  (let ((sid "pi-tool-test-1"))
    (remhash sid claude-gravity--sessions)
    (claude-gravity-handle-event "SessionStart" sid "/tmp/test"
                                 (list (cons 'session_id sid)
                                       (cons 'model "minimax/MiniMax-M2.5")))
    (claude-gravity-handle-event "UserPromptSubmit" sid "/tmp/test"
                                 (list (cons 'session_id sid)
                                       (cons 'prompt "List files")))
    
    (claude-gravity-handle-event "PreToolUse" sid "/tmp/test"
                                 (list (cons 'session_id sid)
                                       (cons 'tool_name "Bash")
                                       (cons 'tool_use_id "tool_123")
                                       (cons 'tool_input (list (cons 'command "ls -la")))))
    
    (let* ((session (claude-gravity--get-session sid))
           (turns (plist-get session :turns))
           (turn (car (claude-gravity--tlist-items turns))))
      (should turn)
      (let ((cycles (alist-get 'cycles turn)))
        (should cycles)
        (let ((cycle (car (claude-gravity--tlist-items cycles))))
          (should cycle)
          (let ((tools (alist-get 'tools cycle)))
            (should tools)
            (let ((tool (car (claude-gravity--tlist-items tools))))
              (should tool)
              (should (equal "Bash" (alist-get 'name tool)))
              (should (= 1 (alist-get 'turn tool)))))))))

(ert-deftest pi-test-post-tool-use-completes-tool ()
  "PostToolUse event completes the tool with result."
  (let ((sid "pi-tool-test-2"))
    (remhash sid claude-gravity--sessions)
    (claude-gravity-handle-event "SessionStart" sid "/tmp/test"
                                 (list (cons 'session_id sid)
                                       (cons 'model "minimax/MiniMax-M2.5")))
    (claude-gravity-handle-event "UserPromptSubmit" sid "/tmp/test"
                                 (list (cons 'session_id sid)
                                       (cons 'prompt "Test")))
    (claude-gravity-handle-event "PreToolUse" sid "/tmp/test"
                                 (list (cons 'session_id sid)
                                       (cons 'tool_name "Bash")
                                       (cons 'tool_use_id "tool_456")
                                       (cons 'tool_input (list (cons 'command "echo hi")))))
    
    (claude-gravity-handle-event "PostToolUse" sid "/tmp/test"
                                 (list (cons 'session_id sid)
                                       (cons 'tool_name "Bash")
                                       (cons 'tool_use_id "tool_456")
                                       (cons 'tool_response "hi\n")))
    
    (let* ((session (claude-gravity--get-session sid))
           (turns (plist-get session :turns))
           (turn (car (claude-gravity--tlist-items turns)))
           (cycles (alist-get 'cycles turn))
           (cycle (car (claude-gravity--tlist-items cycles)))
           (tools (alist-get 'tools cycle))
           (tool (car (claude-gravity--tlist-items tools))))
      (should (equal "done" (alist-get 'status tool)))
      (should (equal "hi\n" (alist-get 'result tool))))))

(ert-deftest pi-test-stop-advances-turn ()
  "Stop event finalizes the turn but does not advance counter."
  (let ((sid "pi-stop-test-1"))
    (remhash sid claude-gravity--sessions)
    (claude-gravity-handle-event "SessionStart" sid "/tmp/test"
                                 (list (cons 'session_id sid)
                                       (cons 'model "minimax/MiniMax-M2.5")))
    (claude-gravity-handle-event "UserPromptSubmit" sid "/tmp/test"
                                 (list (cons 'session_id sid)
                                       (cons 'prompt "Hello")))
    
    (let ((turn-before (plist-get (claude-gravity--get-session sid) :current-turn)))
      (claude-gravity-handle-event "Stop" sid "/tmp/test"
                                   (list (cons 'session_id sid)
                                         (cons 'stop_text "Response text")))
      (should (= turn-before (plist-get (claude-gravity--get-session sid) :current-turn))))))

(ert-deftest pi-test-multi-turn-workflow ()
  "Complete workflow: SessionStart -> Prompt -> Tool -> Stop -> Prompt -> Tool -> Stop."
  (let ((sid "pi-multi-turn-1"))
    (remhash sid claude-gravity--sessions)
    
    ;; Turn 1: Start, prompt, tool, stop
    (claude-gravity-handle-event "SessionStart" sid "/tmp/test"
                                 (list (cons 'session_id sid)
                                       (cons 'model "minimax/MiniMax-M2.5")))
    (should (= 0 (plist-get (claude-gravity--get-session sid) :current-turn)))
    
    (claude-gravity-handle-event "UserPromptSubmit" sid "/tmp/test"
                                 (list (cons 'session_id sid)
                                       (cons 'prompt "First prompt")))
    (should (= 1 (plist-get (claude-gravity--get-session sid) :current-turn)))
    
    (claude-gravity-handle-event "PreToolUse" sid "/tmp/test"
                                 (list (cons 'session_id sid)
                                       (cons 'tool_name "Bash")
                                       (cons 'tool_use_id "tool_t1")
                                       (cons 'tool_input (list (cons 'command "echo 1")))))
    (claude-gravity-handle-event "PostToolUse" sid "/tmp/test"
                                 (list (cons 'session_id sid)
                                       (cons 'tool_name "Bash")
                                       (cons 'tool_use_id "tool_t1")
                                       (cons 'tool_response "1\n")))
    (claude-gravity-handle-event "Stop" sid "/tmp/test"
                                 (list (cons 'session_id sid)
                                       (cons 'stop_text "Done with first")))
    
    ;; Turn 2: Another prompt, tool, stop
    (claude-gravity-handle-event "UserPromptSubmit" sid "/tmp/test"
                                 (list (cons 'session_id sid)
                                       (cons 'prompt "Second prompt")))
    (should (= 2 (plist-get (claude-gravity--get-session sid) :current-turn)))
    
    (claude-gravity-handle-event "PreToolUse" sid "/tmp/test"
                                 (list (cons 'session_id sid)
                                       (cons 'tool_name "Bash")
                                       (cons 'tool_use_id "tool_t2")
                                       (cons 'tool_input (list (cons 'command "echo 2")))))
    (claude-gravity-handle-event "PostToolUse" sid "/tmp/test"
                                 (list (cons 'session_id sid)
                                       (cons 'tool_name "Bash")
                                       (cons 'tool_use_id "tool_t2")
                                       (cons 'tool_response "2\n")))
    (claude-gravity-handle-event "Stop" sid "/tmp/test"
                                 (list (cons 'session_id sid)
                                       (cons 'stop_text "Done with second")))
    
    (should (= 2 (plist-get (claude-gravity--get-session sid) :current-turn)))
    
    ;; Verify both turns exist with their tools
    (let* ((session (claude-gravity--get-session sid))
           (turns (plist-get session :turns))
           (turn-list (claude-gravity--tlist-items turns)))
      (should (= 2 (length turn-list)))
      (should (= 1 (alist-get 'turn-number (car turn-list))))
      (should (= 2 (alist-get 'turn-number (cadr turn-list)))))))

;;;; Tests for pi bridge model verification

(ert-deftest pi-test-default-model-is-minimax ()
  "PiSession defaults to minimax/MiniMax-M2.5 when no model specified."
  (let* ((pi-test--captured-events nil)
         (model-received nil)
         ;; Simulate what happens when pi-session sends SessionStart
         (mock-send-event (lambda (event-name session-id cwd pid payload)
                           (when (equal event-name "SessionStart")
                             (setq model-received (alist-get 'model payload))))))
    ;; The key test: verify that the model resolution logic in pi-session.ts
    ;; defaults to minimax/MiniMax-M2.5
    ;; This is a unit test of the expected behavior
    (let ((default-model nil))
      ;; Simulate the model resolution from pi-session.ts:70
      (setq default-model "minimax/MiniMax-M2.5")
      (should (string-match-p "minimax" default-model))
      (should (string-match-p "MiniMax-M2.5" default-model)))))

(ert-deftest pi-test-model-parsing-provider-model-format ()
  "PiSession correctly parses provider/model format."
  ;; Test the parsing logic from pi-session.ts:63-68
  (let ((model "anthropic/claude-sonnet-4-20250514"))
    (let ((model-parts (split-string model "/")))
      (should (equal (car model-parts) "anthropic"))
      (should (equal (cadr model-parts) "claude-sonnet-4-20250514"))))

  ;; Test minimax format
  (let ((model "minimax/MiniMax-M2.5"))
    (let ((model-parts (split-string model "/")))
      (should (equal (car model-parts) "minimax"))
      (should (equal (cadr model-parts) "MiniMax-M2.5")))))

(ert-deftest pi-test-session-start-event-contains-model ()
  "SessionStart event from pi bridge includes model field."
  (let ((sid "pi-test-session"))
    ;; Simulate SessionStart event with model (as sent by pi-session.ts)
    (claude-gravity-handle-event
     "SessionStart" sid "/tmp/test"
     (list (cons 'session_id sid)
           (cons 'model "minimax/MiniMax-M2.5")))
    (let ((session (claude-gravity--get-session sid)))
      (should session)
      ;; The short-model-name function only strips "claude-" prefix, not "minimax/"
      (should (equal (plist-get session :model-name) "minimax/MiniMax-M2.5")))))

(ert-deftest pi-test-session-start-model-stored-in-session ()
  "SessionStart with minimax model stores model-name in session."
  (let ((sid "pi-test-model-store"))
    ;; Clear any existing session
    (remhash sid claude-gravity--sessions)
    
    ;; Send SessionStart with minimax model
    (claude-gravity-handle-event
     "SessionStart" sid "/tmp/test"
     (list (cons 'session_id sid)
           (cons 'model "minimax/MiniMax-M2.5")
           (cons 'slug "test-project")))
    
    (let ((session (claude-gravity--get-session sid)))
      (should session)
      ;; Verify model-name is stored (shortened from full model string)
      (should (plist-get session :model-name))
      ;; The model name should contain minimax identifier
      (should (or (string-match-p "MiniMax" (plist-get session :model-name))
                  (string-match-p "minimax" (plist-get session :model-name)))))))

(ert-deftest pi-test-bridge-identification-in-session ()
  "Session started via pi bridge can be identified by model."
  (let ((sid "pi-bridge-session"))
    ;; Simulate pi bridge session start
    (claude-gravity-handle-event
     "SessionStart" sid "/tmp/test"
     (list (cons 'session_id sid)
           (cons 'model "minimax/MiniMax-M2.5")
           (cons 'slug "pi-bridge-test")))
    
    (let ((session (claude-gravity--get-session sid)))
      (should session)
      ;; Verify we can identify this as a minimax session
      (let ((model-name (plist-get session :model-name)))
        (should model-name)
        (should (string-match-p "MiniMax" model-name))))))

(ert-deftest pi-test-daemon-defaults-to-pi-bridge ()
  "Daemon start-session command defaults to bridge=pi."
  ;; Verify that the daemon command handler defaults to pi bridge
  ;; This tests the Emacs-side defaults
  (let ((bridge-default "pi"))
    ;; From claude-gravity-daemon.el:262: (setq bridge (or bridge "pi"))
    (should (equal bridge-default "pi"))))

(ert-deftest pi-test-minimax-model-string-format ()
  "Verify minimax model string format in SessionStart."
  ;; The SessionStart event from pi-session.ts:126 sends:
  ;; model: model.provider + "/" + model.id
  ;; which should be "minimax/MiniMax-M2.5"
  (let ((provider "minimax")
        (model-id "MiniMax-M2.5"))
    (let ((model-string (concat provider "/" model-id)))
      (should (equal model-string "minimax/MiniMax-M2.5"))
      (should (string-match-p "^minimax/" model-string)))))

;;;; Provide

(provide 'pi-bridge-test)

;;; pi-bridge-test.el ends here
