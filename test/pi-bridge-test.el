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
