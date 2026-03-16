;;; claude-gravity-smoke-test.el --- Installation smoke tests  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Smoke tests that validate the Emacs side of emacs-gravity installation:
;; - All modules load without error
;; - Can connect to a running gravity-server
;; - Receives overview.snapshot from server
;;
;; Run with gravity-server already started (GRAVITY_TERMINAL_SOCK env var set):
;;   emacs --batch -L /workspace -l claude-gravity-smoke-test -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)

;; ── Test 1: All modules load ─────────────────────────────────────────

(ert-deftest smoke-test-require-loads ()
  "Loading claude-gravity pulls in all 15+ modules without error."
  (require 'claude-gravity)
  ;; Spot-check critical symbols exist
  (should (fboundp 'claude-gravity-status))
  (should (fboundp 'claude-gravity-server-start))
  (should (boundp 'claude-gravity--sessions))
  (should (fboundp 'claude-gravity--apply-patch))
  (should (fboundp 'claude-gravity--tlist-new)))

;; ── Test 2: Connect to server ────────────────────────────────────────

(ert-deftest smoke-test-server-connection ()
  "Can connect to gravity-server terminal socket."
  (require 'claude-gravity)
  (let ((sock-path (getenv "GRAVITY_TERMINAL_SOCK")))
    (unless sock-path
      (ert-skip "GRAVITY_TERMINAL_SOCK not set"))
    (unless (file-exists-p sock-path)
      (ert-skip (format "Socket not found: %s" sock-path)))
    (let ((proc (make-network-process
                 :name "smoke-test-conn"
                 :family 'local
                 :service sock-path
                 :noquery t)))
      (should (processp proc))
      (should (eq (process-status proc) 'open))
      (delete-process proc))))

;; ── Test 3: Receives overview.snapshot ───────────────────────────────

(ert-deftest smoke-test-overview-received ()
  "Connecting to server yields an overview.snapshot message."
  (require 'claude-gravity)
  (let ((sock-path (getenv "GRAVITY_TERMINAL_SOCK")))
    (unless sock-path
      (ert-skip "GRAVITY_TERMINAL_SOCK not set"))
    (unless (file-exists-p sock-path)
      (ert-skip (format "Socket not found: %s" sock-path)))
    (let ((received-data "")
          (proc nil))
      (setq proc (make-network-process
                  :name "smoke-test-overview"
                  :family 'local
                  :service sock-path
                  :noquery t
                  :filter (lambda (_proc data)
                            (setq received-data (concat received-data data)))))
      (should (processp proc))
      ;; Wait up to 2 seconds for data
      (let ((deadline (+ (float-time) 2.0)))
        (while (and (< (float-time) deadline)
                    (not (string-match-p "overview\\.snapshot" received-data)))
          (accept-process-output proc 0.1)))
      (delete-process proc)
      (should (string-match-p "overview\\.snapshot" received-data)))))

(provide 'claude-gravity-smoke-test)
;;; claude-gravity-smoke-test.el ends here
