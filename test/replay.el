;;; cg-test-replay.el --- Replay infrastructure for Claude Gravity tests  -*- lexical-binding: t; -*-

;;; Commentary:

;; Replays captured bridge output (JSON arrays of socket messages) through
;; `claude-gravity-handle-event' for ERT testing.  Each fixture file is a
;; JSON array where every element matches the socket message format:
;;
;;   {"event": "...", "session_id": "...", "cwd": "...", "pid": N, "data": {...}}
;;
;; Fixtures can be:
;;   - Captured live via CLAUDE_GRAVITY_DUMP_DIR (bridge output files)
;;   - Hand-crafted for specific test scenarios

;;; Code:

(require 'claude-gravity)

(defun cg-test-replay-transcript (fixture-file)
  "Replay FIXTURE-FILE through `claude-gravity-handle-event'.
FIXTURE-FILE is a JSON array of socket messages.
Returns the session-id from the last event."
  (let* ((json-str (with-temp-buffer
                     (insert-file-contents fixture-file)
                     (buffer-string)))
         (events (json-parse-string json-str
                                    :object-type 'alist
                                    :array-type 'array))
         (last-sid nil))
    (seq-doseq (msg events)
      (let ((event (alist-get 'event msg))
            (sid (alist-get 'session_id msg))
            (cwd (alist-get 'cwd msg))
            (pid (alist-get 'pid msg))
            (data (alist-get 'data msg)))
        (setq last-sid sid)
        (claude-gravity-handle-event event sid cwd data pid)))
    last-sid))

(provide 'cg-test-replay)
;;; cg-test-replay.el ends here
