;;; cg-test-replay.el --- Replay JSON transcripts for ERT tests  -*- lexical-binding: t; -*-

;;; Code:

(require 'claude-gravity)

(defun cg-test-replay-transcript (json-file)
  "Replay events from JSON-FILE through `claude-gravity-handle-event'.
JSON-FILE is a JSON array of event objects, each with keys:
  event, session_id, cwd, pid, data.
Returns the session-id from the first event."
  (let* ((events (claude-gravity--json-read-file json-file))
         (first-sid nil))
    (dolist (ev events)
      (let ((event (alist-get 'event ev))
            (sid (alist-get 'session_id ev))
            (cwd (alist-get 'cwd ev))
            (pid (alist-get 'pid ev))
            (data (alist-get 'data ev)))
        (unless first-sid (setq first-sid sid))
        (claude-gravity-handle-event event sid cwd data pid)))
    first-sid))

(provide 'cg-test-replay)
;;; cg-test-replay.el ends here
