;;; driver.el --- e2e harness driver helpers  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Loaded into an `emacs --daemon' running the REAL gravity client (full
;; command loop: network filter, idle/poll timers, magit-section render).
;; The scenario runner calls these via `emacsclient --eval'.
;;
;; Result-passing is FILE-BASED on purpose: emacsclient prints Lisp values
;; with quoting/escaping that is painful to parse from a shell. Predicates
;; return t/nil (printed cleanly); buffer/state dumps are written to a path
;; the runner reads. No screen-scraping, no print-format guessing.
;;
;; GRAVITY_TERMINAL_SOCK / GRAVITY_HOOK_SOCK / GRAVITY_PID_FILE are read by
;; claude-gravity at load time, so the daemon must be started with those
;; env vars already pointing at the proxy / per-run sockets.

;;; Code:

(require 'claude-gravity)
(require 'cl-lib)

(defun cge-connect ()
  "Connect the client to the (proxy) terminal socket. Connect-only —
the runner owns the server process, Emacs must NOT spawn it.
Returns t if the network process is live."
  (claude-gravity--client-connect)
  (and claude-gravity--client-process
       (process-live-p claude-gravity--client-process)
       t))

(defun cge-pump (secs)
  "Pump process output and timers for SECS seconds.
Runs the network filter and lets idle/poll timers fire (the daemon is
perpetually idle, so `run-with-idle-timer' fetches fire promptly)."
  (let ((deadline (+ (float-time) secs)))
    (while (< (float-time) deadline)
      (accept-process-output nil 0.05)
      (sit-for 0.05)))
  t)

(defun cge-poll ()
  "Force an immediate pull poll (deterministic — no waiting on the
idle timer). Returns t."
  (claude-gravity--poll-now)
  (cge-pump 0.3)
  t)

(defun cge-wait (pred-string timeout)
  "Eval PRED-STRING (a Lisp form in a string) repeatedly, pumping
between tries, until non-nil or TIMEOUT seconds. Returns t/nil."
  (let ((form (car (read-from-string pred-string)))
        (deadline (+ (float-time) timeout))
        (ok nil))
    (while (and (not ok) (< (float-time) deadline))
      (setq ok (ignore-errors (eval form t)))
      (unless ok
        (accept-process-output nil 0.1)
        (sit-for 0.1)))
    (and ok t)))

(defun cge-buffer-names (regexp)
  "Write (one per line) live buffer names matching REGEXP — return count."
  (length (seq-filter (lambda (b) (string-match-p regexp (buffer-name b)))
                       (buffer-list))))

(defun cge-bmatch (buffer regexp)
  "Return t if BUFFER exists and its text matches REGEXP, else nil."
  (let ((b (get-buffer buffer)))
    (and b
         (with-current-buffer b
           (save-excursion
             (goto-char (point-min))
             (and (re-search-forward regexp nil t) t))))))

(defun cge--show-section-tree (section)
  "Recursively `magit-section-show' SECTION and all descendants.
Showing a section runs its (possibly deferred) washer, which inserts
children whose own washers must then be run — hence the post-order
recursion AFTER showing."
  (magit-section-show section)
  (dolist (child (oref section children))
    (cge--show-section-tree child)))

(defun cge-expand-all-match (name-regexp)
  "Recursively expand every magit-section in the first buffer matching
NAME-REGEXP. Frozen turns defer their children to a washer that only
runs when the section is shown; expanding forces those washers so a
subsequent dump captures the full transcript. Two passes — the first
runs turn washers (inserting Edited Files etc.), the second expands the
newly-materialised subsections. Returns t, or nil if no buffer matched."
  (let ((b (cge--first-matching name-regexp)))
    (and b
         (with-current-buffer b
           (when (and (boundp 'magit-root-section) magit-root-section)
             (cge--show-section-tree magit-root-section)
             (cge--show-section-tree magit-root-section))
           t))))

(defun cge-dump (buffer path)
  "Write BUFFER's text (no properties) to PATH. Returns t (empty file
if the buffer is absent, so the runner can still assert)."
  (let ((b (get-buffer buffer)))
    (with-temp-file path
      (when b
        (insert (with-current-buffer b
                  (buffer-substring-no-properties (point-min) (point-max))))))
    t))

(defun cge--first-matching (regexp)
  "Return the first live buffer whose name matches REGEXP, or nil.
Buffer names (inbox id, session slug) are not predictable, so scenarios
target by name pattern."
  (seq-find (lambda (b) (string-match-p regexp (buffer-name b)))
            (buffer-list)))

(defun cge-dump-match (name-regexp path)
  "Write the first buffer whose name matches NAME-REGEXP to PATH (empty
file if none). Returns t."
  (let ((b (cge--first-matching name-regexp)))
    (with-temp-file path
      (when b (insert (with-current-buffer b
                        (buffer-substring-no-properties (point-min) (point-max))))))
    t))

(defun cge-eval-in-match (name-regexp form-string)
  "Eval FORM-STRING with the first buffer matching NAME-REGEXP current
\(drive an action on a buffer whose exact name is unpredictable).
Returns t on success, nil if no buffer or the form errors."
  (let ((b (cge--first-matching name-regexp)))
    (and b
         (condition-case err
             (with-current-buffer b
               (eval (car (read-from-string form-string)) t) t)
           (error (message "cge-eval-in-match error: %S" err) nil)))))

(defun cge-eval-in (buffer form-string)
  "Eval FORM-STRING with BUFFER current (drive an action: answer a
question, approve a plan, press a key via its command). Returns t on
success, nil on error or missing buffer."
  (let ((b (get-buffer buffer)))
    (and b
         (condition-case err
             (with-current-buffer b
               (eval (car (read-from-string form-string)) t)
               t)
           (error
            (message "cge-eval-in error: %S" err)
            nil)))))

(defun cge-open-inbox (session-id)
  "Open the action buffer for SESSION-ID's first pending inbox item,
dispatching by type exactly as the UI does (permission / question /
plan-review). Synchronous (no run-at-time defer) for deterministic
driving. Returns the type as a string, or nil if no pending item."
  (let ((item (cl-find-if
               (lambda (it)
                 (and (equal (alist-get 'session-id it) session-id)
                      (memq (alist-get 'type it)
                            '(permission question plan-review))))
               claude-gravity--inbox)))
    (when item
      (pcase (alist-get 'type item)
        ('permission   (claude-gravity--inbox-act-permission item))
        ('question     (claude-gravity--inbox-act-question item))
        ('plan-review  (claude-gravity--inbox-act-plan-review item)))
      (format "%s" (alist-get 'type item)))))

(defun cge-pi-text-pending-p ()
  "Return t if a pi input/editor inbox item (data.pi_ui.kind = \"text\")
is pending — unambiguous regardless of session id."
  (and (cl-find-if
        (lambda (it)
          (let* ((d (alist-get 'data it))
                 (pi (alist-get 'pi_ui d)))
            (equal (and pi (alist-get 'kind pi)) "text")))
        claude-gravity--inbox)
       t))

(defun cge-open-pi-text ()
  "Open the pending pi text-entry item's action buffer via the real
dispatch (`claude-gravity--inbox-act-question' → pi-text branch).
Returns t, or nil if none pending."
  (let ((item (cl-find-if
               (lambda (it)
                 (let* ((d (alist-get 'data it))
                        (pi (alist-get 'pi_ui d)))
                   (equal (and pi (alist-get 'kind pi)) "text")))
               claude-gravity--inbox)))
    (and item
         (progn (claude-gravity--inbox-act-question item) t))))

(defun cge-state (path)
  "Write a small JSON snapshot of sessions + inbox to PATH (debugging
and coarse assertions). Returns t."
  (let (sessions)
    (maphash
     (lambda (id s)
       (push (list (cons 'id id)
                   (cons 'status (format "%s" (plist-get s :status)))
                   (cons 'claude-status (format "%s" (plist-get s :claude-status)))
                   (cons 'turns (length (claude-gravity--tlist-items
                                         (or (plist-get s :turns)
                                             (claude-gravity--tlist-new))))))
             sessions))
     claude-gravity--sessions)
    (with-temp-file path
      (insert (json-encode
               (list (cons 'sessions sessions)
                     (cons 'inbox (length claude-gravity--inbox))))))
    t))

(provide 'driver)
;;; driver.el ends here
