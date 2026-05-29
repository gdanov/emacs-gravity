;;; claude-gravity-pull-signal-test.el --- Pull signal/poll behavior  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Phase 3.5 — signal-fed client tests.  Push terminal communication was
;; removed; the signal+poll path is now the ONLY route for inbox actions
;; (permission / question / plan-review).  These drive
;; `claude-gravity--handle-server-message' with recorded server signals and
;; assert: inbox/notice signals poll IMMEDIATELY (the original regression's
;; UX completion), session/overview stay debounced, and an inbox-items poll
;; response converges local inbox to server truth.

;;; Code:

(require 'ert)
(require 'claude-gravity)

(defmacro cgps--with-captured-poll (calls-var &rest body)
  "Run BODY with `claude-gravity--send-to-server' captured into CALLS-VAR.
Also stubs inbox-indicator/refresh so the test stays pure in batch."
  (declare (indent 1))
  `(let ((,calls-var nil)
         (claude-gravity--poll-timer nil)
         (claude-gravity--pending-signal nil))
     (cl-letf (((symbol-function 'claude-gravity--send-to-server)
                (lambda (msg) (push msg ,calls-var)))
               ((symbol-function 'claude-gravity--update-inbox-indicator)
                #'ignore)
               ((symbol-function 'claude-gravity--schedule-refresh)
                #'ignore))
       ,@body)))

(ert-deftest cgps-inbox-signal-polls-immediately ()
  "A `state-changed' what=inbox signal must poll synchronously, no timer."
  (cgps--with-captured-poll calls
    (claude-gravity--handle-server-message
     '((type . "state-changed") (what . "inbox") (seq . 7)))
    (should (member '((type . "poll")) calls))
    (should (null claude-gravity--poll-timer))
    (should (null claude-gravity--pending-signal))))

(ert-deftest cgps-notice-signal-polls-immediately ()
  "A `state-changed' what=notice signal also polls synchronously."
  (cgps--with-captured-poll calls
    (claude-gravity--handle-server-message
     '((type . "state-changed") (what . "notice") (seq . 1)))
    (should (member '((type . "poll")) calls))
    (should (null claude-gravity--poll-timer))))

(ert-deftest cgps-session-signal-is-debounced ()
  "A `state-changed' what=session signal must NOT poll synchronously;
it schedules an idle fetch instead (pull mode's anti-UI-churn property)."
  (cgps--with-captured-poll calls
    (claude-gravity--handle-server-message
     '((type . "state-changed") (what . "session")
       (sessionId . "s1") (seq . 3)))
    (should (null calls))                       ; no immediate poll
    (should claude-gravity--poll-timer)         ; idle timer scheduled
    ;; cleanup: don't leak the timer into other tests
    (cancel-timer claude-gravity--poll-timer)
    (setq claude-gravity--poll-timer nil)))

(ert-deftest cgps-overview-signal-is-debounced ()
  "what=overview is debounced like session."
  (cgps--with-captured-poll calls
    (claude-gravity--handle-server-message
     '((type . "state-changed") (what . "overview") (seq . 2)))
    (should (null calls))
    (should claude-gravity--poll-timer)
    (cancel-timer claude-gravity--poll-timer)
    (setq claude-gravity--poll-timer nil)))

(ert-deftest cgps-inbox-items-converges-to-question ()
  "An `inbox-items' poll response replaces local inbox with server truth;
a question item stays a question (not coerced to permission)."
  (let ((claude-gravity--inbox nil))
    (cl-letf (((symbol-function 'claude-gravity--update-inbox-indicator)
               #'ignore)
              ((symbol-function 'claude-gravity--schedule-refresh)
               #'ignore))
      (claude-gravity--handle-server-message
       '((type . "inbox-items")
         (items . (((id . 1)
                    (type . "question")
                    (sessionId . "s1")
                    (project . "proj")
                    (label . "my-slug")
                    (timestamp . 0)
                    (summary . "Pick one")
                    (data . ((tool_name . "AskUserQuestion")
                             (tool_use_id . "tu_q"))))))))
      (should (= 1 (length claude-gravity--inbox)))
      (should (eq 'question (alist-get 'type (car claude-gravity--inbox))))
      (should (= 1 (alist-get 'id (car claude-gravity--inbox)))))))

(provide 'claude-gravity-pull-signal-test)
;;; claude-gravity-pull-signal-test.el ends here
