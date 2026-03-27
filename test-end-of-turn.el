;;; test-end-of-turn.el --- End-of-turn rendering and dedup tests  -*- lexical-binding: t; -*-

;; Verifies end-of-turn rendering behavior:
;; a) Rendering: correct faces applied (claude-gravity-agent-stop-text, claude-gravity-thinking)
;; b) Deduplication: stop_text vs last tool's post_text

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'magit-section)
(require 'claude-gravity-core)
(require 'claude-gravity-faces)
(require 'claude-gravity-session)
(require 'claude-gravity-state)
(require 'claude-gravity-text)
(require 'claude-gravity-render)

;; ---------------------------------------------------------------------------
;; Test Infrastructure
;; ---------------------------------------------------------------------------

(defun test-eot--render-stop-text (turn-node)
  "Render stop text from TURN-NODE into a temp buffer.
Returns the buffer (caller should kill it)."
  (let ((buf (generate-new-buffer " *test-eot-render*")))
    (with-current-buffer buf
      (cl-letf (((symbol-function 'window-width) (lambda (&rest _) 80)))
        (magit-insert-section (root)
          (claude-gravity--insert-stop-text turn-node))))
    buf))

(defun test-eot--render-agent-completions (agents)
  "Render agent completions into a temp buffer.
AGENTS is a list of agent alists.  Returns the buffer."
  (let ((buf (generate-new-buffer " *test-eot-render-agents*")))
    (with-current-buffer buf
      (cl-letf (((symbol-function 'window-width) (lambda (&rest _) 80)))
        (magit-insert-section (root)
          (claude-gravity--insert-agent-completions agents))))
    buf))

(defun test-eot--buffer-has-face-p (buf face)
  "Return non-nil if any text in BUF has FACE in its face properties."
  (with-current-buffer buf
    (let ((pos (point-min)) found)
      (while (and (not found) (< pos (point-max)))
        (let ((f (get-text-property pos 'face)))
          (when (or (eq f face)
                    (and (listp f) (memq face f)))
            (setq found t)))
        (setq pos (or (next-single-property-change pos 'face nil (point-max))
                      (point-max))))
      found)))

(defun test-eot--buffer-text-with-face (buf face)
  "Return concatenated text from BUF that has FACE applied."
  (with-current-buffer buf
    (let ((pos (point-min)) parts)
      (while (< pos (point-max))
        (let ((f (get-text-property pos 'face))
              (next (or (next-single-property-change pos 'face nil (point-max))
                        (point-max))))
          (when (or (eq f face)
                    (and (listp f) (memq face f)))
            (push (buffer-substring-no-properties pos next) parts))
          (setq pos next)))
      (string-join (nreverse parts) ""))))

(defun test-eot--count-occurrences (buf text)
  "Count how many times TEXT appears in BUF content."
  (with-current-buffer buf
    (let ((content (buffer-substring-no-properties (point-min) (point-max)))
          (count 0)
          (start 0))
      (while (setq start (string-search text content start))
        (cl-incf count)
        (setq start (+ start (length text))))
      count)))

;; ---------------------------------------------------------------------------
;; Helper: Build turn nodes with tools for dedup tests
;; ---------------------------------------------------------------------------

(defun test-eot--make-turn-with-tool (turn-number &optional post-text post-thinking)
  "Create a turn node with one tool that has POST-TEXT and POST-THINKING."
  (let* ((turn (claude-gravity--make-turn-node turn-number))
         (step (claude-gravity--make-step-node nil nil))
         (tool (list (cons 'tool_use_id (format "tool_%d" turn-number))
                     (cons 'name "Read")
                     (cons 'status "done")
                     (cons 'post_text post-text)
                     (cons 'post_thinking post-thinking))))
    (claude-gravity--tlist-append (alist-get 'steps turn) step)
    (claude-gravity--tlist-append (alist-get 'tools step) tool)
    turn))


;; ===========================================================================
;; Group 1: Rendering Face Tests
;; ===========================================================================

(ert-deftest test-eot/render-stop-text-face ()
  "Stop text renders with `claude-gravity-agent-stop-text' face."
  (let* ((turn (claude-gravity--make-turn-node 1)))
    (setf (alist-get 'stop_text turn) "All done, everything looks good.")
    (let ((buf (test-eot--render-stop-text turn)))
      (unwind-protect
          (progn
            (should (test-eot--buffer-has-face-p buf 'claude-gravity-agent-stop-text))
            (should (string-match-p "All done"
                      (test-eot--buffer-text-with-face buf 'claude-gravity-agent-stop-text))))
        (kill-buffer buf)))))


(ert-deftest test-eot/render-stop-thinking-face ()
  "Stop thinking renders with `claude-gravity-thinking' face."
  (let* ((turn (claude-gravity--make-turn-node 1)))
    (setf (alist-get 'stop_thinking turn) "Let me summarize the work done.")
    (let ((buf (test-eot--render-stop-text turn)))
      (unwind-protect
          (progn
            (should (test-eot--buffer-has-face-p buf 'claude-gravity-thinking))
            (should (string-match-p "summarize"
                      (test-eot--buffer-text-with-face buf 'claude-gravity-thinking))))
        (kill-buffer buf)))))


(ert-deftest test-eot/render-agent-completion-face ()
  "Agent completion text renders with `claude-gravity-agent-stop-text' face."
  (let* ((agents (list (list (cons 'status "done")
                             (cons 'type "explore")
                             (cons 'stop_text "Agent found 42 matches.")
                             (cons 'stop_thinking nil))))
         (buf (test-eot--render-agent-completions agents)))
    (unwind-protect
        (progn
          (should (test-eot--buffer-has-face-p buf 'claude-gravity-agent-stop-text))
          (should (string-match-p "42 matches"
                    (test-eot--buffer-text-with-face buf 'claude-gravity-agent-stop-text)))
          ;; Should also have the "Agent completed:" label
          (with-current-buffer buf
            (should (string-match-p "explore.*completed"
                      (buffer-substring-no-properties (point-min) (point-max))))))
      (kill-buffer buf))))


(ert-deftest test-eot/render-agent-thinking-face ()
  "Agent completion thinking renders with `claude-gravity-thinking' face."
  (let* ((agents (list (list (cons 'status "done")
                             (cons 'type "explore")
                             (cons 'stop_text nil)
                             (cons 'stop_thinking "Reflecting on findings."))))
         (buf (test-eot--render-agent-completions agents)))
    (unwind-protect
        (progn
          (should (test-eot--buffer-has-face-p buf 'claude-gravity-thinking))
          (should (string-match-p "Reflecting"
                    (test-eot--buffer-text-with-face buf 'claude-gravity-thinking))))
      (kill-buffer buf))))


(ert-deftest test-eot/render-empty-stop-nothing ()
  "Turn with nil stop_text produces no agent-stop-text face."
  (let* ((turn (claude-gravity--make-turn-node 1))
         (buf (test-eot--render-stop-text turn)))
    (unwind-protect
        (should-not (test-eot--buffer-has-face-p buf 'claude-gravity-agent-stop-text))
      (kill-buffer buf))))


;; ===========================================================================
;; Group 2: Dedup Logic Tests
;; ===========================================================================

(ert-deftest test-eot/dedup-equal ()
  "When stop_text equals last tool's post_text, stop_text is suppressed."
  (let* ((turn (test-eot--make-turn-with-tool 1 "Shared conclusion." nil)))
    (setf (alist-get 'stop_text turn) "Shared conclusion.")
    (let ((buf (test-eot--render-stop-text turn)))
      (unwind-protect
          ;; stop_text suppressed because post_text >= length of stop_text.
          ;; insert-stop-text produces no output for the stop portion.
          (should (= 0 (test-eot--count-occurrences buf "Shared conclusion.")))
        (kill-buffer buf)))))


(ert-deftest test-eot/dedup-stop-subsumes ()
  "When stop_text extends post_text, post_text is cleared and stop_text shown."
  (let* ((turn (test-eot--make-turn-with-tool 1 "Short" nil)))
    (setf (alist-get 'stop_text turn) "Short\n\nWith more context.")
    (let ((buf (test-eot--render-stop-text turn)))
      (unwind-protect
          (progn
            ;; stop_text is longer, so post_text nilled, stop_text rendered
            (should (test-eot--buffer-has-face-p buf 'claude-gravity-agent-stop-text))
            (should (string-match-p "more context"
                      (test-eot--buffer-text-with-face buf 'claude-gravity-agent-stop-text)))
            ;; Verify the tool's post_text was cleared
            (let* ((steps (claude-gravity--tlist-items (alist-get 'steps turn)))
                   (last-step (car (last steps)))
                   (tools (claude-gravity--tlist-items (alist-get 'tools last-step)))
                   (last-tool (car (last tools))))
              (should (null (alist-get 'post_text last-tool)))))
        (kill-buffer buf)))))


(ert-deftest test-eot/dedup-post-subsumes ()
  "When post_text extends stop_text, stop_text is suppressed."
  (let* ((turn (test-eot--make-turn-with-tool 1 "Long\n\nWith details." nil)))
    (setf (alist-get 'stop_text turn) "Long")
    (let ((buf (test-eot--render-stop-text turn)))
      (unwind-protect
          ;; stop_text shorter -> suppressed. insert-stop-text renders nothing.
          (should-not (test-eot--buffer-has-face-p buf 'claude-gravity-agent-stop-text))
        (kill-buffer buf)))))


(ert-deftest test-eot/dedup-no-overlap ()
  "When stop_text and post_text don't overlap, both are preserved."
  (let* ((turn (test-eot--make-turn-with-tool 1 "Tool output analysis." nil)))
    (setf (alist-get 'stop_text turn) "Completely different conclusion.")
    (let ((buf (test-eot--render-stop-text turn)))
      (unwind-protect
          (progn
            ;; stop_text rendered (no dedup since texts differ)
            (should (test-eot--buffer-has-face-p buf 'claude-gravity-agent-stop-text))
            (should (string-match-p "different conclusion"
                      (test-eot--buffer-text-with-face buf 'claude-gravity-agent-stop-text))))
        (kill-buffer buf)))))


(ert-deftest test-eot/dedup-thinking ()
  "When stop_thinking equals last tool's post_thinking, thinking is suppressed."
  (let* ((turn (test-eot--make-turn-with-tool 1 nil "Same reflection.")))
    (setf (alist-get 'stop_thinking turn) "Same reflection.")
    (let ((buf (test-eot--render-stop-text turn)))
      (unwind-protect
          ;; stop_thinking suppressed (equal to post_thinking, post is >= length)
          (should-not (test-eot--buffer-has-face-p buf 'claude-gravity-thinking))
        (kill-buffer buf)))))


(ert-deftest test-eot/text-subsumes-p-unit ()
  "Unit tests for the text-subsumes-p helper."
  ;; Equal strings
  (should (claude-gravity--text-subsumes-p "A" "A"))
  ;; A is prefix of B (with paragraph break)
  (should (claude-gravity--text-subsumes-p "A\n\nB" "A"))
  ;; B is prefix of A (reverse)
  (should (claude-gravity--text-subsumes-p "A" "A\n\nB"))
  ;; No paragraph boundary -- NOT subsumes
  (should-not (claude-gravity--text-subsumes-p "AB" "A"))
  ;; Empty string
  (should-not (claude-gravity--text-subsumes-p "" "A"))
  ;; Nil
  (should-not (claude-gravity--text-subsumes-p nil "A"))
  (should-not (claude-gravity--text-subsumes-p "A" nil)))


(provide 'test-end-of-turn)
;;; test-end-of-turn.el ends here
