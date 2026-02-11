;;; test-end-of-turn.el --- End-of-turn replay tests  -*- lexical-binding: t; -*-

;; Verifies end-of-turn message handling across the full Emacs pipeline:
;; a) View model state: stop_text/stop_thinking stored on turn nodes and agents
;; b) Rendering: correct faces applied (claude-gravity-assistant-text, claude-gravity-thinking)
;; c) Deduplication: stop_text vs last tool's post_text

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'magit-section)
(require 'claude-gravity-core)
(require 'claude-gravity-faces)
(require 'claude-gravity-session)
(require 'claude-gravity-state)
(require 'claude-gravity-events)
(require 'claude-gravity-text)
(require 'claude-gravity-render)

;; ---------------------------------------------------------------------------
;; Test Infrastructure
;; ---------------------------------------------------------------------------

(defvar test-eot--counter 0
  "Counter for generating unique session IDs.")

(defun test-eot--unique-id ()
  "Return a unique session ID for testing."
  (cl-incf test-eot--counter)
  (format "test-eot-%d-%d" (float-time) test-eot--counter))

(defun test-eot--feed-event (event session-id data)
  "Feed EVENT for SESSION-ID with DATA through the event handler.
Suppresses side-effects: refresh scheduling, inbox, notifications."
  (cl-letf (((symbol-function 'claude-gravity--schedule-refresh) #'ignore)
            ((symbol-function 'claude-gravity--schedule-session-refresh) #'ignore)
            ((symbol-function 'claude-gravity--inbox-add) #'ignore)
            ((symbol-function 'claude-gravity--inbox-remove-for-session) #'ignore)
            ((symbol-function 'claude-gravity--clear-notification-indicator) #'ignore)
            ((symbol-function 'claude-gravity--update-notification-indicator) #'ignore)
            ((symbol-function 'claude-gravity--dismiss-stale-inbox-items) #'ignore))
    (claude-gravity-handle-event event session-id "/tmp/test-eot" data)))

(defun test-eot--get-session (session-id)
  "Get the session plist for SESSION-ID."
  (claude-gravity--get-session session-id))

(defun test-eot--current-turn-node (session)
  "Get the current turn node from SESSION."
  (claude-gravity--current-turn-node session))

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

(defun test-eot--cleanup-session (session-id)
  "Remove test session from the global hash table."
  (remhash session-id claude-gravity--sessions))

;; ---------------------------------------------------------------------------
;; Helper: Build turn nodes with tools for dedup tests
;; ---------------------------------------------------------------------------

(defun test-eot--make-turn-with-tool (turn-number &optional post-text post-thinking)
  "Create a turn node with one tool that has POST-TEXT and POST-THINKING."
  (let* ((turn (claude-gravity--make-turn-node turn-number))
         (cycle (claude-gravity--make-cycle-node nil nil))
         (tool (list (cons 'tool_use_id (format "tool_%d" turn-number))
                     (cons 'name "Read")
                     (cons 'status "done")
                     (cons 'post_text post-text)
                     (cons 'post_thinking post-thinking))))
    (claude-gravity--tlist-append (alist-get 'cycles turn) cycle)
    (claude-gravity--tlist-append (alist-get 'tools cycle) tool)
    turn))


;; ===========================================================================
;; Group 1: View Model State Tests
;; ===========================================================================

(ert-deftest test-eot/stop-text-on-turn ()
  "Stop event stores stop_text and stop_thinking on the current turn node."
  (let* ((sid (test-eot--unique-id)))
    (unwind-protect
        (progn
          (test-eot--feed-event "UserPromptSubmit" sid
            `((prompt . "Fix the bug")))
          (test-eot--feed-event "PreToolUse" sid
            `((tool_use_id . "t1") (tool_name . "Read")
              (tool_input . ((file_path . "/src/main.ts")))))
          (test-eot--feed-event "PostToolUse" sid
            `((tool_use_id . "t1") (tool_name . "Read")
              (tool_response . "file contents")))
          (test-eot--feed-event "Stop" sid
            `((stop_text . "All done, bug is fixed.")
              (stop_thinking . "Let me wrap up the findings.")))
          (let* ((session (test-eot--get-session sid))
                 (turn (test-eot--current-turn-node session)))
            (should (equal (alist-get 'stop_text turn) "All done, bug is fixed."))
            (should (equal (alist-get 'stop_thinking turn) "Let me wrap up the findings."))))
      (test-eot--cleanup-session sid))))


(ert-deftest test-eot/stop-text-nil-when-absent ()
  "Stop event without stop_text leaves turn node values as nil."
  (let* ((sid (test-eot--unique-id)))
    (unwind-protect
        (progn
          (test-eot--feed-event "UserPromptSubmit" sid
            `((prompt . "Hello")))
          (test-eot--feed-event "Stop" sid '())
          (let* ((session (test-eot--get-session sid))
                 (turn (test-eot--current-turn-node session)))
            (should (null (alist-get 'stop_text turn)))
            (should (null (alist-get 'stop_thinking turn)))))
      (test-eot--cleanup-session sid))))


(ert-deftest test-eot/stop-text-turn-0 ()
  "Stop event stores stop_text on turn 0 (pre-prompt, no UserPromptSubmit)."
  (let* ((sid (test-eot--unique-id)))
    (unwind-protect
        (progn
          (test-eot--feed-event "PreToolUse" sid
            `((tool_use_id . "t1") (tool_name . "Glob")
              (tool_input . ((pattern . "**/*.el")))))
          (test-eot--feed-event "PostToolUse" sid
            `((tool_use_id . "t1") (tool_name . "Glob")
              (tool_response . "found files")))
          (test-eot--feed-event "Stop" sid
            `((stop_text . "Found 5 files in the project.")))
          (let* ((session (test-eot--get-session sid))
                 (turn (test-eot--current-turn-node session)))
            (should (equal (alist-get 'stop_text turn) "Found 5 files in the project."))))
      (test-eot--cleanup-session sid))))


(ert-deftest test-eot/agent-stop-text ()
  "SubagentStop stores stop_text and stop_thinking on agent alist."
  (let* ((sid (test-eot--unique-id)))
    (unwind-protect
        (progn
          (test-eot--feed-event "UserPromptSubmit" sid
            `((prompt . "Research this")))
          (test-eot--feed-event "SubagentStart" sid
            `((agent_id . "agent_001") (agent_type . "explore")
              (agent_transcript_path . "/tmp/test-agent.jsonl")))
          (test-eot--feed-event "SubagentStop" sid
            `((agent_id . "agent_001")
              (agent_stop_text . "Agent completed the research.")
              (agent_stop_thinking . "Based on my analysis.")))
          (let* ((session (test-eot--get-session sid))
                 (agent (claude-gravity--find-agent session "agent_001")))
            (should agent)
            (should (equal (alist-get 'stop_text agent) "Agent completed the research."))
            (should (equal (alist-get 'stop_thinking agent) "Based on my analysis."))
            (should (equal (alist-get 'status agent) "done"))))
      (test-eot--cleanup-session sid))))


(ert-deftest test-eot/agent-stop-text-nil ()
  "SubagentStop without text leaves agent's stop_text as nil."
  (let* ((sid (test-eot--unique-id)))
    (unwind-protect
        (progn
          (test-eot--feed-event "SubagentStart" sid
            `((agent_id . "agent_002") (agent_type . "explore")))
          (test-eot--feed-event "SubagentStop" sid
            `((agent_id . "agent_002")))
          (let* ((session (test-eot--get-session sid))
                 (agent (claude-gravity--find-agent session "agent_002")))
            (should agent)
            (should (null (alist-get 'stop_text agent)))
            (should (null (alist-get 'stop_thinking agent)))
            (should (equal (alist-get 'status agent) "done"))))
      (test-eot--cleanup-session sid))))


(ert-deftest test-eot/post-tool-text ()
  "PostToolUse stores post_tool_text and post_tool_thinking on tool alist."
  (let* ((sid (test-eot--unique-id)))
    (unwind-protect
        (progn
          (test-eot--feed-event "PreToolUse" sid
            `((tool_use_id . "t1") (tool_name . "Read")
              (tool_input . ((file_path . "/src/main.ts")))))
          (test-eot--feed-event "PostToolUse" sid
            `((tool_use_id . "t1") (tool_name . "Read")
              (tool_response . "file contents")
              (post_tool_text . "Now I understand the structure.")
              (post_tool_thinking . "Interesting pattern here.")))
          (let* ((session (test-eot--get-session sid))
                 (tool (claude-gravity-model-find-tool session "t1")))
            (should tool)
            (should (equal (alist-get 'post_text tool) "Now I understand the structure."))
            (should (equal (alist-get 'post_thinking tool) "Interesting pattern here."))))
      (test-eot--cleanup-session sid))))


(ert-deftest test-eot/multi-turn-stop-text ()
  "Multiple turns each get their own distinct stop_text."
  (let* ((sid (test-eot--unique-id)))
    (unwind-protect
        (progn
          ;; Turn 1
          (test-eot--feed-event "UserPromptSubmit" sid '((prompt . "First")))
          (test-eot--feed-event "Stop" sid '((stop_text . "Stop text for turn 1")))
          ;; Turn 2
          (test-eot--feed-event "UserPromptSubmit" sid '((prompt . "Second")))
          (test-eot--feed-event "Stop" sid '((stop_text . "Stop text for turn 2")))
          ;; Turn 3
          (test-eot--feed-event "UserPromptSubmit" sid '((prompt . "Third")))
          (test-eot--feed-event "Stop" sid '((stop_text . "Stop text for turn 3")))

          (let* ((session (test-eot--get-session sid))
                 (turns (claude-gravity--tlist-items (plist-get session :turns))))
            ;; Turn 0 is the pre-prompt turn (no stop text)
            ;; Turns 1-3 are our prompted turns
            (should (equal (alist-get 'stop_text (nth 1 turns)) "Stop text for turn 1"))
            (should (equal (alist-get 'stop_text (nth 2 turns)) "Stop text for turn 2"))
            (should (equal (alist-get 'stop_text (nth 3 turns)) "Stop text for turn 3"))))
      (test-eot--cleanup-session sid))))


(ert-deftest test-eot/elapsed-computed ()
  "Stop event computes elapsed time on the prompt entry."
  (let* ((sid (test-eot--unique-id)))
    (unwind-protect
        (progn
          (test-eot--feed-event "UserPromptSubmit" sid '((prompt . "Go")))
          ;; Small delay to ensure non-zero elapsed
          (sleep-for 0.05)
          (test-eot--feed-event "Stop" sid '())
          (let* ((session (test-eot--get-session sid))
                 (turn (test-eot--current-turn-node session))
                 (prompt (alist-get 'prompt turn)))
            (should (listp prompt))
            (should (numberp (alist-get 'elapsed prompt)))
            (should (> (alist-get 'elapsed prompt) 0))))
      (test-eot--cleanup-session sid))))


;; ===========================================================================
;; Group 2: Rendering Face Tests
;; ===========================================================================

(ert-deftest test-eot/render-stop-text-face ()
  "Stop text renders with `claude-gravity-assistant-text' face."
  (let* ((turn (claude-gravity--make-turn-node 1)))
    (setf (alist-get 'stop_text turn) "All done, everything looks good.")
    (let ((buf (test-eot--render-stop-text turn)))
      (unwind-protect
          (progn
            (should (test-eot--buffer-has-face-p buf 'claude-gravity-assistant-text))
            (should (string-match-p "All done"
                      (test-eot--buffer-text-with-face buf 'claude-gravity-assistant-text))))
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
  "Turn with nil stop_text produces no assistant-text face."
  (let* ((turn (claude-gravity--make-turn-node 1))
         (buf (test-eot--render-stop-text turn)))
    (unwind-protect
        (should-not (test-eot--buffer-has-face-p buf 'claude-gravity-assistant-text))
      (kill-buffer buf))))


;; ===========================================================================
;; Group 3: Dedup Logic Tests
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
            (should (test-eot--buffer-has-face-p buf 'claude-gravity-assistant-text))
            (should (string-match-p "more context"
                      (test-eot--buffer-text-with-face buf 'claude-gravity-assistant-text)))
            ;; Verify the tool's post_text was cleared
            (let* ((cycles (claude-gravity--tlist-items (alist-get 'cycles turn)))
                   (last-cycle (car (last cycles)))
                   (tools (claude-gravity--tlist-items (alist-get 'tools last-cycle)))
                   (last-tool (car (last tools))))
              (should (null (alist-get 'post_text last-tool)))))
        (kill-buffer buf)))))


(ert-deftest test-eot/dedup-post-subsumes ()
  "When post_text extends stop_text, stop_text is suppressed."
  (let* ((turn (test-eot--make-turn-with-tool 1 "Long\n\nWith details." nil)))
    (setf (alist-get 'stop_text turn) "Long")
    (let ((buf (test-eot--render-stop-text turn)))
      (unwind-protect
          ;; stop_text shorter → suppressed. insert-stop-text renders nothing.
          (should-not (test-eot--buffer-has-face-p buf 'claude-gravity-assistant-text))
        (kill-buffer buf)))))


(ert-deftest test-eot/dedup-no-overlap ()
  "When stop_text and post_text don't overlap, both are preserved."
  (let* ((turn (test-eot--make-turn-with-tool 1 "Tool output analysis." nil)))
    (setf (alist-get 'stop_text turn) "Completely different conclusion.")
    (let ((buf (test-eot--render-stop-text turn)))
      (unwind-protect
          (progn
            ;; stop_text rendered (no dedup since texts differ)
            (should (test-eot--buffer-has-face-p buf 'claude-gravity-assistant-text))
            (should (string-match-p "different conclusion"
                      (test-eot--buffer-text-with-face buf 'claude-gravity-assistant-text))))
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
  ;; No paragraph boundary — NOT subsumes
  (should-not (claude-gravity--text-subsumes-p "AB" "A"))
  ;; Empty string
  (should-not (claude-gravity--text-subsumes-p "" "A"))
  ;; Nil
  (should-not (claude-gravity--text-subsumes-p nil "A"))
  (should-not (claude-gravity--text-subsumes-p "A" nil)))



;; ===========================================================================
;; Group 4: Integration Tests — Full Session Buffer Rendering
;; ===========================================================================

;; These tests feed events through the handler, build up model state,
;; then render via `claude-gravity-insert-turns` — the same function used
;; by the real session buffer — and verify the full rendered output.

(defun test-eot--render-full-turns (session)
  "Render turns section for SESSION into a temp buffer via `claude-gravity-insert-turns'.
Returns the buffer (caller should kill it)."
  (let ((buf (generate-new-buffer " *test-eot-full*")))
    (with-current-buffer buf
      (cl-letf (((symbol-function 'window-width) (lambda (&rest _) 80)))
        (magit-insert-section (root)
          (claude-gravity-insert-turns session))))
    buf))

(defun test-eot--buffer-content (buf)
  "Return full text content of BUF."
  (with-current-buffer buf
    (buffer-substring-no-properties (point-min) (point-max))))

(defun test-eot--text-appears-before-p (buf text-a text-b)
  "Return non-nil if TEXT-A appears before TEXT-B in BUF."
  (let ((content (test-eot--buffer-content buf)))
    (let ((pos-a (string-search text-a content))
          (pos-b (string-search text-b content)))
      (and pos-a pos-b (< pos-a pos-b)))))


(ert-deftest test-eot/integration-simple-turn ()
  "Full rendering: tool → stop_text appears after tool in rendered buffer."
  (let* ((sid (test-eot--unique-id)))
    (unwind-protect
        (progn
          (test-eot--feed-event "UserPromptSubmit" sid '((prompt . "Fix it")))
          (test-eot--feed-event "PreToolUse" sid
            `((tool_use_id . "it1") (tool_name . "Read")
              (tool_input . ((file_path . "/src/main.ts")))))
          (test-eot--feed-event "PostToolUse" sid
            `((tool_use_id . "it1") (tool_name . "Read")
              (tool_response . "file contents")))
          (test-eot--feed-event "Stop" sid
            `((stop_text . "All fixed and working.")))
          (let* ((session (test-eot--get-session sid))
                 (buf (test-eot--render-full-turns session)))
            (unwind-protect
                (progn
                  ;; stop_text rendered in buffer
                  (should (string-match-p "All fixed and working"
                            (test-eot--buffer-content buf)))
                  ;; stop_text has correct face
                  (should (test-eot--buffer-has-face-p buf 'claude-gravity-assistant-text))
                  ;; Tool appears before stop_text
                  (should (test-eot--text-appears-before-p buf "Read" "All fixed")))
              (kill-buffer buf))))
      (test-eot--cleanup-session sid))))


(ert-deftest test-eot/integration-stop-thinking-and-text ()
  "Full rendering: both stop_thinking and stop_text rendered in order."
  (let* ((sid (test-eot--unique-id)))
    (unwind-protect
        (progn
          (test-eot--feed-event "UserPromptSubmit" sid '((prompt . "Analyze")))
          (test-eot--feed-event "PreToolUse" sid
            `((tool_use_id . "it2") (tool_name . "Glob")
              (tool_input . ((pattern . "**/*.ts")))))
          (test-eot--feed-event "PostToolUse" sid
            `((tool_use_id . "it2") (tool_name . "Glob")
              (tool_response . "found files")))
          (test-eot--feed-event "Stop" sid
            `((stop_text . "Analysis complete.")
              (stop_thinking . "Let me think about this.")))
          (let* ((session (test-eot--get-session sid))
                 (buf (test-eot--render-full-turns session)))
            (unwind-protect
                (progn
                  ;; Both rendered
                  (should (string-match-p "Analysis complete" (test-eot--buffer-content buf)))
                  (should (string-match-p "Let me think" (test-eot--buffer-content buf)))
                  ;; Correct faces
                  (should (test-eot--buffer-has-face-p buf 'claude-gravity-assistant-text))
                  (should (test-eot--buffer-has-face-p buf 'claude-gravity-thinking))
                  ;; Thinking appears before text
                  (should (test-eot--text-appears-before-p buf "Let me think" "Analysis complete")))
              (kill-buffer buf))))
      (test-eot--cleanup-session sid))))


(ert-deftest test-eot/integration-agent-completion-in-turn ()
  "Full rendering: agent stop_text appears between tools and stop_text."
  (let* ((sid (test-eot--unique-id)))
    (unwind-protect
        (progn
          (test-eot--feed-event "UserPromptSubmit" sid '((prompt . "Research")))
          ;; Launch agent via Task tool
          (test-eot--feed-event "PreToolUse" sid
            `((tool_use_id . "it3") (tool_name . "Task")
              (tool_input . ((description . "Search codebase")
                             (prompt . "find patterns")))))
          (test-eot--feed-event "SubagentStart" sid
            `((agent_id . "ag_int_1") (agent_type . "explore")
              (tool_use_id . "it3")))
          (test-eot--feed-event "SubagentStop" sid
            `((agent_id . "ag_int_1")
              (agent_stop_text . "Found 15 matches in 3 files.")))
          (test-eot--feed-event "PostToolUse" sid
            `((tool_use_id . "it3") (tool_name . "Task")
              (tool_response . "agent done")))
          (test-eot--feed-event "Stop" sid
            `((stop_text . "Research is complete.")))
          (let* ((session (test-eot--get-session sid))
                 (buf (test-eot--render-full-turns session)))
            (unwind-protect
                (let ((content (test-eot--buffer-content buf)))
                  ;; Agent completion text rendered
                  (should (string-match-p "15 matches" content))
                  ;; Agent label present
                  (should (string-match-p "explore.*completed" content))
                  ;; Stop text rendered
                  (should (string-match-p "Research is complete" content))
                  ;; Agent completion rendered with correct face
                  (should (string-match-p "15 matches"
                            (test-eot--buffer-text-with-face buf 'claude-gravity-agent-stop-text)))
                  ;; Agent completion before stop_text
                  (should (test-eot--text-appears-before-p buf "15 matches" "Research is complete")))
              (kill-buffer buf))))
      (test-eot--cleanup-session sid))))


(ert-deftest test-eot/integration-dedup-in-context ()
  "Full rendering: dedup works when post_text is rendered by tool, stop_text by insert-stop-text."
  (let* ((sid (test-eot--unique-id)))
    (unwind-protect
        (progn
          (test-eot--feed-event "UserPromptSubmit" sid '((prompt . "Check")))
          (test-eot--feed-event "PreToolUse" sid
            `((tool_use_id . "it4") (tool_name . "Read")
              (tool_input . ((file_path . "/src/config.ts")))))
          (test-eot--feed-event "PostToolUse" sid
            `((tool_use_id . "it4") (tool_name . "Read")
              (tool_response . "config data")
              (post_tool_text . "The config looks correct.")))
          (test-eot--feed-event "Stop" sid
            `((stop_text . "The config looks correct.")))
          (let* ((session (test-eot--get-session sid))
                 (buf (test-eot--render-full-turns session)))
            (unwind-protect
                (progn
                  ;; Text should appear exactly once (dedup suppresses stop_text)
                  (should (= 1 (test-eot--count-occurrences buf "config looks correct"))))
              (kill-buffer buf))))
      (test-eot--cleanup-session sid))))


(ert-deftest test-eot/integration-dedup-stop-longer ()
  "Full rendering: when stop_text extends post_text, dedup clears post_text for next render.
On first render, both post_text and stop_text may appear since insert-turn-children
renders post_text before insert-stop-text runs the dedup.  After dedup, a second
render should show only stop_text."
  (let* ((sid (test-eot--unique-id)))
    (unwind-protect
        (progn
          (test-eot--feed-event "UserPromptSubmit" sid '((prompt . "Fix")))
          (test-eot--feed-event "PreToolUse" sid
            `((tool_use_id . "it5") (tool_name . "Edit")
              (tool_input . ((file_path . "/src/main.ts")
                             (old_string . "foo")
                             (new_string . "bar")))))
          (test-eot--feed-event "PostToolUse" sid
            `((tool_use_id . "it5") (tool_name . "Edit")
              (tool_response . "edited")
              (post_tool_text . "Fixed the issue")))
          (test-eot--feed-event "Stop" sid
            `((stop_text . "Fixed the issue\n\nAlso verified the tests pass.")))
          (let* ((session (test-eot--get-session sid)))
            ;; First render: triggers dedup mutation (clears post_text on tool)
            (let ((buf1 (test-eot--render-full-turns session)))
              (unwind-protect
                  (let ((content (test-eot--buffer-content buf1)))
                    ;; The extended stop_text is always present
                    (should (string-match-p "verified the tests" content)))
                (kill-buffer buf1)))
            ;; Second render: post_text was cleared by dedup, only stop_text remains
            (let ((buf2 (test-eot--render-full-turns session)))
              (unwind-protect
                  (progn
                    (should (= 1 (test-eot--count-occurrences buf2 "Fixed the issue")))
                    (should (string-match-p "verified the tests"
                              (test-eot--buffer-content buf2))))
                (kill-buffer buf2)))))
      (test-eot--cleanup-session sid))))


(ert-deftest test-eot/integration-multi-turn ()
  "Full rendering: 3 turns each with distinct stop_text, all visible."
  (let* ((sid (test-eot--unique-id)))
    (unwind-protect
        (progn
          ;; Turn 1
          (test-eot--feed-event "UserPromptSubmit" sid '((prompt . "Step one")))
          (test-eot--feed-event "PreToolUse" sid
            `((tool_use_id . "imt1") (tool_name . "Read")
              (tool_input . ((file_path . "/a.ts")))))
          (test-eot--feed-event "PostToolUse" sid
            `((tool_use_id . "imt1") (tool_name . "Read")
              (tool_response . "a")))
          (test-eot--feed-event "Stop" sid
            `((stop_text . "Turn one complete.")))
          ;; Turn 2
          (test-eot--feed-event "UserPromptSubmit" sid '((prompt . "Step two")))
          (test-eot--feed-event "PreToolUse" sid
            `((tool_use_id . "imt2") (tool_name . "Read")
              (tool_input . ((file_path . "/b.ts")))))
          (test-eot--feed-event "PostToolUse" sid
            `((tool_use_id . "imt2") (tool_name . "Read")
              (tool_response . "b")))
          (test-eot--feed-event "Stop" sid
            `((stop_text . "Turn two complete.")))
          ;; Turn 3
          (test-eot--feed-event "UserPromptSubmit" sid '((prompt . "Step three")))
          (test-eot--feed-event "PreToolUse" sid
            `((tool_use_id . "imt3") (tool_name . "Read")
              (tool_input . ((file_path . "/c.ts")))))
          (test-eot--feed-event "PostToolUse" sid
            `((tool_use_id . "imt3") (tool_name . "Read")
              (tool_response . "c")))
          (test-eot--feed-event "Stop" sid
            `((stop_text . "Turn three complete.")))
          (let* ((session (test-eot--get-session sid))
                 (buf (test-eot--render-full-turns session)))
            (unwind-protect
                (let ((content (test-eot--buffer-content buf)))
                  ;; All 3 stop texts present
                  (should (string-match-p "Turn one complete" content))
                  (should (string-match-p "Turn two complete" content))
                  (should (string-match-p "Turn three complete" content))
                  ;; Correct order
                  (should (test-eot--text-appears-before-p buf "Turn one" "Turn two"))
                  (should (test-eot--text-appears-before-p buf "Turn two" "Turn three"))
                  ;; Each has the face
                  (let ((face-text (test-eot--buffer-text-with-face buf 'claude-gravity-assistant-text)))
                    (should (string-match-p "Turn one complete" face-text))
                    (should (string-match-p "Turn three complete" face-text))))
              (kill-buffer buf))))
      (test-eot--cleanup-session sid))))


(ert-deftest test-eot/integration-post-tool-text ()
  "Full rendering: post_tool_text rendered on tool, distinct from stop_text."
  (let* ((sid (test-eot--unique-id)))
    (unwind-protect
        (progn
          (test-eot--feed-event "UserPromptSubmit" sid '((prompt . "Build")))
          (test-eot--feed-event "PreToolUse" sid
            `((tool_use_id . "ipt1") (tool_name . "Bash")
              (tool_input . ((command . "npm run build")))))
          (test-eot--feed-event "PostToolUse" sid
            `((tool_use_id . "ipt1") (tool_name . "Bash")
              (tool_response . "build OK")
              (post_tool_text . "Build succeeded on first try.")))
          (test-eot--feed-event "Stop" sid
            `((stop_text . "All tasks complete.")))
          (let* ((session (test-eot--get-session sid))
                 (buf (test-eot--render-full-turns session)))
            (unwind-protect
                (let ((content (test-eot--buffer-content buf)))
                  ;; Both texts present (no overlap, no dedup)
                  (should (string-match-p "Build succeeded" content))
                  (should (string-match-p "All tasks complete" content))
                  ;; post_tool_text appears before stop_text
                  (should (test-eot--text-appears-before-p buf "Build succeeded" "All tasks complete"))
                  ;; Both have assistant-text face
                  (let ((face-text (test-eot--buffer-text-with-face buf 'claude-gravity-assistant-text)))
                    (should (string-match-p "Build succeeded" face-text))
                    (should (string-match-p "All tasks complete" face-text))))
              (kill-buffer buf))))
      (test-eot--cleanup-session sid))))


(ert-deftest test-eot/integration-turn-0-stop-text ()
  "Full rendering: pre-prompt turn 0 with stop_text renders correctly."
  (let* ((sid (test-eot--unique-id)))
    (unwind-protect
        (progn
          ;; No UserPromptSubmit — this is turn 0
          (test-eot--feed-event "PreToolUse" sid
            `((tool_use_id . "it0") (tool_name . "Glob")
              (tool_input . ((pattern . "**/*.el")))))
          (test-eot--feed-event "PostToolUse" sid
            `((tool_use_id . "it0") (tool_name . "Glob")
              (tool_response . "5 files found")))
          (test-eot--feed-event "Stop" sid
            `((stop_text . "Codebase has 5 elisp files.")))
          (let* ((session (test-eot--get-session sid))
                 (buf (test-eot--render-full-turns session)))
            (unwind-protect
                (let ((content (test-eot--buffer-content buf)))
                  ;; Pre-prompt label visible
                  (should (string-match-p "Pre-prompt" content))
                  ;; Stop text present
                  (should (string-match-p "5 elisp files" content))
                  ;; Correct face
                  (should (string-match-p "5 elisp files"
                            (test-eot--buffer-text-with-face buf 'claude-gravity-assistant-text))))
              (kill-buffer buf))))
      (test-eot--cleanup-session sid))))


(ert-deftest test-eot/integration-no-stop-text ()
  "Full rendering: turn without stop_text has no trailing assistant text."
  (let* ((sid (test-eot--unique-id)))
    (unwind-protect
        (progn
          (test-eot--feed-event "UserPromptSubmit" sid '((prompt . "Do stuff")))
          (test-eot--feed-event "PreToolUse" sid
            `((tool_use_id . "ins1") (tool_name . "Read")
              (tool_input . ((file_path . "/x.ts")))))
          (test-eot--feed-event "PostToolUse" sid
            `((tool_use_id . "ins1") (tool_name . "Read")
              (tool_response . "content")))
          (test-eot--feed-event "Stop" sid '())
          (let* ((session (test-eot--get-session sid))
                 (buf (test-eot--render-full-turns session)))
            (unwind-protect
                (progn
                  ;; Tool rendered
                  (should (string-match-p "Read" (test-eot--buffer-content buf)))
                  ;; No assistant text face (no stop_text, no post_tool_text)
                  (should-not (test-eot--buffer-has-face-p buf 'claude-gravity-assistant-text)))
              (kill-buffer buf))))
      (test-eot--cleanup-session sid))))


(provide 'test-end-of-turn)
;;; test-end-of-turn.el ends here
