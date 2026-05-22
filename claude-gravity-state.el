;;; claude-gravity-state.el --- State management for Claude Gravity  -*- lexical-binding: t; -*-

;;; Code:

(require 'claude-gravity-core)
(require 'claude-gravity-session)

; Forward declarations for functions in modules loaded later
(declare-function claude-gravity--inbox-notify "claude-gravity-client")
(declare-function claude-gravity--update-inbox-indicator "claude-gravity-client")
(declare-function claude-gravity--render-overview "claude-gravity-ui")
(declare-function claude-gravity--render-session-buffer "claude-gravity-ui")
(declare-function claude-gravity--session-buffer-name "claude-gravity-ui")
(declare-function claude-gravity--tool-signature "claude-gravity-diff")
(declare-function claude-gravity-tail "claude-gravity-ui")
(declare-function claude-gravity--plan-review-on-kill "claude-gravity-plan-review")
(declare-function claude-gravity--update-streaming-text-fast "claude-gravity-ui")


;;; Inbox — Async queue for items needing user attention

(defvar claude-gravity--inbox nil
  "List of inbox items needing user attention, newest first.
Each item is an alist with keys: id, type, session-id, project, label,
timestamp, summary, data, socket-proc.")


(defvar claude-gravity--inbox-counter 0
  "Monotonic counter for inbox item IDs.")


(defvar claude-gravity--inbox-action-buffers (make-hash-table :test 'eql)
  "Map from inbox item ID to its open action buffer, if any.
Used by dismiss logic to find and kill the correct action buffer.")


(defvar claude-gravity--turn-auto-approve nil
  "Alist of (SESSION-ID . TURN-NUMBER) pairs with turn-scoped auto-approve.
When a permission request arrives for a session whose current turn matches,
it is automatically approved without user interaction.
Cleared on turn boundaries (Stop, UserPromptSubmit, SessionEnd).")


(defun claude-gravity--inbox-add (type session-id data proc)
  "Add an inbox item of TYPE for SESSION-ID with DATA and socket PROC.
TYPE is a symbol: permission, question, plan-review, or idle.
Returns the new item."
  (cl-incf claude-gravity--inbox-counter)
  (let* ((session (claude-gravity--get-session session-id))
         (project (when session (plist-get session :project)))
         (label (if session
                    (claude-gravity--session-label session)
                  (claude-gravity--session-short-id session-id)))
         (summary (claude-gravity--inbox-summary type data))
         (item `((id . ,claude-gravity--inbox-counter)
                 (type . ,type)
                 (session-id . ,session-id)
                 (project . ,project)
                 (label . ,label)
                 (timestamp . ,(current-time))
                 (summary . ,summary)
                 (data . ,data)
                 (socket-proc . ,proc))))
    (push item claude-gravity--inbox)
    (claude-gravity--inbox-notify item)
    (claude-gravity--schedule-refresh)
    ;; Render session buffer unconditionally (even if buried) so inbox badge
    ;; and item are visible when user switches to the buffer.
    (when session
      (let* ((owned-buf (let ((b (plist-get session :buffer)))
                          (and b (buffer-live-p b) b)))
             (buf (or owned-buf
                      (get-buffer (claude-gravity--session-buffer-name session)))))
        (when buf
          (claude-gravity--render-session-buffer session))))
    item))


(defun claude-gravity--inbox-remove (id)
  "Remove inbox item with ID.  Schedules overview and session refresh."
  (let ((item (claude-gravity--inbox-find id)))
    (setq claude-gravity--inbox
          (cl-remove-if (lambda (it) (eq (alist-get 'id it) id))
                        claude-gravity--inbox))
    (claude-gravity--update-inbox-indicator)
    (claude-gravity--schedule-refresh)
    (when-let ((sid (and item (alist-get 'session-id item))))
      (claude-gravity--schedule-session-refresh sid))))


(defun claude-gravity--inbox-remove-for-session (session-id &optional type)
  "Remove all inbox items for SESSION-ID, optionally filtered by TYPE."
  (setq claude-gravity--inbox
        (cl-remove-if (lambda (item)
                        (and (equal (alist-get 'session-id item) session-id)
                             (or (null type)
                                 (eq (alist-get 'type item) type))))
                      claude-gravity--inbox))
  (claude-gravity--update-inbox-indicator)
  (claude-gravity--schedule-refresh)
  (claude-gravity--schedule-session-refresh session-id))


(defun claude-gravity--inbox-find (id)
  "Return inbox item with ID, or nil."
  (cl-find-if (lambda (item) (eq (alist-get 'id item) id))
              claude-gravity--inbox))


(defun claude-gravity--dismiss-stale-inbox-items (session-id)
  "Dismiss stale bidirectional inbox items for SESSION-ID.
Called on turn/session boundaries (Stop, UserPromptSubmit, SessionEnd).

An inbox item is genuinely stale only when the bridge process has
already exited (socket proc is dead).  This happens when:
  - The user handled the action in the TUI (bridge got a response and exited)
  - The bridge crashed or timed out
  - Claude Code killed the hook process (e.g. Ctrl-C)

If the socket proc is ALIVE, the bridge is still waiting for our
response — do NOT kill it.  Killing live procs causes a race
condition: a late-arriving Stop event can dismiss a plan-review
item that was just added for the NEXT tool call."
  (let ((stale (cl-remove-if-not
                (lambda (item)
                  (and (equal (alist-get 'session-id item) session-id)
                       (memq (alist-get 'type item)
                             '(permission question plan-review))
                       ;; Only stale if proc is dead (bridge already exited).
                       ;; In client/server mode, socket-proc is nil — the server
                       ;; owns the connection, so the item is NOT stale.
                       (let ((proc (alist-get 'socket-proc item)))
                         (and proc (not (process-live-p proc))))))
                claude-gravity--inbox)))
    (when stale
      (dolist (item stale)
        (claude-gravity--dismiss-single-inbox-item item)
        (claude-gravity--log 'debug "Auto-dismissed stale %s for session %s"
                             (alist-get 'type item) session-id))
      ;; Remove dismissed items from inbox
      (dolist (item stale)
        (claude-gravity--inbox-remove (alist-get 'id item))))))


(defun claude-gravity--dismiss-single-inbox-item (item)
  "Dismiss a single inbox ITEM: close socket proc and kill action buffer."
  (let ((proc (alist-get 'socket-proc item))
        (item-id (alist-get 'id item))
        (item-type (alist-get 'type item)))
    ;; Close the bridge socket (it's waiting for a response)
    (when (and proc (process-live-p proc))
      (delete-process proc))
    ;; Kill associated action buffer via hash table lookup
    (pcase item-type
      ((or 'permission 'question)
       (let ((buf (gethash item-id claude-gravity--inbox-action-buffers)))
         (when (and buf (buffer-live-p buf))
           (kill-buffer buf))
         (remhash item-id claude-gravity--inbox-action-buffers)))
      ('plan-review
       (let ((label (alist-get 'label item)))
         (when label
           (let ((buf (get-buffer (format "*Claude Plan Review: %s*" label))))
             (when (and buf (buffer-live-p buf))
               ;; Remove kill-buffer hook to avoid double-send of deny
               (with-current-buffer buf
                 (remove-hook 'kill-buffer-hook
                              #'claude-gravity--plan-review-on-kill t))
               (kill-buffer buf)))))))))


(defun claude-gravity--inbox-summary (type data)
  "Generate summary text for inbox item of TYPE with DATA."
  (pcase type
    ('permission
     (let ((tool-name (alist-get 'tool_name data))
           (tool-input (alist-get 'tool_input data)))
       (if tool-name
           (claude-gravity--tool-signature tool-name tool-input)
         "Permission request")))
    ('question
     (let* ((tool-input (alist-get 'tool_input data))
            (questions (alist-get 'questions tool-input))
            (first-q (and (vectorp questions) (> (length questions) 0)
                          (aref questions 0)))
            (q-text (and first-q (alist-get 'question first-q))))
       (if q-text
           (truncate-string-to-width
            (replace-regexp-in-string "\n" " " q-text) 80)
         "Question from Claude")))
    ('plan-review "Plan ready for review")
    ('idle
     (let* ((turn (alist-get 'turn data))
            (snippet (replace-regexp-in-string "[\n\r\t]+" " "
                       (or (alist-get 'snippet data) "idle"))))
       (format "Turn %s — %s"
               (or turn "?")
               snippet)))
    (_ "Unknown")))


;;; Follow mode

(defvar-local claude-gravity--follow-mode nil
  "When non-nil, auto-tail the buffer after each refresh.")


;;; Refresh timers

(defvar claude-gravity--refresh-timer nil
  "Timer for debounced overview UI refresh.")


(defvar claude-gravity--session-refresh-timers (make-hash-table :test 'equal)
  "Per-session debounce timers.")


(defun claude-gravity--schedule-refresh ()
  "Schedule an overview UI refresh after events settle."
  (when claude-gravity--refresh-timer
    (cancel-timer claude-gravity--refresh-timer))
  (setq claude-gravity--refresh-timer
        (run-with-idle-timer claude-gravity-refresh-interval nil
                             #'claude-gravity--do-refresh)))


(defun claude-gravity--do-refresh ()
  "Perform the actual debounced overview refresh."
  (setq claude-gravity--refresh-timer nil)
  (let ((buf (get-buffer claude-gravity-buffer-name)))
    (when (and buf (get-buffer-window buf t))
      (claude-gravity--render-overview))))


(defun claude-gravity--schedule-session-refresh (session-id)
  "Schedule a refresh for the session buffer of SESSION-ID.
Also invalidates cached header-line so next redisplay recomputes it."
  (let ((existing (gethash session-id claude-gravity--session-refresh-timers))
        (session (gethash session-id claude-gravity--sessions)))
    (when existing (cancel-timer existing))
    ;; Cancel any pending streaming timer to avoid double-rendering
    (let ((streaming-timer (gethash session-id claude-gravity--streaming-refresh-timers)))
      (when streaming-timer
        (cancel-timer streaming-timer)
        (remhash session-id claude-gravity--streaming-refresh-timers)))
    ;; Invalidate header-line cache so next redisplay picks up new state
    (when session (plist-put session :header-line-cache nil))
    (puthash session-id
             (run-with-idle-timer claude-gravity-refresh-interval nil
                                  #'claude-gravity--do-session-refresh session-id)
             claude-gravity--session-refresh-timers)))


;;; Streaming-text fast-path refresh timers

(defvar claude-gravity--streaming-refresh-timers (make-hash-table :test 'equal)
  "Per-session fast debounce timers for streaming-text updates.")


(defcustom claude-gravity-streaming-refresh-interval 0.08
  "Debounce interval for streaming-text updates (seconds).
Faster than `claude-gravity-refresh-interval' since streaming updates
only touch a small section of the buffer."
  :type 'number
  :group 'claude-gravity)


(defun claude-gravity--schedule-streaming-refresh (session-id)
  "Schedule a fast streaming-text-only refresh for SESSION-ID."
  (let ((existing (gethash session-id claude-gravity--streaming-refresh-timers)))
    (when existing (cancel-timer existing))
    (puthash session-id
             (run-with-idle-timer claude-gravity-streaming-refresh-interval nil
                                  #'claude-gravity--do-streaming-refresh session-id)
             claude-gravity--streaming-refresh-timers)))


(defun claude-gravity--do-streaming-refresh (session-id)
  "Fast refresh: only update the streaming-text section for SESSION-ID."
  (remhash session-id claude-gravity--streaming-refresh-timers)
  (let ((session (claude-gravity--get-session session-id)))
    (when session
      (claude-gravity--update-streaming-text-fast session))))


(defun claude-gravity--do-session-refresh (session-id)
  "Refresh the buffer for SESSION-ID if it exists and is visible."
  (remhash session-id claude-gravity--session-refresh-timers)
  (let* ((session (claude-gravity--get-session session-id))
         (owned-buf (when session
                      (let ((b (plist-get session :buffer)))
                        (and b (buffer-live-p b) b))))
         (buf (or owned-buf
                  (when session
                    (get-buffer (claude-gravity--session-buffer-name session))))))
    (when buf
      ;; Adopt orphan buffer: found by name but not yet owned by this session.
      ;; Happens after /clear when a new session reuses the same slug.
      (unless owned-buf
        (plist-put session :buffer buf)
        (with-current-buffer buf
          (setq claude-gravity--buffer-session-id session-id)))
      ;; Only render if the buffer is visible in some window
      (when (get-buffer-window buf t)
        (claude-gravity--render-session-buffer session)
        (when (buffer-local-value 'claude-gravity--follow-mode buf)
          (with-current-buffer buf
            (claude-gravity-tail)))))))


;;; Turn tree structure
;;
;; The turn tree mirrors the screen: Session → Turns → Steps → Tools.
;; Patch application (in client.el) writes to the tree; the renderer iterates it.

(defun claude-gravity--make-turn-node (turn-number)
  "Create a new turn node alist for TURN-NUMBER."
  (list (cons 'turn-number turn-number)
        (cons 'prompt nil)
        (cons 'steps (claude-gravity--tlist-new))
        (cons 'agents (claude-gravity--tlist-new))
        (cons 'tasks nil)
        ;; `edited-files' is a plain list of file-diff alists, upserted by
        ;; `update_turn_file' patches.  Pre-allocated to nil so later
        ;; `setf (alist-get 'edited-files ...)' mutates in place (the
        ;; documented setf-on-a-new-key pitfall — see CLAUDE.md / MEMORY).
        (cons 'edited-files nil)
        (cons 'tool-count 0)
        (cons 'agent-count 0)
        (cons 'frozen nil)
        (cons 'stop_text nil)
        (cons 'stop_thinking nil)
        (cons 'token-in nil)
        (cons 'token-out nil)))

(defun claude-gravity--make-step-node (&optional thinking text)
  "Create a new step node alist with optional THINKING and TEXT."
  (list (cons 'thinking thinking)
        (cons 'text text)
        (cons 'tools (claude-gravity--tlist-new))))

(defun claude-gravity--ensure-step (turn-node &optional thinking text)
  "Return current step in TURN-NODE, creating one if empty.
If THINKING or TEXT differ from current step, start a new step."
  (let* ((steps-tl (alist-get 'steps turn-node))
         (current (claude-gravity--tlist-last-item steps-tl)))
    (if (and current
             (not (and thinking
                       (not (equal thinking (alist-get 'thinking current)))))
             (not (and text
                       (not (string-empty-p text))
                       (alist-get 'text current)
                       (not (string-empty-p (alist-get 'text current)))
                       (not (claude-gravity--text-subsumes-p text (alist-get 'text current))))))
        current
      ;; Create new step
      (let ((new-step (claude-gravity--make-step-node thinking text)))
        (claude-gravity--tlist-append steps-tl new-step)
        new-step))))

(defun claude-gravity--current-turn-node (session)
  "Return the current (last) turn node from SESSION's :turns tlist."
  (claude-gravity--tlist-last-item (plist-get session :turns)))

(defun claude-gravity--get-turn-node (session turn-number)
  "Get turn node for TURN-NUMBER from SESSION, using the turn index."
  (let ((index (plist-get session :turn-index)))
    (when index
      (gethash turn-number index))))

(defun claude-gravity--index-turn (session turn-node)
  "Add TURN-NODE to SESSION's turn index."
  (let ((index (plist-get session :turn-index)))
    (unless index
      (setq index (make-hash-table :test 'eql))
      (plist-put session :turn-index index))
    (puthash (alist-get 'turn-number turn-node) turn-node index)))

(defun claude-gravity--link-agent-to-task-tool (session turn-node agent)
  "Link AGENT to its spawning Task tool in TURN-NODE.
Scans recent tools in the turn for an unlinked Task tool matching agent type."
  (let* ((atype (alist-get 'type agent))
         (steps (claude-gravity--tlist-items (alist-get 'steps turn-node)))
         (found nil))
    (when atype
      ;; Scan steps in reverse (most recent first) for matching unlinked Task tool
      (dolist (step (reverse steps))
        (unless found
          (dolist (tool (reverse (claude-gravity--tlist-items (alist-get 'tools step))))
            (when (and (not found)
                       (equal (alist-get 'name tool) "Task")
                       (equal (alist-get 'subagent_type (alist-get 'input tool)) atype)
                       (not (alist-get 'agent tool)))
              ;; Bidirectional link — use nconc since 'agent key doesn't exist yet
              (nconc tool (list (cons 'agent agent)))
              (setf (alist-get 'task-tool agent) tool)
              (setq found t))))))))



;;; Agent helpers

(defun claude-gravity--find-agent (session agent-id)
  "Find and return agent alist for AGENT-ID in SESSION, or nil.
Uses :agent-index hash table for O(1) lookup."
  (let ((idx (plist-get session :agent-index)))
    (when (hash-table-p idx)
      (gethash agent-id idx))))


(defun claude-gravity--tree-total-tool-count (session)
  "Return total tool count across all turns in SESSION.
Uses the cached :total-tool-count when available."
  (or (plist-get session :total-tool-count)
      (let ((total 0))
        (dolist (turn-node (claude-gravity--tlist-items (plist-get session :turns)))
          (cl-incf total (or (alist-get 'tool-count turn-node) 0)))
        (plist-put session :total-tool-count total)
        total)))


;;; Model mutation API — functions used by tmux.el, actions.el, and other live code

(defun claude-gravity-model-session-end (session)
  "Mark SESSION as ended."
  (plist-put session :status 'ended)
  (plist-put session :claude-status 'idle))


(defun claude-gravity-model-set-claude-status (session status)
  "Set SESSION's :claude-status to STATUS (idle or responding)."
  (plist-put session :claude-status status))


(defun claude-gravity-model-set-permission-mode (session mode)
  "Set SESSION's :permission-mode to MODE."
  (plist-put session :permission-mode mode))


(defun claude-gravity-model-add-prompt (session entry)
  "Append prompt ENTRY to SESSION and increment :current-turn.
Creates a new turn node in the :turns tree."
  (let ((new-turn (1+ (or (plist-get session :current-turn) 0))))
    (plist-put session :current-turn new-turn)
    ;; Freeze previous turn, create new turn node
    (let ((prev-turn (claude-gravity--current-turn-node session)))
      (when prev-turn
        (setf (alist-get 'frozen prev-turn) t)))
    (let ((turn-node (claude-gravity--make-turn-node new-turn)))
      (setf (alist-get 'prompt turn-node) entry)
      (claude-gravity--tlist-append (plist-get session :turns) turn-node)
      (claude-gravity--index-turn session turn-node))))


(defun claude-gravity-model-update-prompt-answer (session tool-use-id answer)
  "Update the question prompt matching TOOL-USE-ID in SESSION with ANSWER."
  (when tool-use-id
    (dolist (turn-node (claude-gravity--tlist-items (plist-get session :turns)))
      (let ((p (alist-get 'prompt turn-node)))
        (when (and p
                   (equal (alist-get 'type p) 'question)
                   (equal (alist-get 'tool_use_id p) tool-use-id))
          (setf (alist-get 'answer p) answer)
          (setf (alist-get 'elapsed p)
                (float-time (time-subtract (current-time)
                                           (alist-get 'submitted p)))))))))


(defun claude-gravity-model-file-edit-tools (session file-path)
  "Collect all Edit and Write tools for FILE-PATH from SESSION's turn tree.
Returns a list of (TURN-NUMBER . TOOL) pairs sorted by turn order."
  (let ((result nil))
    (dolist (turn-node (claude-gravity--tlist-items (plist-get session :turns)))
      (let ((turn-num (alist-get 'number turn-node)))
        (cl-labels
            ((collect-from-steps (steps-tl)
               (dolist (step (when steps-tl (claude-gravity--tlist-items steps-tl)))
                 (dolist (tool (claude-gravity--tlist-items (alist-get 'tools step)))
                   (when (and (member (alist-get 'name tool) '("Edit" "Write"))
                              (equal (alist-get 'file_path (alist-get 'input tool))
                                     file-path))
                     (push (cons turn-num tool) result))
                   ;; Check agent sub-tools
                   (let ((agent (alist-get 'agent tool)))
                     (when agent
                       (collect-from-steps (alist-get 'steps agent))))))))
          (collect-from-steps (alist-get 'steps turn-node)))))
    (nreverse result)))

(defun claude-gravity-model-find-tool (session tool-use-id)
  "Find and return tool alist in SESSION matching TOOL-USE-ID, or nil.
Uses the :tool-index hash table for O(1) lookup."
  (gethash tool-use-id (plist-get session :tool-index)))


(defun claude-gravity-model-toggle-ignored (session)
  "Toggle SESSION's :ignored flag and persist the ignore list to disk."
  (let ((new-val (not (plist-get session :ignored))))
    (plist-put session :ignored new-val)
    (claude-gravity--write-ignored-sessions (plist-get session :cwd))
    (claude-gravity--log 'debug "Session %s %s"
                         (plist-get session :session-id)
                         (if new-val "ignored" "un-ignored"))))


(defun claude-gravity--write-ignored-sessions (cwd)
  "Write the list of ignored session IDs to CWD/.claude/gravity-ignored-sessions.json.
Scans all sessions matching CWD and collects those with :ignored t."
  (when (and cwd (not (string-empty-p cwd)))
    (let ((ignored-ids nil))
      (maphash (lambda (_id session)
                 (when (and (plist-get session :ignored)
                            (equal (plist-get session :cwd) cwd))
                   (push (plist-get session :session-id) ignored-ids)))
               claude-gravity--sessions)
      (let* ((dir (expand-file-name ".claude" cwd))
             (file (expand-file-name "gravity-ignored-sessions.json" dir)))
        (if ignored-ids
            (progn
              (unless (file-exists-p dir) (make-directory dir t))
              (with-temp-file file
                (insert (json-encode (vconcat ignored-ids)))))
          ;; No ignored sessions — remove the file if it exists
          (when (file-exists-p file)
            (delete-file file)))))))


(provide 'claude-gravity-state)
;;; claude-gravity-state.el ends here