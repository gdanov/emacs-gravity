;;; claude-gravity-state.el --- State management for Claude Gravity  -*- lexical-binding: t; -*-

;;; Code:

(require 'claude-gravity-core)
(require 'claude-gravity-session)

; Forward declarations for functions in modules loaded later
(declare-function claude-gravity--inbox-notify "claude-gravity-socket")
(declare-function claude-gravity--update-inbox-indicator "claude-gravity-socket")
(declare-function claude-gravity--render-overview "claude-gravity-ui")
(declare-function claude-gravity--render-session-buffer "claude-gravity-ui")
(declare-function claude-gravity--session-buffer-name "claude-gravity-ui")
(declare-function claude-gravity--tool-signature "claude-gravity-diff")
(declare-function claude-gravity-tail "claude-gravity-ui")


;;; Inbox — Async queue for items needing user attention

(defvar claude-gravity--inbox nil
  "List of inbox items needing user attention, newest first.
Each item is an alist with keys: id, type, session-id, project, label,
timestamp, summary, data, socket-proc.")


(defvar claude-gravity--inbox-counter 0
  "Monotonic counter for inbox item IDs.")


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
    item))


(defun claude-gravity--inbox-remove (id)
  "Remove inbox item with ID.  Schedules overview refresh."
  (setq claude-gravity--inbox
        (cl-remove-if (lambda (item) (eq (alist-get 'id item) id))
                      claude-gravity--inbox))
  (claude-gravity--update-inbox-indicator)
  (claude-gravity--schedule-refresh))


(defun claude-gravity--inbox-remove-for-session (session-id &optional type)
  "Remove all inbox items for SESSION-ID, optionally filtered by TYPE."
  (setq claude-gravity--inbox
        (cl-remove-if (lambda (item)
                        (and (equal (alist-get 'session-id item) session-id)
                             (or (null type)
                                 (eq (alist-get 'type item) type))))
                      claude-gravity--inbox))
  (claude-gravity--update-inbox-indicator)
  (claude-gravity--schedule-refresh))


(defun claude-gravity--inbox-find (id)
  "Return inbox item with ID, or nil."
  (cl-find-if (lambda (item) (eq (alist-get 'id item) id))
              claude-gravity--inbox))


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
        (run-with-idle-timer 0.1 nil #'claude-gravity--do-refresh)))


(defun claude-gravity--do-refresh ()
  "Perform the actual debounced overview refresh."
  (setq claude-gravity--refresh-timer nil)
  (when (get-buffer claude-gravity-buffer-name)
    (claude-gravity--render-overview)))


(defun claude-gravity--schedule-session-refresh (session-id)
  "Schedule a refresh for the session buffer of SESSION-ID."
  (let ((existing (gethash session-id claude-gravity--session-refresh-timers)))
    (when existing (cancel-timer existing))
    (puthash session-id
             (run-with-idle-timer 0.1 nil
                                  #'claude-gravity--do-session-refresh session-id)
             claude-gravity--session-refresh-timers)))


(defun claude-gravity--do-session-refresh (session-id)
  "Refresh the buffer for SESSION-ID if it exists."
  (remhash session-id claude-gravity--session-refresh-timers)
  (let* ((session (claude-gravity--get-session session-id))
         (buf (when session
                (or (let ((b (plist-get session :buffer)))
                      (and b (buffer-live-p b) b))
                    (get-buffer (claude-gravity--session-buffer-name session))))))
    (when buf
      (claude-gravity--render-session-buffer session)
      (when (buffer-local-value 'claude-gravity--follow-mode buf)
        (with-current-buffer buf
          (claude-gravity-tail))))))


;;; Tool helpers

;;; File tracking

(defun claude-gravity--track-file (session tool-name tool-input)
  "Track file from TOOL-NAME with TOOL-INPUT in SESSION's :files hash table."
  (let ((files (plist-get session :files))
        (path nil)
        (op nil))
    (pcase tool-name
      ("Read"  (setq path (alist-get 'file_path tool-input) op "read"))
      ("Edit"  (setq path (alist-get 'file_path tool-input) op "edit"))
      ("Write" (setq path (alist-get 'file_path tool-input) op "write")))
    (when (and path op files)
      (let ((entry (gethash path files)))
        (if entry
            (progn
              (unless (member op (alist-get 'ops entry))
                (setf (alist-get 'ops entry) (cons op (alist-get 'ops entry))))
              (setf (alist-get 'last-touched entry) (current-time)))
          (puthash path (list (cons 'ops (list op))
                              (cons 'last-touched (current-time)))
                   files))))))


;;; Task tracking

(defun claude-gravity--track-task (session event tool-name tool-input tool-use-id &optional tool-response)
  "Track task operations in SESSION from EVENT with TOOL-NAME, TOOL-INPUT, TOOL-USE-ID and TOOL-RESPONSE."
  (let ((tasks (or (plist-get session :tasks)
                   (let ((ht (make-hash-table :test 'equal)))
                     (plist-put session :tasks ht)
                     ht))))
    (pcase event
      ("PreToolUse"
       (pcase tool-name
         ("TaskCreate"
          ;; Store with tool_use_id as temp key; we'll re-key on PostToolUse
          (puthash (concat "_pending_" tool-use-id)
                   (list (cons 'subject (alist-get 'subject tool-input))
                         (cons 'description (alist-get 'description tool-input))
                         (cons 'activeForm (alist-get 'activeForm tool-input))
                         (cons 'status "pending")
                         (cons 'turn (or (plist-get session :current-turn) 0)))
                   tasks))
         ("TaskUpdate"
          (let* ((task-id (alist-get 'taskId tool-input))
                 (entry (gethash task-id tasks)))
            (when entry
              (let ((new-status (alist-get 'status tool-input))
                    (new-subject (alist-get 'subject tool-input))
                    (new-desc (alist-get 'description tool-input))
                    (new-active (alist-get 'activeForm tool-input)))
                (when new-status (setf (alist-get 'status entry) new-status))
                (when new-subject (setf (alist-get 'subject entry) new-subject))
                (when new-desc (setf (alist-get 'description entry) new-desc))
                (when new-active (setf (alist-get 'activeForm entry) new-active))
                (puthash task-id entry tasks)))))))
      ("PostToolUse"
       (pcase tool-name
         ("TaskCreate"
          ;; Re-key from temp ID to real taskId
          (let* ((temp-key (concat "_pending_" tool-use-id))
                 (entry (gethash temp-key tasks))
                 (task-data (alist-get 'task tool-response))
                 (task-id (or (alist-get 'taskId tool-response)
                              (alist-get 'id task-data))))
            (when (and entry task-id)
              (setf (alist-get 'taskId entry) task-id)
              (puthash task-id entry tasks)
              (remhash temp-key tasks))))
         ("TaskList"
          ;; Reconcile with authoritative list
          (let ((task-list (alist-get 'tasks tool-response)))
            (when (and task-list (listp task-list))
              (dolist (task task-list)
                (let* ((task-id (alist-get 'id task))
                       (existing (gethash task-id tasks))
                       (new-entry (list (cons 'taskId task-id)
                                        (cons 'subject (alist-get 'subject task))
                                        (cons 'description (alist-get 'description task))
                                        (cons 'status (alist-get 'status task))
                                        (cons 'activeForm (or (alist-get 'activeForm task)
                                                              (and existing (alist-get 'activeForm existing))))
                                        (cons 'turn (or (and existing (alist-get 'turn existing)) 0)))))
                  (puthash task-id new-entry tasks)))))))))))


;;; Agent helpers

(defun claude-gravity--find-agent-by-id (agents agent-id)
  "Find index of agent in AGENTS list matching AGENT-ID.
For vector AGENTS (legacy), searches with aref; for lists, uses cl-position."
  (cond
   ((vectorp agents)
    (let ((idx nil))
      (dotimes (i (length agents))
        (when (equal (alist-get 'agent_id (aref agents i)) agent-id)
          (setq idx i)))
      idx))
   ((listp agents)
    (cl-position agent-id agents :test #'equal :key (lambda (a) (alist-get 'agent_id a))))
   (t nil)))


(defun claude-gravity--find-agent (session agent-id)
  "Find and return agent alist for AGENT-ID in SESSION, or nil."
  (let ((agents (plist-get session :agents)))
    (cl-find agent-id agents :test #'equal :key (lambda (a) (alist-get 'agent_id a)))))


(defun claude-gravity--agent-tools-list (session agent-id)
  "Return the tools list for AGENT-ID in SESSION.
If AGENT-ID is nil, return the root tools list.
If the agent is not found, return nil."
  (if (null agent-id)
      (alist-get 'tools (plist-get session :state))
    (let ((agent (claude-gravity--find-agent session agent-id)))
      (when agent
        (alist-get 'agent-tools agent)))))


(defun claude-gravity--set-agent-tools-list (session agent-id new-list)
  "Set the tools list for AGENT-ID in SESSION to NEW-LIST.
If AGENT-ID is nil, set root tools."
  (if (null agent-id)
      (setf (alist-get 'tools (plist-get session :state)) new-list)
    (let ((agent (claude-gravity--find-agent session agent-id)))
      (when agent
        (setf (alist-get 'agent-tools agent) new-list)))))


;;; Model mutation API
;;
;; These functions encapsulate all session state mutations.
;; Both the hooks adapter (handle-event) and the future JSON-output
;; adapter call these to update the view model.
;; Model functions do NOT trigger UI refresh — that's the adapter's job.

(defun claude-gravity-model-session-end (session)
  "Mark SESSION as ended."
  (plist-put session :status 'ended))


(defun claude-gravity-model-set-claude-status (session status)
  "Set SESSION's :claude-status to STATUS (idle or responding)."
  (plist-put session :claude-status status))


(defun claude-gravity-model-set-permission-mode (session mode)
  "Set SESSION's :permission-mode to MODE."
  (plist-put session :permission-mode mode))


(defun claude-gravity-model-set-token-usage (session usage)
  "Set SESSION's :token-usage to USAGE alist."
  (when usage
    (plist-put session :token-usage usage)))


(defun claude-gravity-model-set-plan (session plan)
  "Set SESSION's :plan to PLAN plist."
  (plist-put session :plan plan))


(defun claude-gravity-model-add-prompt (session entry)
  "Append prompt ENTRY to SESSION's :prompts and increment :current-turn."
  (plist-put session :prompts
             (nconc (plist-get session :prompts) (list entry)))
  (plist-put session :current-turn
             (1+ (or (plist-get session :current-turn) 0))))


(defun claude-gravity-model-finalize-last-prompt (session &optional stop-text stop-thinking)
  "Compute elapsed time on SESSION's last prompt.
Optionally store STOP-TEXT and STOP-THINKING."
  (let* ((prompts (plist-get session :prompts))
         (last-prompt (car (last prompts))))
    (when last-prompt
      ;; Elapsed: key exists in alist (created as nil), so setf is in-place
      (when (and (listp last-prompt)
                 (not (alist-get 'elapsed last-prompt))
                 (alist-get 'submitted last-prompt))
        (setf (alist-get 'elapsed last-prompt)
              (float-time (time-subtract (current-time)
                                         (alist-get 'submitted last-prompt)))))
      ;; stop_text/stop_thinking: keys pre-initialized as nil in prompt alist,
      ;; so setf/alist-get modifies the existing cons cell in-place.
      (when stop-text
        (setf (alist-get 'stop_text last-prompt) stop-text))
      (when stop-thinking
        (setf (alist-get 'stop_thinking last-prompt) stop-thinking))
      )))


(defun claude-gravity-model-update-prompt-answer (session tool-use-id answer)
  "Update the question prompt matching TOOL-USE-ID in SESSION with ANSWER."
  (let ((prompts (plist-get session :prompts)))
    (when (and prompts tool-use-id)
      (dolist (p prompts)
        (when (and (equal (alist-get 'type p) 'question)
                   (equal (alist-get 'tool_use_id p) tool-use-id))
          (setf (alist-get 'answer p) answer)
          (setf (alist-get 'elapsed p)
                (float-time (time-subtract (current-time)
                                           (alist-get 'submitted p)))))))))


(defun claude-gravity-model-add-tool (session tool agent-id candidate-ids)
  "Add TOOL to SESSION, routing based on AGENT-ID.
AGENT-ID can be a string (definitive agent), \"ambiguous\", or nil (root).
CANDIDATE-IDS is a list of possible agent IDs when ambiguous.
Deduplicates by tool_use_id — skips if a tool with the same ID already exists."
  (let ((tid (alist-get 'tool_use_id tool)))
    (when (or (null tid) (null (claude-gravity-model-find-tool session tid)))
      (cond
       ;; Definitively attributed to an agent
       ((and agent-id (not (equal agent-id "ambiguous")))
        (let ((agent-tools (claude-gravity--agent-tools-list session agent-id)))
          (if agent-tools
              (claude-gravity--set-agent-tools-list
               session agent-id (nconc agent-tools (list tool)))
            ;; Agent not found — fall back to root
            (setf (alist-get 'tools (plist-get session :state))
                  (nconc (alist-get 'tools (plist-get session :state)) (list tool))))))
       ;; Ambiguous — store in root with ambiguous flag
       ((equal agent-id "ambiguous")
        (setf (alist-get 'ambiguous tool) t)
        (when candidate-ids
          (setf (alist-get 'candidate-agents tool)
                (append candidate-ids nil)))
        (setf (alist-get 'tools (plist-get session :state))
              (nconc (alist-get 'tools (plist-get session :state)) (list tool))))
       ;; No agent context — root tool
       (t
        (setf (alist-get 'tools (plist-get session :state))
              (nconc (alist-get 'tools (plist-get session :state)) (list tool)))))
      ;; Register in tool index for O(1) lookup
      (when tid
        (puthash tid tool (plist-get session :tool-index))))))


(defun claude-gravity-model-complete-tool (session tool-use-id agent-id result)
  "Mark tool TOOL-USE-ID as done in SESSION with RESULT.
AGENT-ID is used for routing; handles re-attribution of ambiguous tools."
  (let ((tool (claude-gravity-model-find-tool session tool-use-id)))
    (when tool
      ;; Re-attribute ambiguous root tool to agent if now definitive
      (when (and (alist-get 'ambiguous tool)
                 agent-id (not (equal agent-id "ambiguous")))
        (let ((agent-tools (claude-gravity--agent-tools-list session agent-id)))
          (when agent-tools
            (setf (alist-get 'ambiguous tool) nil)
            ;; Remove from root tools list
            (setf (alist-get 'tools (plist-get session :state))
                  (delq tool (alist-get 'tools (plist-get session :state))))
            ;; Add to agent tools
            (claude-gravity--set-agent-tools-list
             session agent-id (nconc agent-tools (list tool))))))
      ;; Mark as done
      (setf (alist-get 'status tool) "done")
      (setf (alist-get 'result tool) result))))


(defun claude-gravity-model-find-tool (session tool-use-id)
  "Find and return tool alist in SESSION matching TOOL-USE-ID, or nil.
Uses the :tool-index hash table for O(1) lookup."
  (gethash tool-use-id (plist-get session :tool-index)))


(defun claude-gravity-model-add-agent (session agent)
  "Add AGENT alist to SESSION's :agents list if not already present."
  (let* ((new-id (alist-get 'agent_id agent))
         (existing (claude-gravity--find-agent session new-id)))
    (unless existing
      (plist-put session :agents
                 (nconc (plist-get session :agents) (list agent))))))


(defun claude-gravity-model-complete-agent (session agent-id &rest props)
  "Mark agent AGENT-ID as done in SESSION.
PROPS is a plist with optional keys:
  :transcript-path, :stop-text, :stop-thinking."
  (let ((agent (claude-gravity--find-agent session agent-id)))
    (when agent
      ;; Directly modify agent alist fields
      (setf (alist-get 'status agent) "done")
      (let ((ts (alist-get 'timestamp agent)))
        (when ts
          (setf (alist-get 'duration agent)
                (float-time (time-subtract (current-time) ts)))))
      ;; Store stop-text if provided
      (let ((st (plist-get props :stop-text)))
        (when st
          (setf (alist-get 'stop_text agent) st)))
      ;; Store transcript-path if provided
      (let ((tp (plist-get props :transcript-path)))
        (when tp (setf (alist-get 'transcript_path agent) tp)))
      ;; Store stop-thinking if provided
      (let ((sth (plist-get props :stop-thinking)))
        (when sth
          (setf (alist-get 'stop_thinking agent) sth))))))


(defun claude-gravity-model-move-tools-to-agent (session agent-id tool-ids)
  "Move ambiguous root tools matching TOOL-IDS to agent AGENT-ID in SESSION."
  (when (and tool-ids (> (length tool-ids) 0))
    (let ((agent (claude-gravity--find-agent session agent-id)))
      (when agent
        (let* ((root-tools (alist-get 'tools (plist-get session :state)))
               (agent-tools (alist-get 'agent-tools agent))
               (to-move nil)
               (keep nil))
          (dolist (tool root-tools)
            (let ((tid (alist-get 'tool_use_id tool)))
              (if (and (alist-get 'ambiguous tool)
                       tid
                       (seq-contains-p tool-ids tid #'equal))
                  (progn
                    (setf (alist-get 'ambiguous tool) nil)
                    (push tool to-move))
                (push tool keep))))
          (when to-move
            (setf (alist-get 'tools (plist-get session :state))
                  (nreverse keep))
            (setf (alist-get 'agent-tools agent)
                  (nconc (or agent-tools nil) (nreverse to-move)))
            (claude-gravity--log 'debug "Claude Gravity: moved %d tools to agent %s"
                     (length to-move) agent-id)))))))


(defun claude-gravity-model-add-notification (session notif)
  "Append NOTIF alist to SESSION's :notifications list."
  (plist-put session :notifications
             (nconc (plist-get session :notifications) (list notif))))


(defun claude-gravity--session-has-tool-p (session tool-use-id)
  "Return non-nil if TOOL-USE-ID exists anywhere in SESSION's tools."
  (not (null (gethash tool-use-id (plist-get session :tool-index)))))


(defun claude-gravity-model-append-streaming-text (session text)
  "Append TEXT to SESSION's :streaming-text accumulator."
  (let ((current (or (plist-get session :streaming-text) "")))
    (plist-put session :streaming-text (concat current text))))


(defun claude-gravity-model-clear-streaming-text (session)
  "Clear SESSION's :streaming-text accumulator."
  (plist-put session :streaming-text nil))


(defun claude-gravity-model-update-session-meta (session &rest props)
  "Update SESSION metadata from PROPS plist.
Supported keys: :pid, :slug, :last-event-time."
  (plist-put session :last-event-time
             (or (plist-get props :last-event-time) (current-time)))
  (let ((pid (plist-get props :pid)))
    (when (and pid (numberp pid) (> pid 0))
      (plist-put session :pid pid)))
  (let ((slug (plist-get props :slug)))
    (when (and slug (stringp slug) (not (plist-get session :slug)))
      (let ((old-buf (plist-get session :buffer)))
        (plist-put session :slug slug)
        (when (and old-buf (buffer-live-p old-buf))
          (with-current-buffer old-buf
            (rename-buffer (claude-gravity--session-buffer-name session) t)))))))

(provide 'claude-gravity-state)
;;; claude-gravity-state.el ends here