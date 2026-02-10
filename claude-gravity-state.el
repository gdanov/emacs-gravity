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
        (run-with-idle-timer 0.05 nil #'claude-gravity--do-refresh)))


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
             (run-with-idle-timer 0.05 nil
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
          (let ((entry (list (cons 'subject (alist-get 'subject tool-input))
                             (cons 'description (alist-get 'description tool-input))
                             (cons 'activeForm (alist-get 'activeForm tool-input))
                             (cons 'status "pending")
                             (cons 'turn (or (plist-get session :current-turn) 0)))))
            (puthash (concat "_pending_" tool-use-id) entry tasks)
            ;; Tree path: also add task to current turn node
            (claude-gravity--tree-add-task session entry)))
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

(defun claude-gravity--find-agent (session agent-id)
  "Find and return agent alist for AGENT-ID in SESSION, or nil.
Uses :agent-index hash table for O(1) lookup."
  (let ((idx (plist-get session :agent-index)))
    (when (hash-table-p idx)
      (gethash agent-id idx))))


;;; Turn tree structure
;;
;; The turn tree mirrors the screen: Session → Turns → Cycles → Tools.
;; Hooks write directly to the tree; the renderer iterates it.

(defun claude-gravity--make-turn-node (turn-number)
  "Create a new turn node alist for TURN-NUMBER."
  (list (cons 'turn-number turn-number)
        (cons 'prompt nil)
        (cons 'cycles (claude-gravity--tlist-new))
        (cons 'agents (claude-gravity--tlist-new))
        (cons 'tasks nil)
        (cons 'tool-count 0)
        (cons 'agent-count 0)
        (cons 'frozen nil)
        (cons 'stop_text nil)
        (cons 'stop_thinking nil)))

(defun claude-gravity--make-cycle-node (&optional thinking text)
  "Create a new cycle node alist with optional THINKING and TEXT."
  (list (cons 'thinking thinking)
        (cons 'text text)
        (cons 'tools (claude-gravity--tlist-new))))

(defun claude-gravity--current-turn-node (session)
  "Return the current (last) turn node from SESSION's :turns tlist."
  (claude-gravity--tlist-last-item (plist-get session :turns)))

(defun claude-gravity--get-turn-node (session turn-number)
  "Return turn node for TURN-NUMBER from SESSION, or nil."
  (cl-find turn-number (claude-gravity--tlist-items (plist-get session :turns))
           :key (lambda (t-node) (alist-get 'turn-number t-node))))

(defun claude-gravity--current-cycle (turn-node)
  "Return the current (last) cycle from TURN-NODE, or nil."
  (claude-gravity--tlist-last-item (alist-get 'cycles turn-node)))

(defun claude-gravity--ensure-cycle (turn-node &optional thinking text)
  "Return current cycle in TURN-NODE, creating one if empty.
If THINKING or TEXT differ from current cycle, start a new cycle."
  (let* ((cycles-tl (alist-get 'cycles turn-node))
         (current (claude-gravity--tlist-last-item cycles-tl)))
    (if (and current
             (not (and thinking
                       (not (equal thinking (alist-get 'thinking current)))))
             (not (and text
                       (not (string-empty-p text))
                       (alist-get 'text current)
                       (not (string-empty-p (alist-get 'text current)))
                       (not (claude-gravity--text-subsumes-p text (alist-get 'text current))))))
        current
      ;; Create new cycle
      (let ((new-cycle (claude-gravity--make-cycle-node thinking text)))
        (claude-gravity--tlist-append cycles-tl new-cycle)
        new-cycle))))

(defun claude-gravity--tree-add-tool (session tool &optional agent-id)
  "Add TOOL to SESSION's turn tree, routing based on AGENT-ID.
Handles cycle boundary detection at insertion time."
  (let* ((turn-num (or (alist-get 'turn tool) 0))
         (turn-node (or (claude-gravity--get-turn-node session turn-num)
                        (claude-gravity--current-turn-node session)))
         (athink (alist-get 'assistant_thinking tool))
         (atext (alist-get 'assistant_text tool)))
    (when turn-node
      (if (and agent-id (not (equal agent-id "ambiguous")))
          ;; Route to agent's cycles
          (let ((agent (claude-gravity--find-agent session agent-id)))
            (when agent
              (let* ((agent-cycles (alist-get 'cycles agent))
                     (cycle (if (not (claude-gravity--tlist-items agent-cycles))
                                ;; First cycle for this agent
                                (let ((c (claude-gravity--make-cycle-node athink atext)))
                                  (claude-gravity--tlist-append agent-cycles c)
                                  c)
                              (claude-gravity--agent-ensure-cycle agent athink atext))))
                (claude-gravity--tlist-append (alist-get 'tools cycle) tool)
                (cl-incf (alist-get 'tool-count agent)))))
        ;; Route to turn's root cycles (including ambiguous)
        (let* ((cycles-tl (alist-get 'cycles turn-node))
               (current (claude-gravity--tlist-last-item cycles-tl))
               ;; Dedup: if same assistant_text as current cycle, clear it (parallel call)
               (atext (if (and atext current
                               (equal atext (alist-get 'text current)))
                          (progn (setf (alist-get 'assistant_text tool) nil) nil)
                        atext))
               (athink (if (and athink current
                                (equal athink (alist-get 'thinking current)))
                           (progn (setf (alist-get 'assistant_thinking tool) nil) nil)
                         athink))
               ;; Start new cycle if we have new assistant text
               (cycle (if (or (not current)
                              (and atext (not (string-empty-p atext))
                                   (alist-get 'text current)
                                   (not (string-empty-p (alist-get 'text current)))
                                   (not (claude-gravity--text-subsumes-p atext (alist-get 'text current))))
                              (and athink (not (string-empty-p athink))
                                   (alist-get 'thinking current)
                                   (not (string-empty-p (alist-get 'thinking current)))
                                   (not (equal athink (alist-get 'thinking current)))))
                          ;; New cycle
                          (let ((c (claude-gravity--make-cycle-node athink atext)))
                            (claude-gravity--tlist-append cycles-tl c)
                            c)
                        ;; Reuse current cycle, but set text/thinking if not yet set
                        (progn
                          (when (and atext (not (string-empty-p atext))
                                     (or (not (alist-get 'text current))
                                         (string-empty-p (alist-get 'text current))))
                            (setf (alist-get 'text current) atext))
                          (when (and athink (not (string-empty-p athink))
                                     (or (not (alist-get 'thinking current))
                                         (string-empty-p (alist-get 'thinking current))))
                            (setf (alist-get 'thinking current) athink))
                          current))))
          (claude-gravity--tlist-append (alist-get 'tools cycle) tool)
          ;; Only increment turn tool count for root (non-agent) tools
          (cl-incf (alist-get 'tool-count turn-node)))))))

(defun claude-gravity--agent-ensure-cycle (agent &optional thinking text)
  "Ensure AGENT has a current cycle, creating new one if needed."
  (let* ((cycles-tl (alist-get 'cycles agent))
         (current (claude-gravity--tlist-last-item cycles-tl)))
    (if (and current
             (not (and text
                       (not (string-empty-p text))
                       (alist-get 'text current)
                       (not (string-empty-p (alist-get 'text current)))
                       (not (claude-gravity--text-subsumes-p text (alist-get 'text current))))))
        (progn
          ;; Set text/thinking if not yet set
          (when (and text (not (string-empty-p text))
                     (or (not (alist-get 'text current))
                         (string-empty-p (alist-get 'text current))))
            (setf (alist-get 'text current) text))
          (when (and thinking (not (string-empty-p thinking))
                     (or (not (alist-get 'thinking current))
                         (string-empty-p (alist-get 'thinking current))))
            (setf (alist-get 'thinking current) thinking))
          current)
      ;; New cycle
      (let ((c (claude-gravity--make-cycle-node thinking text)))
        (claude-gravity--tlist-append cycles-tl c)
        c))))

(defun claude-gravity--tree-add-agent (session agent)
  "Add AGENT to current turn node's agents tlist in SESSION."
  (let ((turn-node (claude-gravity--current-turn-node session)))
    (when turn-node
      (claude-gravity--tlist-append (alist-get 'agents turn-node) agent)
      (cl-incf (alist-get 'agent-count turn-node))
      ;; Try to link agent to its spawning Task tool
      (claude-gravity--link-agent-to-task-tool session turn-node agent))))

(defun claude-gravity--link-agent-to-task-tool (session turn-node agent)
  "Link AGENT to its spawning Task tool in TURN-NODE.
Scans recent tools in the turn for an unlinked Task tool matching agent type."
  (let* ((atype (alist-get 'type agent))
         (cycles (claude-gravity--tlist-items (alist-get 'cycles turn-node)))
         (found nil))
    (when atype
      ;; Scan cycles in reverse (most recent first) for matching unlinked Task tool
      (dolist (cycle (reverse cycles))
        (unless found
          (dolist (tool (reverse (claude-gravity--tlist-items (alist-get 'tools cycle))))
            (when (and (not found)
                       (equal (alist-get 'name tool) "Task")
                       (equal (alist-get 'subagent_type (alist-get 'input tool)) atype)
                       (not (alist-get 'agent tool)))
              ;; Bidirectional link — use nconc since 'agent key doesn't exist yet
              (nconc tool (list (cons 'agent agent)))
              (setf (alist-get 'task-tool agent) tool)
              (setq found t))))))))

(defun claude-gravity--tree-add-task (session task)
  "Add TASK alist to current turn node's tasks list in SESSION."
  (let ((turn-node (claude-gravity--current-turn-node session)))
    (when turn-node
      (setf (alist-get 'tasks turn-node)
            (nconc (alist-get 'tasks turn-node) (list task))))))

(defun claude-gravity--tree-total-tool-count (session)
  "Return total tool count across all turns in SESSION."
  (let ((total 0))
    (dolist (turn-node (claude-gravity--tlist-items (plist-get session :turns)))
      (cl-incf total (or (alist-get 'tool-count turn-node) 0)))
    total))


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
      (claude-gravity--tlist-append (plist-get session :turns) turn-node))))


(defun claude-gravity-model-finalize-last-prompt (session &optional stop-text stop-thinking)
  "Compute elapsed time on SESSION's last prompt.
Optionally store STOP-TEXT and STOP-THINKING."
  (let* ((last-turn (claude-gravity--current-turn-node session))
         (last-prompt (when last-turn (alist-get 'prompt last-turn))))
    (when last-prompt
      ;; Elapsed: key exists in alist (created as nil), so setf is in-place
      (when (and (listp last-prompt)
                 (not (alist-get 'elapsed last-prompt))
                 (alist-get 'submitted last-prompt))
        (setf (alist-get 'elapsed last-prompt)
              (float-time (time-subtract (current-time)
                                         (alist-get 'submitted last-prompt))))))
    ;; stop_text/stop_thinking stored on the turn node (not prompt) —
    ;; This works even when last-prompt is nil (turn 0 / pre-prompt activity).
    ;; Use assq guard + nconc for old turns that lack pre-allocated keys.
    (when (and last-turn stop-text)
      (unless (assq 'stop_text last-turn)
        (nconc last-turn (list (cons 'stop_text nil))))
      (setf (alist-get 'stop_text last-turn) stop-text))
    (when (and last-turn stop-thinking)
      (unless (assq 'stop_thinking last-turn)
        (nconc last-turn (list (cons 'stop_thinking nil))))
      (setf (alist-get 'stop_thinking last-turn) stop-thinking))))


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


(defun claude-gravity-model-add-tool (session tool agent-id candidate-ids)
  "Add TOOL to SESSION's turn tree, routing based on AGENT-ID.
AGENT-ID can be a string (definitive agent), \"ambiguous\", or nil (root).
CANDIDATE-IDS is a list of possible agent IDs when ambiguous.
Deduplicates by tool_use_id — skips if a tool with the same ID already exists."
  (let ((tid (alist-get 'tool_use_id tool)))
    (when (or (null tid) (null (claude-gravity-model-find-tool session tid)))
      ;; Set ambiguous flag if needed
      (when (equal agent-id "ambiguous")
        (setf (alist-get 'ambiguous tool) t)
        (when candidate-ids
          (setf (alist-get 'candidate-agents tool)
                (append candidate-ids nil))))
      ;; Register in tool index for O(1) lookup
      (when tid
        (puthash tid tool (plist-get session :tool-index)))
      ;; Route to turn tree
      (claude-gravity--tree-add-tool session tool agent-id))))


(defun claude-gravity-model-complete-tool (session tool-use-id _agent-id result)
  "Mark tool TOOL-USE-ID as done in SESSION with RESULT.
_AGENT-ID is accepted for API compatibility but not used (tree routes at insertion)."
  (let ((tool (claude-gravity-model-find-tool session tool-use-id)))
    (when tool
      (setf (alist-get 'status tool) "done")
      (setf (alist-get 'result tool) result))))


(defun claude-gravity-model-find-tool (session tool-use-id)
  "Find and return tool alist in SESSION matching TOOL-USE-ID, or nil.
Uses the :tool-index hash table for O(1) lookup."
  (gethash tool-use-id (plist-get session :tool-index)))


(defun claude-gravity-model-add-agent (session agent)
  "Add AGENT alist to SESSION if not already present.
Registers in :agent-index for O(1) lookup and adds to turn tree."
  (let* ((new-id (alist-get 'agent_id agent))
         (existing (claude-gravity--find-agent session new-id)))
    (unless existing
      ;; Register in agent-index
      (let ((idx (plist-get session :agent-index)))
        (when (hash-table-p idx)
          (puthash new-id agent idx)))
      ;; Add to turn tree
      (claude-gravity--tree-add-agent session agent))))


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