;;; claude-gravity.el --- Claude Code UI for Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  User

;; Author: User <user@example.com>
;; Version: 0.2.0
;; Package-Requires: ((emacs "27.1") (magit-section "3.0.0") (transient "0.3.0"))
;; Keywords: tools, ai, claude

;;; Commentary:
;; A Magit-like interface for Claude Code with multi-session support.

;;; Code:

(require 'magit-section)
(require 'transient)
(require 'json)
(require 'seq)
(require 'cl-lib)

(defgroup claude-gravity nil
  "Claude Code interface."
  :group 'tools)

(defconst claude-gravity--indent-step 2
  "Indentation step (in spaces) per nesting level.")

(defconst claude-gravity--indent-continuation 4
  "Extra indentation (in spaces) for wrapped/continuation lines after a label.")

(defun claude-gravity--section-depth ()
  "Return the nesting depth of the section currently being inserted.
Depth 0 is the root section.  Each nested `magit-insert-section' adds 1."
  (let ((depth 0)
        (s magit-insert-section--current))
    (while (and s (oref s parent))
      (setq depth (1+ depth)
            s (oref s parent)))
    depth))

(defun claude-gravity--indent (&optional extra)
  "Return indentation string for the current section depth.
EXTRA is additional spaces beyond the depth-based indent."
  (make-string (+ (* (claude-gravity--section-depth) claude-gravity--indent-step)
                   (or extra 0))
               ?\s))

(defvar claude-gravity--margin-char "┊"
  "Current margin indicator character.
Dynamically let-bound inside agent branches to use a different character.")

(defvar claude-gravity--margin-face 'claude-gravity-margin-indicator
  "Current margin indicator face.
Dynamically let-bound inside agent branches to use a different color.")

(defvar claude-gravity--agent-depth 0
  "Current agent nesting depth.
Dynamically let-bound inside agent branches for nested tint selection.")

(defvar claude-gravity-buffer-name "*Structured Claude Sessions*"
  "Name of the overview buffer.")

;;; Comments (Mock)

(defun claude-gravity--make-comment-overlay (beg end text)
  "Create a comment overlay from BEG to END with TEXT."
  (let ((ov (make-overlay beg end)))
    (overlay-put ov 'face '(:underline (:style wave :color "orange")))
    (overlay-put ov 'help-echo text)
    (overlay-put ov 'claude-comment t)))

(defun claude-gravity-comment-at-point ()
  "Mock action: Comment on the item at point."
  (interactive)
  (let ((section (magit-current-section)))
    (if section
        (let* ((beg (magit-section-start section))
               (end (magit-section-end section))
               (text (read-string "Comment: ")))
          (claude-gravity--make-comment-overlay beg end text)
          (message "Added comment: %s" text))
      (message "No section selected"))))

;;; Session Registry

(defvar claude-gravity--sessions (make-hash-table :test 'equal)
  "Hash table mapping session-id -> session plist.
Each session plist has keys:
  :session-id      - string
  :cwd             - string (project working directory)
  :project         - string (basename of cwd)
  :state           - alist with (tools . []) (chat . [])
  :status          - symbol: active or ended
  :start-time      - time value
  :last-event-time - time value (updated on every event)
  :pid             - integer (Claude Code process ID) or nil
  :plan            - plist (:content STRING) or nil")

(defvar-local claude-gravity--buffer-session-id nil
  "Session ID for this per-session buffer.")

(defun claude-gravity--get-session (session-id)
  "Return session plist for SESSION-ID, or nil."
  (gethash session-id claude-gravity--sessions))

(defun claude-gravity--normalize-cwd (cwd)
  "Strip trailing slash from CWD unless it is root /."
  (if (and cwd (> (length cwd) 1) (string-suffix-p "/" cwd))
      (substring cwd 0 -1)
    (or cwd "")))

(defun claude-gravity--session-short-id (session-id)
  "Return first 4 chars of SESSION-ID for display."
  (if (and session-id (> (length session-id) 4))
      (substring session-id 0 4)
    (or session-id "?")))

(defun claude-gravity--session-label (session)
  "Return display label: slug if available, else short session-id."
  (or (plist-get session :slug)
      (claude-gravity--session-short-id (plist-get session :session-id))))

(defun claude-gravity--ensure-session (session-id cwd)
  "Get or create session for SESSION-ID with CWD.  Returns session plist."
  (or (gethash session-id claude-gravity--sessions)
      (let ((session (list :session-id session-id
                           :cwd (claude-gravity--normalize-cwd (or cwd ""))
                           :project (file-name-nondirectory
                                     (directory-file-name (or cwd "")))
                           :state (list (cons 'tools []) (cons 'chat []))
                           :status 'active
                           :claude-status 'idle
                           :start-time (current-time)
                           :last-event-time (current-time)
                           :pid nil
                           :plan nil
                           :prompts []
                           :agents []
                           :files (make-hash-table :test 'equal)
                           :tasks (make-hash-table :test 'equal)
                           :current-turn 0
                           :permission-mode nil
                           :slug nil
                           :buffer nil)))
        (puthash session-id session claude-gravity--sessions)
        (claude-gravity--load-allow-patterns session)
        session)))

(defun claude-gravity--reset-session (session)
  "Reset conversational state of SESSION, preserving identity fields.
Called when a session is restarted (e.g. via /reset or /clear)."
  (plist-put session :state (list (cons 'tools []) (cons 'chat [])))
  (plist-put session :claude-status 'idle)
  (plist-put session :start-time (current-time))
  (plist-put session :last-event-time (current-time))
  (plist-put session :plan nil)
  (plist-put session :prompts [])
  (plist-put session :agents [])
  (plist-put session :files (make-hash-table :test 'equal))
  (plist-put session :tasks (make-hash-table :test 'equal))
  (plist-put session :current-turn 0)
  (plist-put session :permission-mode nil)
  (plist-put session :slug nil)
  (plist-put session :status 'active)
  (claude-gravity--load-allow-patterns session)
  (message "Claude Gravity: session %s reset" (plist-get session :session-id))
  session)

(defun claude-gravity--load-allow-patterns (session)
  "Load allow patterns from .claude/settings.local.json for SESSION.
Stores the patterns list on SESSION's :allow-patterns property."
  (let* ((cwd (plist-get session :cwd))
         (settings-path (expand-file-name ".claude/settings.local.json" cwd)))
    (if (and cwd (not (string-empty-p cwd)) (file-exists-p settings-path))
        (condition-case err
            (let* ((json-object-type 'alist)
                   (json-array-type 'list)
                   (data (json-read-file settings-path))
                   (perms (alist-get 'permissions data))
                   (allow (alist-get 'allow perms)))
              (plist-put session :allow-patterns (or allow nil)))
          (error
           (message "Claude Gravity: failed to read allow patterns: %s" err)
           (plist-put session :allow-patterns nil)))
      (plist-put session :allow-patterns nil))))

(defun claude-gravity-get-state ()
  "Return state for the current session buffer, or first active session."
  (if claude-gravity--buffer-session-id
      (let ((session (claude-gravity--get-session claude-gravity--buffer-session-id)))
        (when session (plist-get session :state)))
    ;; Fallback: first active session
    (let ((result (list (cons 'tools []) (cons 'chat []))))
      (maphash (lambda (_id session)
                 (when (eq (plist-get session :status) 'active)
                   (setq result (plist-get session :state))))
               claude-gravity--sessions)
      result)))

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
            (snippet (alist-get 'snippet data)))
       (format "Turn %s — %s"
               (or turn "?")
               (or snippet "idle"))))
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

(defun claude-gravity--find-tool-by-id (tools tool-use-id)
  "Find index of tool in TOOLS vector matching TOOL-USE-ID."
  (let ((idx nil))
    (dotimes (i (length tools))
      (when (equal (alist-get 'tool_use_id (aref tools i)) tool-use-id)
        (setq idx i)))
    idx))

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
  "Find index of agent in AGENTS vector matching AGENT-ID."
  (let ((idx nil))
    (dotimes (i (length agents))
      (when (equal (alist-get 'agent_id (aref agents i)) agent-id)
        (setq idx i)))
    idx))

(defun claude-gravity--agent-tools-vector (session agent-id)
  "Return the tools vector for AGENT-ID in SESSION.
If AGENT-ID is nil, return the root tools vector.
If the agent is not found, return nil."
  (if (null agent-id)
      (alist-get 'tools (plist-get session :state))
    (let* ((agents (plist-get session :agents))
           (idx (claude-gravity--find-agent-by-id agents agent-id)))
      (when idx
        (alist-get 'agent-tools (aref agents idx))))))

(defun claude-gravity--set-agent-tools-vector (session agent-id new-vec)
  "Set the tools vector for AGENT-ID in SESSION to NEW-VEC.
If AGENT-ID is nil, set root tools."
  (if (null agent-id)
      (setf (alist-get 'tools (plist-get session :state)) new-vec)
    (let* ((agents (plist-get session :agents))
           (idx (claude-gravity--find-agent-by-id agents agent-id)))
      (when idx
        (let ((agent (aref agents idx)))
          (setf (alist-get 'agent-tools agent) new-vec)
          (aset agents idx agent))))))

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
             (vconcat (plist-get session :prompts) (vector entry)))
  (plist-put session :current-turn
             (1+ (or (plist-get session :current-turn) 0))))

(defun claude-gravity-model-finalize-last-prompt (session &optional stop-text stop-thinking)
  "Compute elapsed time on SESSION's last prompt.
Optionally store STOP-TEXT and STOP-THINKING."
  (let* ((prompts (plist-get session :prompts))
         (len (length prompts)))
    (when (> len 0)
      (let ((last-prompt (aref prompts (1- len))))
        ;; Elapsed: key exists in alist (created as nil), so setf is in-place
        (when (and (listp last-prompt)
                   (not (alist-get 'elapsed last-prompt))
                   (alist-get 'submitted last-prompt))
          (setf (alist-get 'elapsed last-prompt)
                (float-time (time-subtract (current-time)
                                           (alist-get 'submitted last-prompt)))))
        ;; stop_text/stop_thinking: new keys, setf conses new head → must write back
        (when stop-text
          (setf (alist-get 'stop_text last-prompt) stop-text))
        (when stop-thinking
          (setf (alist-get 'stop_thinking last-prompt) stop-thinking))
        (when (or stop-text stop-thinking)
          (aset prompts (1- len) last-prompt))))))

(defun claude-gravity-model-update-prompt-answer (session tool-use-id answer)
  "Update the question prompt matching TOOL-USE-ID in SESSION with ANSWER."
  (let ((prompts (plist-get session :prompts)))
    (when (and prompts tool-use-id)
      (dotimes (i (length prompts))
        (let ((p (aref prompts i)))
          (when (and (equal (alist-get 'type p) 'question)
                     (equal (alist-get 'tool_use_id p) tool-use-id))
            (setf (alist-get 'answer p) answer)
            (setf (alist-get 'elapsed p)
                  (float-time (time-subtract (current-time)
                                             (alist-get 'submitted p))))
            (aset prompts i p)))))))

(defun claude-gravity-model-add-tool (session tool agent-id candidate-ids)
  "Add TOOL to SESSION, routing based on AGENT-ID.
AGENT-ID can be a string (definitive agent), \"ambiguous\", or nil (root).
CANDIDATE-IDS is a list of possible agent IDs when ambiguous."
  (cond
   ;; Definitively attributed to an agent
   ((and agent-id (not (equal agent-id "ambiguous")))
    (let ((agent-tools (claude-gravity--agent-tools-vector session agent-id)))
      (if agent-tools
          (claude-gravity--set-agent-tools-vector
           session agent-id (vconcat agent-tools (vector tool)))
        ;; Agent not found — fall back to root
        (let ((root-tools (alist-get 'tools (plist-get session :state))))
          (setf (alist-get 'tools (plist-get session :state))
                (vconcat root-tools (vector tool)))))))
   ;; Ambiguous — store in root with ambiguous flag
   ((equal agent-id "ambiguous")
    (setf (alist-get 'ambiguous tool) t)
    (when candidate-ids
      (setf (alist-get 'candidate-agents tool)
            (append candidate-ids nil)))
    (let ((root-tools (alist-get 'tools (plist-get session :state))))
      (setf (alist-get 'tools (plist-get session :state))
            (vconcat root-tools (vector tool)))))
   ;; No agent context — root tool
   (t
    (let ((root-tools (alist-get 'tools (plist-get session :state))))
      (setf (alist-get 'tools (plist-get session :state))
            (vconcat root-tools (vector tool)))))))

(defun claude-gravity-model-complete-tool (session tool-use-id agent-id result)
  "Mark tool TOOL-USE-ID as done in SESSION with RESULT.
AGENT-ID is used for routing; handles re-attribution of ambiguous tools."
  (let ((target-tools nil)
        (idx nil))
    ;; Search agent tools first if agent specified
    (when (and agent-id (not (equal agent-id "ambiguous")))
      (setq target-tools (claude-gravity--agent-tools-vector session agent-id))
      (when target-tools
        (setq idx (claude-gravity--find-tool-by-id target-tools tool-use-id))))
    ;; Fall back to root
    (unless idx
      (setq target-tools (alist-get 'tools (plist-get session :state)))
      (setq idx (claude-gravity--find-tool-by-id target-tools tool-use-id))
      ;; Re-attribute ambiguous tool if now definitive
      (when (and idx agent-id (not (equal agent-id "ambiguous")))
        (let ((tool (aref target-tools idx)))
          (when (alist-get 'ambiguous tool)
            (setf (alist-get 'ambiguous tool) nil)
            (setf (alist-get 'status tool) "done")
            (setf (alist-get 'result tool) result)
            (let ((agent-tools (claude-gravity--agent-tools-vector session agent-id)))
              (when agent-tools
                (claude-gravity--set-agent-tools-vector
                 session agent-id (vconcat agent-tools (vector tool)))
                (let ((remaining nil))
                  (dotimes (i (length target-tools))
                    (unless (= i idx) (push (aref target-tools i) remaining)))
                  (setf (alist-get 'tools (plist-get session :state))
                        (vconcat (nreverse remaining))))
                (setq idx nil)))))))
    ;; Normal completion
    (when idx
      (let ((tool (aref target-tools idx)))
        (setf (alist-get 'status tool) "done")
        (setf (alist-get 'result tool) result)
        (aset target-tools idx tool)))))

(defun claude-gravity-model-find-tool (session tool-use-id)
  "Find and return tool alist in SESSION matching TOOL-USE-ID, or nil.
Searches both root tools and agent tools."
  (let ((root-tools (alist-get 'tools (plist-get session :state)))
        (agents (plist-get session :agents))
        (result nil))
    ;; Search root tools first
    (when root-tools
      (dotimes (i (length root-tools))
        (when (equal (alist-get 'tool_use_id (aref root-tools i)) tool-use-id)
          (setq result (aref root-tools i)))))
    ;; If not found in root, search agent tools
    (unless result
      (when agents
        (dotimes (i (length agents))
          (let* ((agent (aref agents i))
                 (agent-tools (alist-get 'agent-tools agent)))
            (when agent-tools
              (dotimes (j (length agent-tools))
                (when (equal (alist-get 'tool_use_id (aref agent-tools j)) tool-use-id)
                  (setq result (aref agent-tools j)))))))))
    result))

(defun claude-gravity-model-add-agent (session agent)
  "Add AGENT alist to SESSION's :agents vector if not already present."
  (let* ((agents (plist-get session :agents))
         (new-id (alist-get 'agent_id agent))
         (existing (claude-gravity--find-agent-by-id agents new-id)))
    (unless existing
      (plist-put session :agents (vconcat agents (vector agent))))))

(defun claude-gravity-model-complete-agent (session agent-id &rest props)
  "Mark agent AGENT-ID as done in SESSION.
PROPS is a plist with optional keys:
  :transcript-path, :stop-text, :stop-thinking."
  (let* ((agents (plist-get session :agents))
         (idx (claude-gravity--find-agent-by-id agents agent-id)))
    (when idx
      (let ((agent (aref agents idx)))
        (setf (alist-get 'status agent) "done")
        (let ((ts (alist-get 'timestamp agent)))
          (when ts
            (setf (alist-get 'duration agent)
                  (float-time (time-subtract (current-time) ts)))))
        (let ((tp (plist-get props :transcript-path)))
          (when tp (setf (alist-get 'transcript_path agent) tp)))
        (let ((st (plist-get props :stop-text)))
          (when st (setf (alist-get 'stop_text agent) st)))
        (let ((sth (plist-get props :stop-thinking)))
          (when sth (setf (alist-get 'stop_thinking agent) sth)))
        (aset agents idx agent)))))

(defun claude-gravity-model-move-tools-to-agent (session agent-id tool-ids)
  "Move ambiguous root tools matching TOOL-IDS to agent AGENT-ID in SESSION."
  (when (and tool-ids (> (length tool-ids) 0))
    (let* ((agents (plist-get session :agents))
           (idx (claude-gravity--find-agent-by-id agents agent-id)))
      (when idx
        (let* ((agent (aref agents idx))
               (root-tools (alist-get 'tools (plist-get session :state)))
               (agent-tools (alist-get 'agent-tools agent))
               (to-move nil)
               (keep nil))
          (dotimes (i (length root-tools))
            (let* ((tool (aref root-tools i))
                   (tid (alist-get 'tool_use_id tool)))
              (if (and (alist-get 'ambiguous tool)
                       tid
                       (seq-contains-p tool-ids tid #'equal))
                  (progn
                    (setf (alist-get 'ambiguous tool) nil)
                    (push tool to-move))
                (push tool keep))))
          (when to-move
            (setf (alist-get 'tools (plist-get session :state))
                  (vconcat (nreverse keep)))
            (setf (alist-get 'agent-tools agent)
                  (vconcat agent-tools (vconcat (nreverse to-move))))
            (aset agents idx agent)
            (message "Claude Gravity: moved %d tools to agent %s"
                     (length to-move) agent-id)))))))

(defun claude-gravity-model-add-notification (session notif)
  "Append NOTIF alist to SESSION's :notifications vector."
  (let ((notifs (or (plist-get session :notifications) [])))
    (plist-put session :notifications (vconcat notifs (vector notif)))))

(defun claude-gravity--session-has-tool-p (session tool-use-id)
  "Return non-nil if TOOL-USE-ID exists anywhere in SESSION's tools."
  (or
   (claude-gravity--find-tool-by-id
    (alist-get 'tools (plist-get session :state))
    tool-use-id)
   (let ((agents (plist-get session :agents))
         (found nil))
     (when agents
       (dotimes (i (length agents))
         (unless found
           (let ((agent (aref agents i)))
             (when (claude-gravity--find-tool-by-id
                    (or (alist-get 'agent-tools agent) [])
                    tool-use-id)
               (setq found t))))))
     found)))

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

;;; Event handling
;;
;; Turn demarcation
;; ----------------
;; A "turn" groups a prompt with its resulting tools, agents, and tasks.
;; Turns are advanced (current-turn incremented + prompt entry created) by:
;;
;;   1. UserPromptSubmit — user sends a new prompt
;;   2. ExitPlanMode (PostToolUse) — user approves a plan, creating a
;;      "[Plan approved]" phase-boundary prompt.  Tools after approval
;;      belong to the new execution turn.
;;   3. AskUserQuestion (PreToolUse) — question is shown to user, creating
;;      a question prompt.  The answer and subsequent tools belong to
;;      the question's turn.
;;
;; Each tool, agent, and task captures the current turn number at creation
;; time.  The renderer groups items by turn and shows distinct indicators:
;;   ❯  normal prompt
;;   ?  question (AskUserQuestion)
;;   →  phase boundary (ExitPlanMode)
;;
;; permission_mode (from Claude Code hook payloads) is stored on tool
;; entries and the session for display purposes but is NOT used as a
;; turn boundary signal.

(defun claude-gravity-handle-event (event session-id cwd data &optional pid)
  "Handle EVENT for SESSION-ID (with CWD) carrying DATA.
Optional PID is the Claude Code process ID.
This is the hooks adapter — it parses hook event payloads and calls
the model mutation API to update session state."
  (unless session-id
    (setq session-id "legacy")
    (setq cwd (or cwd "")))
  (message "Handling event: %s for session: %s" event session-id)
  ;; Update PID, last-event-time, and slug on every event
  (let ((existing (claude-gravity--get-session session-id)))
    (when existing
      (claude-gravity-model-update-session-meta
       existing :pid pid :slug (alist-get 'slug data))))
  (pcase event
    ("SessionStart"
     (let ((existing (claude-gravity--get-session session-id)))
       (when existing
         (claude-gravity--reset-session existing)))
     ;; Re-key tmux pending session: match cwd to find temp session
     (let* ((normalized-cwd (claude-gravity--normalize-cwd cwd))
            (pending-temp-id (gethash normalized-cwd claude-gravity--tmux-pending)))
       (when pending-temp-id
         (remhash normalized-cwd claude-gravity--tmux-pending)
         (let ((temp-session (gethash pending-temp-id claude-gravity--sessions))
               (tmux-name (gethash pending-temp-id claude-gravity--tmux-sessions)))
           (when temp-session
             ;; Re-key session from temp-id to real session-id
             (remhash pending-temp-id claude-gravity--sessions)
             (plist-put temp-session :session-id session-id)
             (puthash session-id temp-session claude-gravity--sessions)
             ;; Re-key tmux session mapping
             (when tmux-name
               (remhash pending-temp-id claude-gravity--tmux-sessions)
               (puthash session-id tmux-name claude-gravity--tmux-sessions))
             ;; Rename buffer
             (let ((old-buf (plist-get temp-session :buffer)))
               (when (and old-buf (buffer-live-p old-buf))
                 (let ((new-name (claude-gravity--session-buffer-name temp-session)))
                   (with-current-buffer old-buf
                     (rename-buffer new-name t)
                     (setq claude-gravity--buffer-session-id session-id)))))))))
     (claude-gravity--ensure-session session-id cwd))

    ("SessionEnd"
     (let ((session (claude-gravity--get-session session-id)))
       (when session
         (claude-gravity-model-session-end session)
         ;; Clean up tmux session mapping (tmux process may already be dead)
         (remhash session-id claude-gravity--tmux-sessions)))
     ;; Remove all inbox items for this session
     (claude-gravity--inbox-remove-for-session session-id)
     (claude-gravity--clear-notification-indicator))

    ("UserPromptSubmit"
     (let* ((session (claude-gravity--ensure-session session-id cwd))
            (prompt-text (claude-gravity--strip-system-xml
                          (alist-get 'prompt data))))
       ;; Dedup: if tmux-prompt-sent flag is set, we already created the
       ;; prompt entry in send-prompt — skip to avoid duplicate
       (if (plist-get session :tmux-prompt-sent)
           (plist-put session :tmux-prompt-sent nil)
         (when prompt-text
           (claude-gravity-model-add-prompt
            session (list (cons 'text prompt-text)
                          (cons 'submitted (current-time))
                          (cons 'elapsed nil)))))
       (claude-gravity-model-set-claude-status session 'responding))
     ;; Session is responding — remove idle inbox items
     (claude-gravity--inbox-remove-for-session session-id 'idle)
     (claude-gravity--clear-notification-indicator))

    ("Stop"
     (claude-gravity--clear-notification-indicator)
     (let ((session (claude-gravity--get-session session-id)))
       (when session
         (claude-gravity-model-set-claude-status session 'idle)
         (claude-gravity-model-finalize-last-prompt
          session
          (alist-get 'stop_text data)
          (alist-get 'stop_thinking data))
         (claude-gravity-model-set-token-usage
          session (alist-get 'token_usage data))
         ;; Replace any existing idle inbox item for this session
         (claude-gravity--inbox-remove-for-session session-id 'idle)
         (let* ((turn (plist-get session :current-turn))
                (stop-text (alist-get 'stop_text data))
                (snippet (when stop-text
                           (truncate-string-to-width
                            (replace-regexp-in-string "\n" " " stop-text) 80))))
           (claude-gravity--inbox-add 'idle session-id
                                      `((turn . ,turn) (snippet . ,snippet)) nil)))))

    ("SubagentStart"
     (let* ((session (claude-gravity--ensure-session session-id cwd))
            (new-agent (list (cons 'agent_id (alist-get 'agent_id data))
                             (cons 'type (alist-get 'agent_type data))
                             (cons 'status "running")
                             (cons 'timestamp (current-time))
                             (cons 'turn (or (plist-get session :current-turn) 0))
                             (cons 'agent-tools [])
                             (cons 'transcript_path (alist-get 'agent_transcript_path data)))))
       (claude-gravity-model-add-agent session new-agent)))

    ("SubagentStop"
     (let* ((session (claude-gravity--ensure-session session-id cwd))
            (agent-id (alist-get 'agent_id data)))
       (claude-gravity-model-complete-agent
        session agent-id
        :transcript-path (alist-get 'agent_transcript_path data)
        :stop-text (alist-get 'agent_stop_text data)
        :stop-thinking (alist-get 'agent_stop_thinking data))
       (claude-gravity-model-move-tools-to-agent
        session agent-id (alist-get 'agent_tool_ids data))))

    ("PreToolUse"
     (let* ((session (claude-gravity--ensure-session session-id cwd))
            (parent-agent-id (alist-get 'parent_agent_id data))
            (new-tool (list (cons 'tool_use_id (alist-get 'tool_use_id data))
                            (cons 'name (alist-get 'tool_name data))
                            (cons 'input (alist-get 'tool_input data))
                            (cons 'status "running")
                            (cons 'timestamp (current-time))
                            (cons 'turn (or (plist-get session :current-turn) 0))
                            (cons 'permission_mode (alist-get 'permission_mode data))
                            (cons 'assistant_text (alist-get 'assistant_text data))
                            (cons 'assistant_thinking (alist-get 'assistant_thinking data))
                            (cons 'parent_agent_id parent-agent-id))))
       (claude-gravity-model-add-tool
        session new-tool parent-agent-id
        (alist-get 'candidate_agent_ids data))
       (claude-gravity-model-set-permission-mode
        session (alist-get 'permission_mode data))
       (claude-gravity-model-set-claude-status session 'responding)
       (claude-gravity--track-file session (alist-get 'tool_name data) (alist-get 'tool_input data))
       (claude-gravity--track-task session "PreToolUse" (alist-get 'tool_name data)
                                   (alist-get 'tool_input data) (alist-get 'tool_use_id data))
       ;; AskUserQuestion creates a question prompt and advances the turn
       (when (equal (alist-get 'tool_name data) "AskUserQuestion")
         (let* ((input (alist-get 'tool_input data))
                (questions (alist-get 'questions input))
                (first-q (and (vectorp questions) (> (length questions) 0)
                              (aref questions 0)))
                (q-text (and first-q (alist-get 'question first-q))))
           (when q-text
             (claude-gravity-model-add-prompt
              session (list (cons 'text q-text)
                            (cons 'type 'question)
                            (cons 'tool_use_id (alist-get 'tool_use_id data))
                            (cons 'submitted (current-time))
                            (cons 'elapsed nil)
                            (cons 'answer nil))))))))

    ("PostToolUse"
     (let* ((session (claude-gravity--ensure-session session-id cwd))
            (parent-agent-id (alist-get 'parent_agent_id data))
            (tool-use-id (alist-get 'tool_use_id data))
            (post-text (alist-get 'post_tool_text data))
            (post-think (alist-get 'post_tool_thinking data)))
       ;; Complete tool with response
       (claude-gravity-model-complete-tool
        session tool-use-id parent-agent-id
        (alist-get 'tool_response data))
       ;; Store post-tool text and thinking if present
       (when (and tool-use-id post-text)
         (let ((tool (claude-gravity-model-find-tool session tool-use-id)))
           (when tool
             (setf (alist-get 'post_text tool) post-text))))
       (when (and tool-use-id post-think)
         (let ((tool (claude-gravity-model-find-tool session tool-use-id)))
           (when tool
             (setf (alist-get 'post_thinking tool) post-think))))
       (claude-gravity--track-file session (alist-get 'tool_name data) (alist-get 'tool_input data))
       (claude-gravity--track-task session "PostToolUse" (alist-get 'tool_name data)
                                   (alist-get 'tool_input data) tool-use-id
                                   (alist-get 'tool_response data))
       ;; Store AskUserQuestion answer
       (when (equal (alist-get 'tool_name data) "AskUserQuestion")
         (claude-gravity-model-update-prompt-answer
          session tool-use-id
          (claude-gravity--extract-ask-answer (alist-get 'tool_response data))))
       ;; Detect plan presentation
       (when (equal (alist-get 'tool_name data) "ExitPlanMode")
         (let ((plan-content (alist-get 'plan (alist-get 'tool_input data)))
               (file-path (alist-get 'filePath (alist-get 'tool_response data)))
               (allowed-prompts (alist-get 'allowedPrompts (alist-get 'tool_input data))))
           (when plan-content
             (claude-gravity-model-set-plan
              session (list :content plan-content
                            :file-path file-path
                            :allowed-prompts (append allowed-prompts nil)))))
         ;; Advance turn — plan approval is a phase boundary
         (claude-gravity-model-add-prompt
          session (list (cons 'text "[Plan approved]")
                        (cons 'type 'phase-boundary)
                        (cons 'submitted (current-time))
                        (cons 'elapsed nil))))))

    ("PostToolUseFailure"
     (let* ((session (claude-gravity--ensure-session session-id cwd))
            (parent-agent-id (alist-get 'parent_agent_id data))
            (tool-use-id (alist-get 'tool_use_id data))
            (error-msg (or (alist-get 'error data) "Unknown error"))
            (post-text (alist-get 'post_tool_text data))
            (post-think (alist-get 'post_tool_thinking data)))
       ;; Complete tool with error as result, then set status to error
       (claude-gravity-model-complete-tool
        session tool-use-id parent-agent-id
        (format "[ERROR] %s" error-msg))
       ;; Override status from "done" to "error"
       (let ((tool (claude-gravity-model-find-tool session tool-use-id)))
         (when tool
           (setf (alist-get 'status tool) "error")))
       ;; Store post-tool text and thinking if present
       (when (and tool-use-id post-text)
         (let ((tool (claude-gravity-model-find-tool session tool-use-id)))
           (when tool
             (setf (alist-get 'post_text tool) post-text))))
       (when (and tool-use-id post-think)
         (let ((tool (claude-gravity-model-find-tool session tool-use-id)))
           (when tool
             (setf (alist-get 'post_thinking tool) post-think))))
       (claude-gravity--track-file session (alist-get 'tool_name data) (alist-get 'tool_input data))
       (claude-gravity--track-task session "PostToolUseFailure" (alist-get 'tool_name data)
                                   (alist-get 'tool_input data) tool-use-id
                                   error-msg)))

    ("Notification"
     (let* ((session (claude-gravity--get-session session-id))
            (msg (or (alist-get 'message data) ""))
            (ntype (alist-get 'notification_type data))
            (label (if session
                       (claude-gravity--session-label session)
                     (claude-gravity--session-short-id session-id))))
       (when session
         (claude-gravity-model-add-notification
          session (list (cons 'message msg)
                        (cons 'type ntype)
                        (cons 'timestamp (current-time))))
         ;; Handle reset/clear
         (when (string-match-p "\\(?:reset\\|clear\\)" msg)
           (claude-gravity--reset-session session))
         ;; Update mode-line indicator
         (claude-gravity--update-notification-indicator ntype label))
       ;; Always show in minibuffer
       (message "Claude [%s]: %s" label msg))))

  (claude-gravity--schedule-refresh)
  (when session-id
    (claude-gravity--schedule-session-refresh session-id)))

;;; Faces

(defface claude-gravity-tool-done
  '((t :foreground "green"))
  "Face for completed tool status indicator."
  :group 'claude-gravity)

(defface claude-gravity-tool-running
  '((t :foreground "yellow"))
  "Face for running tool status indicator."
  :group 'claude-gravity)

(defface claude-gravity-tool-error
  '((t :foreground "red"))
  "Face for failed tool status indicator."
  :group 'claude-gravity)

(defface claude-gravity-tool-name
  '((t :weight bold))
  "Face for tool name."
  :group 'claude-gravity)

(defface claude-gravity-detail-label
  '((t :foreground "gray50"))
  "Face for detail labels in expanded tool view."
  :group 'claude-gravity)

(defface claude-gravity-stderr
  '((t :foreground "red"))
  "Face for stderr output."
  :group 'claude-gravity)

(defface claude-gravity-session-ended
  '((t :foreground "gray50"))
  "Face for ended session indicator."
  :group 'claude-gravity)

(defface claude-gravity-prompt
  '((t :foreground "cyan"))
  "Face for user prompt text."
  :group 'claude-gravity)

(defface claude-gravity-task-done
  '((t :foreground "green"))
  "Face for completed task checkbox."
  :group 'claude-gravity)

(defface claude-gravity-task-in-progress
  '((t :foreground "yellow"))
  "Face for in-progress task checkbox."
  :group 'claude-gravity)

(defface claude-gravity-task-pending
  '((t :foreground "gray50"))
  "Face for pending task checkbox."
  :group 'claude-gravity)

(defface claude-gravity-task-active-form
  '((t :foreground "gray50" :slant italic))
  "Face for task activeForm text."
  :group 'claude-gravity)

(defface claude-gravity-status-responding
  '((t :foreground "yellow"))
  "Face for responding status."
  :group 'claude-gravity)

(defface claude-gravity-status-idle
  '((t :foreground "green"))
  "Face for idle status."
  :group 'claude-gravity)

(defface claude-gravity-file-ops
  '((t :foreground "gray60"))
  "Face for file operation labels."
  :group 'claude-gravity)

(defface claude-gravity-question
  '((t :foreground "magenta"))
  "Face for AskUserQuestion prompt indicators."
  :group 'claude-gravity)

(defface claude-gravity-tool-signature
  '((t :foreground "gray45" :slant italic))
  "Face for tool permission signature text."
  :group 'claude-gravity)

(defface claude-gravity-tool-description
  '((t :foreground "#88cc88"))
  "Face for tool description text (the human-readable intent)."
  :group 'claude-gravity)

(defface claude-gravity-assistant-text
  '((t :foreground "#ffbb66"))
  "Face for assistant monologue text between tool calls."
  :group 'claude-gravity)

(defface claude-gravity-thinking
  '((t :foreground "#d0a0ff" :slant italic))
  "Face for assistant extended thinking text."
  :group 'claude-gravity)

(defface claude-gravity-section-heading
  '((t :weight bold :foreground "white"))
  "Face for major section heading text."
  :group 'claude-gravity)

(defface claude-gravity-divider
  '((t :foreground "gray40"))
  "Face for section and turn separator lines."
  :group 'claude-gravity)

(defface claude-gravity-margin-indicator
  '((t :foreground "gray40"))
  "Face for left margin indicators on assistant text."
  :group 'claude-gravity)

(defface claude-gravity-running-bg
  '((((background dark)) :background "#2a2a00")
    (((background light)) :background "#fffde0"))
  "Subtle background highlight for running tools/agents."
  :group 'claude-gravity)

(defface claude-gravity-agent-bg
  '((((background dark)) :background "#0a1a2a")
    (((background light)) :background "#f0f5fa"))
  "Subtle background tint for agent sub-branch content."
  :group 'claude-gravity)

(defface claude-gravity-agent-nested-bg
  '((((background dark)) :background "#0f2030")
    (((background light)) :background "#e8f0f5"))
  "Background for nested agent sub-branches (2+ levels deep)."
  :group 'claude-gravity)

(defface claude-gravity-agent-margin
  '((t :foreground "#5599aa"))
  "Face for margin indicator (┃) inside agent response cycles."
  :group 'claude-gravity)

(defface claude-gravity-header-title
  '((t :weight bold :foreground "white"))
  "Face for the main buffer header title."
  :group 'claude-gravity)

(defface claude-gravity-slug
  '((t :foreground "dark gray" :slant italic))
  "Face for the session slug shown in the header."
  :group 'claude-gravity)

;;; Divider helpers

(defun claude-gravity--section-divider (title)
  "Return a section divider string with TITLE embedded.
Produces: ── Title ──────────────────────
Use as the argument to `magit-insert-heading'."
  (let* ((prefix "── ")
         (suffix " ")
         (label-len (+ (length prefix) (length title) (length suffix)))
         (width (max 40 (- (or (window-width) 80) 2)))
         (remaining (max 3 (- width label-len)))
         (line (make-string remaining ?─)))
    (concat (propertize prefix 'face 'claude-gravity-divider)
            (propertize title 'face 'claude-gravity-section-heading)
            (propertize (concat suffix line) 'face 'claude-gravity-divider))))

(defun claude-gravity--turn-separator ()
  "Insert a thin dashed separator between turns."
  (let* ((indent (claude-gravity--indent))
         (width (max 20 (- (or (window-width) 80) (length indent) 4)))
         (line (make-string width ?╌)))
    (insert indent (propertize line 'face 'claude-gravity-divider) "\n")))

(defun claude-gravity--fontify-markdown (text)
  "Return TEXT with markdown fontification and markup hiding applied.
Uses `markdown-mode' if available; returns TEXT unchanged otherwise."
  (if (fboundp 'markdown-mode)
      (with-temp-buffer
        (insert text)
        (markdown-mode)
        (let ((markdown-hide-markup t))
          (font-lock-ensure))
        (buffer-string))
    text))

(defun claude-gravity--insert-wrapped-with-margin (text indent-or-nil face)
  "Insert TEXT with word-wrap and a ┊ margin indicator.
INDENT-OR-NIL and FACE work like `claude-gravity--insert-wrapped'."
  (when (and text (not (string-empty-p text)))
    (let* ((text (claude-gravity--fontify-markdown text))
           (indent (or indent-or-nil
                       (* (claude-gravity--section-depth) claude-gravity--indent-step)))
           (margin (propertize (concat claude-gravity--margin-char " ")
                              'face claude-gravity--margin-face))
           (prefix (concat (make-string indent ?\s) margin))
           (start (point))
           (fill-column (max 40 (- (or (window-width) 80) 2)))
           (fill-prefix prefix))
      (dolist (para (split-string text "\n"))
        (let ((para-start (point)))
          (insert prefix para "\n")
          (when (> (length para) (- fill-column indent 2))
            (fill-region para-start (point)))))
      (when face
        (add-face-text-property start (point) face)))))

(defun claude-gravity--split-margin-text (text face)
  "Split TEXT into first wrapped margin line and the rest.
Returns (FIRST-LINE . REST-STRING) where FIRST-LINE has ┊ prefix
and FACE applied.  REST-STRING contains remaining lines or nil.
Uses current section depth for indentation."
  (when (and text (stringp text) (not (string-empty-p text)))
    (let* ((text (claude-gravity--fontify-markdown text))
           (indent (* (claude-gravity--section-depth) claude-gravity--indent-step))
           (margin (propertize (concat claude-gravity--margin-char " ")
                              'face claude-gravity--margin-face))
           (prefix (concat (make-string indent ?\s) margin))
           (fc (max 40 (- (or (window-width) 80) 2)))
           (rendered
            (with-temp-buffer
              (let ((fill-column fc)
                    (fill-prefix prefix))
                (dolist (para (split-string text "\n"))
                  (let ((para-start (point)))
                    (insert prefix para "\n")
                    (when (> (length para) (- fc indent 2))
                      (fill-region para-start (point))))))
              (when face
                (add-face-text-property (point-min) (point-max) face))
              (buffer-string))))
      (unless (string-empty-p rendered)
        (let ((nl-pos (string-match "\n" rendered)))
          (if nl-pos
              (let ((first (substring rendered 0 nl-pos))
                    (rest (substring rendered (1+ nl-pos))))
                (cons first (if (string-empty-p rest) nil rest)))
            (cons rendered nil)))))))

;;; Tool display helpers

(defun claude-gravity--extract-ask-answer (response)
  "Extract the user's answer text from AskUserQuestion RESPONSE."
  (cond
   ;; MCP-style vector result
   ((vectorp response)
    (let* ((first (and (> (length response) 0) (aref response 0)))
           (text (and (listp first) (alist-get 'text first))))
      text))
   ;; Alist with answers dict (AskUserQuestion format)
   ((and (listp response) (alist-get 'answers response))
    (let ((answers (alist-get 'answers response)))
      (when (listp answers)
        (cdar answers))))
   ;; Alist with stdout
   ((and (listp response) (alist-get 'stdout response))
    (alist-get 'stdout response))
   ;; Plain string
   ((stringp response) response)
   (t nil)))

(defun claude-gravity--truncate (str max-len)
  "Truncate STR to MAX-LEN chars, adding ellipsis if needed."
  (if (and str (> (length str) max-len))
      (concat (substring str 0 (- max-len 1)) "\u2026")
    (or str "")))

(defun claude-gravity--insert-wrapped (text indent-or-nil &optional face)
  "Insert TEXT with word-wrap, indented by INDENT-OR-NIL spaces.
When INDENT-OR-NIL is nil, uses the current section depth for indentation.
Each paragraph is filled to fit the window width.  Optional FACE
is applied to the inserted text."
  (when (and text (not (string-empty-p text)))
    (let* ((indent (or indent-or-nil
                       (* (claude-gravity--section-depth) claude-gravity--indent-step)))
           (prefix (make-string indent ?\s))
           (start (point))
           (fill-column (max 40 (- (or (window-width) 80) 2)))
           (fill-prefix prefix))
      (dolist (para (split-string text "\n"))
        (let ((para-start (point)))
          (insert prefix para "\n")
          (when (> (length para) (- fill-column indent))
            (fill-region para-start (point)))))
      (when face
        (add-face-text-property start (point) face)))))

(defun claude-gravity--insert-label (text &optional indent)
  "Insert TEXT as a detail label with INDENT spaces (default depth-based)."
  (insert (if indent
              (make-string indent ?\s)
            (claude-gravity--indent))
          (propertize text 'face 'claude-gravity-detail-label)))

(defun claude-gravity--format-elapsed (seconds)
  "Format SECONDS as a human-readable duration string."
  (if (null seconds) "--"
    (let ((secs (truncate seconds)))
      (cond
       ((< secs 60) (format "%ds" secs))
       ((< secs 3600) (format "%dm%02ds" (/ secs 60) (% secs 60)))
       (t (format "%dh%02dm" (/ secs 3600) (% (/ secs 60) 60)))))))

(defun claude-gravity--format-token-count (n)
  "Format token count N compactly: 1234 → 1.2k, 1234567 → 1.2M."
  (cond
   ((null n) "0")
   ((< n 1000) (format "%d" n))
   ((< n 1000000) (format "%.1fk" (/ n 1000.0)))
   (t (format "%.1fM" (/ n 1000000.0)))))

(defun claude-gravity--session-total-elapsed (session)
  "Sum elapsed seconds across all prompts in SESSION."
  (let ((prompts (plist-get session :prompts))
        (total 0.0))
    (when prompts
      (cl-loop for i from 0 below (length prompts)
               for p = (aref prompts i)
               for e = (and (listp p) (alist-get 'elapsed p))
               when (numberp e) do (cl-incf total e)))
    (if (> total 0) total
      ;; Fallback: wall-clock from start to last event
      (let ((start (plist-get session :start-time))
            (last-ev (plist-get session :last-event-time)))
        (when (and start last-ev)
          (float-time (time-subtract last-ev start)))))))

(defun claude-gravity--shorten-path (path)
  "Shorten PATH to project-relative or basename."
  (if (null path) ""
    (let ((name (file-name-nondirectory path)))
      (if (string-empty-p name) path name))))

(defun claude-gravity--tool-summary (name input)
  "Produce a short one-line summary for tool NAME with INPUT alist."
  (pcase name
    ("Bash"
     (let* ((cmd (or (alist-get 'command input) ""))
            (parts (split-string cmd " " t))
            (prog (file-name-nondirectory (or (car parts) "")))
            (args (string-join (cdr parts) " ")))
       (claude-gravity--truncate (concat prog " " args) 60)))
    ("Read"
     (claude-gravity--shorten-path (alist-get 'file_path input)))
    ((or "Edit" "Write")
     (claude-gravity--shorten-path (alist-get 'file_path input)))
    ((or "Grep" "Glob")
     (let ((pattern (alist-get 'pattern input)))
       (if pattern (format "\"%s\"" (claude-gravity--truncate pattern 40)) "")))
    ("AskUserQuestion"
     (let* ((questions (alist-get 'questions input))
            (first-q (and (vectorp questions) (> (length questions) 0)
                          (aref questions 0)))
            (q-text (and first-q (alist-get 'question first-q))))
       (if q-text (claude-gravity--truncate q-text 55) "")))
    (_
     (let ((first-val (cdar input)))
       (if (stringp first-val)
           (claude-gravity--truncate first-val 50)
         "")))))

(defun claude-gravity--tool-description (input)
  "Return the description field from INPUT if present."
  (alist-get 'description input))

(defun claude-gravity--tool-signature (name input)
  "Build the permission-format signature string for tool NAME with INPUT.
Returns a string like \"Bash(npm run build)\" or \"Edit(/path/to/file)\"."
  (pcase name
    ("Bash"
     (let ((cmd (or (alist-get 'command input) "")))
       (format "Bash(%s)" cmd)))
    ((or "Edit" "Write")
     (let ((path (or (alist-get 'file_path input) "")))
       (format "%s(%s)" name path)))
    ("Read"
     (let ((path (or (alist-get 'file_path input) "")))
       (format "Read(%s)" path)))
    ("WebFetch"
     (let* ((url (or (alist-get 'url input) ""))
            (host (when (string-match "https?://\\([^/]+\\)" url)
                    (match-string 1 url))))
       (if host
           (format "WebFetch(domain:%s)" host)
         (format "WebFetch(%s)" url))))
    ((or "Grep" "Glob")
     (let ((pattern (or (alist-get 'pattern input) "")))
       (format "%s(%s)" name pattern)))
    ("Task"
     (let ((subtype (or (alist-get 'subagent_type input) ""))
           (desc (or (alist-get 'description input) "")))
       (if (not (string-empty-p desc))
           (format "Task(%s · %s)" subtype desc)
         (format "Task(%s)" subtype))))
    ("AskUserQuestion"
     "AskUserQuestion")
    (_
     name)))

(defun claude-gravity--insert-label-value (label value &optional face)
  "Insert LABEL followed by VALUE on the same line.
LABEL gets detail-label face, VALUE gets optional FACE."
  (insert (claude-gravity--indent)
          (propertize label 'face 'claude-gravity-detail-label)
          (if face (propertize value face) value)
          "\n"))

(defun claude-gravity--insert-tool-detail (name input result)
  "Insert expanded detail for tool NAME with INPUT and RESULT."
  (pcase name
    ("Bash"
     (let ((cmd (alist-get 'command input)))
       (when cmd
         (claude-gravity--insert-label-value "Command: " cmd))))
    ("Read"
     (let ((path (alist-get 'file_path input)))
       (when path
         (claude-gravity--insert-label-value "File: " path))))
    ((or "Edit" "Write")
     (let ((path (alist-get 'file_path input)))
       (when path
         (claude-gravity--insert-label-value "File: " path))))
    ((or "Grep" "Glob")
     (let ((pattern (alist-get 'pattern input))
           (path (alist-get 'path input)))
       (when pattern
         (claude-gravity--insert-label-value "Pattern: " pattern))
       (when path
         (claude-gravity--insert-label-value "Path: " path))))
    ("AskUserQuestion"
     (let* ((questions (alist-get 'questions input))
            (first-q (and (vectorp questions) (> (length questions) 0)
                          (aref questions 0)))
            (q-text (and first-q (alist-get 'question first-q)))
            (answer (claude-gravity--extract-ask-answer result)))
       (when q-text
         (claude-gravity--insert-label "Question: ")
         (claude-gravity--insert-wrapped q-text nil))
       (when answer
         (claude-gravity--insert-label "Answer: ")
         (claude-gravity--insert-wrapped answer nil 'claude-gravity-question)))))
  ;; Result section
  (when result
    ;; Normalize MCP-style vector results [((type . "text") (text . "..."))]
    (when (vectorp result)
      (let* ((first (and (> (length result) 0) (aref result 0)))
             (text (and (listp first) (alist-get 'text first))))
        (setq result (when text (list (cons 'stdout text))))))
    (let ((stdout (and (listp result) (alist-get 'stdout result)))
          (stderr (and (listp result) (alist-get 'stderr result)))
          (file-data (and (listp result) (alist-get 'file result)))
          (cont-prefix (claude-gravity--indent)))
      ;; Bash-style stdout
      (when (and stdout (not (string-empty-p stdout)))
        (let* ((lines (split-string stdout "\n" t))
               (nlines (length lines))
               (preview (seq-take lines 8)))
          (claude-gravity--insert-label (format "Output (%d lines):\n" nlines))
          (dolist (line preview)
            (insert cont-prefix (claude-gravity--truncate line 80) "\n"))
          (when (> nlines 8)
            (insert (propertize (concat cont-prefix "...\n") 'face 'claude-gravity-detail-label)))))
      ;; Read-style file content
      (when file-data
        (let* ((content (alist-get 'content file-data))
               (num-lines (alist-get 'numLines file-data))
               (total-lines (alist-get 'totalLines file-data)))
          (when content
            (let* ((lines (split-string content "\n"))
                   (preview (seq-take lines 6)))
              (claude-gravity--insert-label (format "Content (%d/%d lines):\n"
                                                    (or num-lines (length lines))
                                                    (or total-lines (length lines))))
              (dolist (line preview)
                (insert cont-prefix (claude-gravity--truncate line 80) "\n"))
              (when (> (length lines) 6)
                (insert (propertize (concat cont-prefix "...\n") 'face 'claude-gravity-detail-label)))))))
      ;; Stderr
      (when (and stderr (not (string-empty-p stderr)))
        (insert cont-prefix
                (propertize "Stderr:\n" 'face 'claude-gravity-stderr))
        (let ((stderr-preview (string-join (seq-take (split-string stderr "\n" t) 4) "\n")))
          (claude-gravity--insert-wrapped stderr-preview nil 'claude-gravity-stderr))))))

;;; Plan display

(defun claude-gravity--update-plan (session content)
  "Update plan for SESSION from CONTENT string."
  (plist-put session :plan (list :content content))
  (claude-gravity--show-plan-buffer session))

(defun claude-gravity--show-plan-buffer (session)
  "Display the plan for SESSION in a dedicated buffer."
  (let ((plan (plist-get session :plan)))
    (when plan
      (let* ((label (claude-gravity--session-label session))
             (buf (get-buffer-create (format "*Claude Plan: %s*" label)))
             (content (plist-get plan :content)))
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert content)
            (goto-char (point-min)))
          (when (fboundp 'markdown-mode)
            (markdown-mode))
          (visual-line-mode 1)
          (setq buffer-read-only t)
          (set-buffer-modified-p nil))
        (pop-to-buffer buf)))))

(defun claude-gravity-show-plan ()
  "Show the plan for the current session."
  (interactive)
  (let* ((sid (or claude-gravity--buffer-session-id
                  (claude-gravity--current-overview-session-id)))
         (session (when sid (claude-gravity--get-session sid))))
    (if (and session (plist-get session :plan))
        (claude-gravity--show-plan-buffer session)
      (message "No plan available"))))

(defun claude-gravity--current-overview-session-id ()
  "Return session-id at point in the overview buffer, or nil."
  (let ((section (magit-current-section)))
    (when (and section (eq (oref section type) 'session-entry))
      (oref section value))))

(defun claude-gravity--plan-preview-lines (content n)
  "Return first N non-empty lines from plan CONTENT."
  (let ((lines (split-string content "\n"))
        result)
    (dolist (line lines)
      (when (and (< (length result) n)
                 (not (string-empty-p (string-trim line))))
        (push line result)))
    (nreverse result)))

(defun claude-gravity--format-allowed-prompts (prompts)
  "Format PROMPTS list as \"Tool(description), ...\" string."
  (mapconcat (lambda (p)
               (format "%s(%s)"
                       (or (alist-get 'tool p) "?")
                       (or (alist-get 'prompt p) "?")))
             prompts ", "))

(defun claude-gravity-open-plan-file ()
  "Open the plan file for the current session."
  (interactive)
  (let* ((sid (or claude-gravity--buffer-session-id
                  (claude-gravity--current-overview-session-id)))
         (session (when sid (claude-gravity--get-session sid)))
         (plan (when session (plist-get session :plan)))
         (fpath (when plan (plist-get plan :file-path))))
    (cond
     ((and fpath (file-exists-p fpath)) (find-file fpath))
     (fpath (message "Plan file not found: %s" fpath))
     (t (message "No plan file path available")))))

(defun claude-gravity-insert-plan (session)
  "Insert plan section for SESSION with inline preview."
  (let ((plan (plist-get session :plan)))
    (when plan
      (let* ((content (plist-get plan :content))
             (file-path (plist-get plan :file-path))
             (allowed-prompts (plist-get plan :allowed-prompts))
             (preview-lines (claude-gravity--plan-preview-lines content 8))
             (all-lines (split-string content "\n" t "[ \t]"))
             (truncated (> (length all-lines) (length preview-lines))))
        (magit-insert-section (plan nil t)
          (magit-insert-heading
            (claude-gravity--section-divider "Plan"))
          (claude-gravity--insert-wrapped
           (string-join preview-lines "\n") nil)
          (when truncated
            (insert (propertize (concat (claude-gravity--indent) "...\n")
                                'face 'claude-gravity-detail-label)))
          (when allowed-prompts
            (claude-gravity--insert-label "Permissions: ")
            (claude-gravity--insert-wrapped
             (claude-gravity--format-allowed-prompts allowed-prompts) nil))
          (when file-path
            (claude-gravity--insert-label "File: ")
            (claude-gravity--insert-wrapped
             (concat file-path "  " (propertize "(F to open)" 'face 'claude-gravity-detail-label))
             nil))
          (insert (propertize (concat (claude-gravity--indent) "P to view full plan\n")
                              'face 'claude-gravity-detail-label))
          (insert "\n"))))))

(defun claude-gravity-insert-streaming-text (session)
  "Insert live streaming text section for SESSION.
Shows assistant text as it generates from the JSON-output adapter.
Only visible when :streaming-text is non-nil (during active generation)."
  (let ((text (plist-get session :streaming-text)))
    (when (and text (stringp text) (not (string-empty-p text)))
      (magit-insert-section (streaming-text nil t)
        (magit-insert-heading
          (concat
           (propertize ">>> " 'face 'claude-gravity-status-responding)
           (propertize "Claude is responding..." 'face 'claude-gravity-assistant-text)))
        (claude-gravity--insert-wrapped-with-margin text nil 'claude-gravity-assistant-text)
        (insert "\n")))))

;;; Section renderers (used by per-session buffers)

(defun claude-gravity-insert-header (session)
  "Insert header section showing session slug, tool count, elapsed time, and tokens from SESSION."
  (let* ((state (plist-get session :state))
         (tool-count (length (alist-get 'tools state)))
         (slug (claude-gravity--session-label session))
         (elapsed (claude-gravity--session-total-elapsed session))
         (usage (plist-get session :token-usage))
         (in-tokens (when usage
                      (+ (or (alist-get 'input_tokens usage) 0)
                         (or (alist-get 'cache_read_input_tokens usage) 0)
                         (or (alist-get 'cache_creation_input_tokens usage) 0))))
         (out-tokens (when usage (or (alist-get 'output_tokens usage) 0)))
         (width (max 40 (- (or (window-width) 80) 2)))
         (top-line (make-string width ?━)))
    (magit-insert-section (header)
      (insert (propertize top-line 'face 'claude-gravity-divider) "\n")
      (magit-insert-heading
        (concat
         (propertize "Structured Claude Session" 'face 'claude-gravity-header-title)
         (propertize (format "  %s" slug) 'face 'claude-gravity-slug)
         (when (gethash (plist-get session :session-id) claude-gravity--tmux-sessions)
           (propertize " [tmux]" 'face 'claude-gravity-detail-label))
         (propertize (format "  ◆ %d tools" tool-count) 'face 'claude-gravity-detail-label)
         (when elapsed
           (propertize (format "  ⏱ %s" (claude-gravity--format-elapsed elapsed))
                       'face 'claude-gravity-detail-label))
         (when (and in-tokens (> in-tokens 0))
           (propertize (format "  ↓%s ↑%s tokens"
                               (claude-gravity--format-token-count in-tokens)
                               (claude-gravity--format-token-count out-tokens))
                       'face 'claude-gravity-detail-label))))
      (insert (propertize top-line 'face 'claude-gravity-divider) "\n")
      (insert "\n"))))

(defun claude-gravity--tasks-by-turn (tasks-ht)
  "Group tasks from TASKS-HT by turn number.
Returns a hash table mapping turn -> list of task alists."
  (let ((groups (make-hash-table :test 'equal)))
    (when tasks-ht
      (maphash (lambda (key val)
                 (unless (string-prefix-p "_pending_" key)
                   (let ((turn (or (alist-get 'turn val) 0)))
                     (puthash turn (append (gethash turn groups) (list val)) groups))))
               tasks-ht))
    groups))

(defun claude-gravity--turn-counts (tools agents tasks)
  "Format count string for TOOLS, AGENTS, TASKS lists.
Format: [Nt Ma P tools] — tasks, agents, total tools."
  (let ((parts nil))
    ;; Build in reverse order (push reverses), final: tasks agents tools
    (when (and tools (> (length tools) 0))
      (push (format "%d tools" (length tools)) parts))
    (when (and agents (> (length agents) 0))
      (push (format "%da" (length agents)) parts))
    (when (and tasks (> (length tasks) 0))
      (push (format "%dt" (length tasks)) parts))
    (if parts
        (propertize (format "[%s]" (string-join parts " "))
                    'face 'claude-gravity-detail-label)
      "")))

(defun claude-gravity--text-subsumes-p (a b)
  "Return non-nil if text A fully contains B's content.
Checks equality and paragraph-boundary prefix match."
  (and a b
       (not (string-empty-p a))
       (not (string-empty-p b))
       (or (equal a b)
           (string-prefix-p (concat b "\n\n") a)
           (string-prefix-p (concat a "\n\n") b))))

(defun claude-gravity--dedup-assistant-text (tools)
  "Remove duplicate assistant_text and assistant_thinking on consecutive TOOLS.
Handles three cases:
1. Parallel tool calls get the same preceding content — clear on 2nd+ tool.
2. Post-tool text on tool N overlaps with assistant_text on tool N+1 — keep
   the more complete version, clear the other.
3. Post-tool thinking similarly deduped."
  (let ((prev-text nil)
        (prev-thinking nil)
        (prev-post-text nil)
        (prev-post-thinking nil))
    (dolist (item tools)
      (let ((atext (alist-get 'assistant_text item))
            (athink (alist-get 'assistant_thinking item)))
        ;; Case 1: parallel tools — same preceding content
        (when (and atext (equal atext prev-text))
          (setf (alist-get 'assistant_text item) nil)
          (setq atext nil))
        (when (and athink (equal athink prev-thinking))
          (setf (alist-get 'assistant_thinking item) nil)
          (setq athink nil))
        ;; Case 2: post_text of previous tool overlaps with this tool's assistant_text
        ;; Keep the more complete version, clear the shorter one
        (when (and atext prev-post-text
                   (claude-gravity--text-subsumes-p atext prev-post-text))
          (if (>= (length prev-post-text) (length atext))
              ;; post_text is more complete or equal — clear assistant_text
              (progn
                (setf (alist-get 'assistant_text item) nil)
                (setq atext nil))
            ;; assistant_text is more complete — clear post_text on previous tool
            ;; (iterate to find previous tool with this post_text)
            (claude-gravity--clear-post-text-matching tools prev-post-text)
            (setq prev-post-text nil)))
        ;; Same for thinking
        (when (and athink prev-post-thinking
                   (claude-gravity--text-subsumes-p athink prev-post-thinking))
          (if (>= (length prev-post-thinking) (length athink))
              (progn
                (setf (alist-get 'assistant_thinking item) nil)
                (setq athink nil))
            (claude-gravity--clear-post-thinking-matching tools prev-post-thinking)
            (setq prev-post-thinking nil)))
        ;; Track for next iteration
        (setq prev-text atext
              prev-thinking athink
              prev-post-text (alist-get 'post_text item)
              prev-post-thinking (alist-get 'post_thinking item))))))

(defun claude-gravity--clear-post-text-matching (tools text)
  "Clear post_text on the tool in TOOLS whose post_text equals TEXT."
  (dolist (item tools)
    (when (equal (alist-get 'post_text item) text)
      (setf (alist-get 'post_text item) nil))))

(defun claude-gravity--clear-post-thinking-matching (tools text)
  "Clear post_thinking on the tool in TOOLS whose post_thinking equals TEXT."
  (dolist (item tools)
    (when (equal (alist-get 'post_thinking item) text)
      (setf (alist-get 'post_thinking item) nil))))

(defun claude-gravity--insert-tool-context (item)
  "Insert thinking and assistant text from ITEM as standalone interlaced lines.
Called before each tool item in the rendering loop."
  (let ((athink (alist-get 'assistant_thinking item))
        (atext (alist-get 'assistant_text item))
        (indent (or (* (claude-gravity--section-depth) claude-gravity--indent-step) 0)))
    ;; Trim leading/trailing whitespace
    (when athink (setq athink (string-trim athink)))
    (when atext (setq atext (string-trim atext)))
    ;; Thinking: show "┊ Thinking..." header, then content indented below
    (when (and athink (not (string-empty-p athink)))
      (let* ((margin (propertize (concat claude-gravity--margin-char " ")
                                'face claude-gravity--margin-face))
             (prefix (concat (make-string indent ?\s) margin)))
        (insert prefix (propertize "Thinking..." 'face 'claude-gravity-thinking) "\n")
        ;; Content below with plain indent (no ┊)
        (claude-gravity--insert-wrapped athink (+ indent 4) 'claude-gravity-thinking))
      (insert "\n"))
    ;; Assistant text: show with ┊ prefix on first line
    (when (and atext (not (string-empty-p atext)))
      (claude-gravity--insert-wrapped-with-margin atext nil 'claude-gravity-assistant-text)
      (insert "\n"))))

(defun claude-gravity--group-response-cycles (tools)
  "Group TOOLS into response cycles based on assistant text boundaries.
A new cycle starts at each tool with non-nil assistant_text or
assistant_thinking (after dedup).  Returns a list of tool-lists."
  (let ((cycles nil)
        (current nil))
    (dolist (item tools)
      (let ((atext (alist-get 'assistant_text item))
            (athink (alist-get 'assistant_thinking item)))
        (when (and current
                   (or (and atext (not (string-empty-p atext)))
                       (and athink (not (string-empty-p athink)))))
          ;; New assistant text = new response cycle
          (push (nreverse current) cycles)
          (setq current nil))
        (push item current)))
    (when current
      (push (nreverse current) cycles))
    (nreverse cycles)))

(defun claude-gravity--build-agent-lookup (agents)
  "Build lookup table from AGENTS for correlating with Task tool calls.
Returns a hash table mapping \"TURN:TYPE\" to a list of agent entries.
Multiple agents with the same turn+type are returned in order."
  (let ((ht (make-hash-table :test 'equal)))
    (dolist (agent (append (or agents []) nil))
      (let* ((turn (or (alist-get 'turn agent) 0))
             (atype (or (alist-get 'type agent) ""))
             (key (format "%d:%s" turn atype)))
        (puthash key (append (gethash key ht) (list agent)) ht)))
    ht))

(defun claude-gravity--pop-matching-agent (lookup turn subagent-type)
  "Pop and return the first matching agent from LOOKUP for TURN and SUBAGENT-TYPE."
  (when (and turn subagent-type)
    (let* ((key (format "%d:%s" turn subagent-type))
           (agents (gethash key lookup)))
      (when agents
        (puthash key (cdr agents) lookup)
        (car agents)))))

(defun claude-gravity--response-cycle-heading (cycle)
  "Return (TEXT . FACE) for a response CYCLE heading.
Shows tool count; full assistant text is rendered in the expanded body."
  (cons (format "%d tool%s" (length cycle) (if (= (length cycle) 1) "" "s"))
        'claude-gravity-detail-label))

(defun claude-gravity--insert-turn-children (tools agents tasks)
  "Insert response cycles, inline agent annotations, and tasks for a turn.
Tools are grouped into response cycles (each assistant message + its tool calls)."
  ;; Filter out AskUserQuestion (shown as prompt entries, not tool items)
  (setq tools (cl-remove-if (lambda (item) (equal (alist-get 'name item) "AskUserQuestion")) tools))
  ;; Dedup assistant text across parallel tool calls
  (when tools (claude-gravity--dedup-assistant-text tools))
  ;; Build agent lookup for inline annotation
  (let ((agent-lookup (claude-gravity--build-agent-lookup agents)))
    ;; Render tools as response cycles
    (when (and tools (> (length tools) 0))
      (let* ((cycles (claude-gravity--group-response-cycles tools))
             (n-cycles (length cycles))
             (cycle-idx 0))
        (dolist (cycle cycles)
          (let* ((is-last (= cycle-idx (1- n-cycles)))
                 (all-done (cl-every (lambda (item) (equal (alist-get 'status item) "done")) cycle))
                 (should-collapse (and (not is-last) all-done))
                 (first-item (car cycle))
                 (athink (alist-get 'assistant_thinking first-item))
                 (atext (alist-get 'assistant_text first-item))
                 (athink (when athink (string-trim athink)))
                 (atext (when atext (string-trim atext)))
                 (split (when (and atext (not (string-empty-p atext)))
                          (claude-gravity--split-margin-text atext 'claude-gravity-assistant-text)))
                 (tools-label (format "%d tool%s" (length cycle)
                                      (if (= (length cycle) 1) "" "s"))))
            ;; Thinking stays outside the section (always visible)
            (when (and athink (not (string-empty-p athink)))
              (let* ((indent (or (* (claude-gravity--section-depth) claude-gravity--indent-step) 0))
                     (margin (propertize (concat claude-gravity--margin-char " ")
                                        'face claude-gravity--margin-face))
                     (prefix (concat (make-string indent ?\s) margin)))
                (insert prefix (propertize "Thinking..." 'face 'claude-gravity-thinking) "\n")
                (claude-gravity--insert-wrapped athink (+ indent 4) 'claude-gravity-thinking)))
            ;; Response cycle section — heading is first line of assistant text
            (magit-insert-section (response-cycle cycle-idx should-collapse)
              (magit-insert-heading
                (if split
                    (car split)
                  (format "%s%s%s"
                          (claude-gravity--indent)
                          (propertize (concat claude-gravity--margin-char " ")
                                      'face claude-gravity--margin-face)
                          (propertize tools-label 'face 'claude-gravity-detail-label))))
              ;; Body: remaining assistant text lines (visible when collapsed)
              (when (cdr split)
                (insert (cdr split))
                ;; Move content marker past assistant text so it stays
                ;; visible when the section is collapsed
                (oset magit-insert-section--current content
                      (if magit-section-inhibit-markers (point) (point-marker))))
              ;; Body: tool count summary (when heading was assistant text)
              (when split
                (insert (format "%s%s%s\n"
                                (claude-gravity--indent)
                                (propertize (concat claude-gravity--margin-char " ")
                                            'face claude-gravity--margin-face)
                                (propertize tools-label 'face 'claude-gravity-detail-label))))
              ;; First tool (context already rendered as heading/body above)
              (claude-gravity--insert-tool-item first-item agent-lookup)
              ;; Remaining tools with their context
              (dolist (item (cdr cycle))
                (claude-gravity--insert-tool-context item)
                (claude-gravity--insert-tool-item item agent-lookup)))
            ;; Separator between cycles (not after the last)
            (unless is-last
              (claude-gravity--turn-separator)))
          (cl-incf cycle-idx)))))
  ;; Tasks subsection at the end
  (when (and tasks (> (length tasks) 0))
    (let ((sorted (sort (copy-sequence tasks)
                        (lambda (a b)
                          (< (claude-gravity--task-sort-key (alist-get 'status a))
                             (claude-gravity--task-sort-key (alist-get 'status b)))))))
      (let ((completed (cl-count-if (lambda (tk) (equal (alist-get 'status tk) "completed")) sorted))
            (total (length sorted)))
        (magit-insert-section (turn-tasks nil t)
          (magit-insert-heading
            (format "%sTasks (%d/%d)" (claude-gravity--indent) completed total))
          (dolist (task sorted)
            (claude-gravity--insert-task-item task)))))))

(defun claude-gravity--insert-task-item (task)
  "Insert a single TASK as a magit-section."
  (let* ((subject (or (alist-get 'subject task) "(no subject)"))
         (status (or (alist-get 'status task) "pending"))
         (active-form (alist-get 'activeForm task))
         (checkbox (pcase status
                     ("completed"
                      (propertize "[x]" 'face 'claude-gravity-task-done))
                     ("in_progress"
                      (propertize "[/]" 'face 'claude-gravity-task-in-progress))
                     (_
                      (propertize "[ ]" 'face 'claude-gravity-task-pending))))
         (suffix (if (and (equal status "in_progress") active-form)
                     (concat "  " (propertize active-form 'face 'claude-gravity-task-active-form))
                   "")))
    (magit-insert-section (task task)
      (insert (format "%s%s %s%s\n" (claude-gravity--indent) checkbox subject suffix)))))

(defun claude-gravity-insert-turns (session)
  "Insert unified turns section for SESSION.
Each turn groups its prompt, tools, agents, and tasks together."
  (let* ((state (plist-get session :state))
         (tools (alist-get 'tools state))
         (prompts (plist-get session :prompts))
         (agents (plist-get session :agents))
         (tasks-ht (plist-get session :tasks))
         (current-turn (or (plist-get session :current-turn) 0))
         (tool-groups (make-hash-table :test 'equal))
         (agent-groups (make-hash-table :test 'equal))
         (task-groups (claude-gravity--tasks-by-turn tasks-ht))
         (max-turn current-turn))
    ;; Build tool groups (excluding AskUserQuestion, shown as prompt entries)
    (dolist (item (append tools nil))
      (unless (equal (alist-get 'name item) "AskUserQuestion")
        (let ((turn (or (alist-get 'turn item) 0)))
          (when (> turn max-turn) (setq max-turn turn))
          (puthash turn (append (gethash turn tool-groups) (list item)) tool-groups))))
    ;; Build agent groups
    (dolist (agent (append (or agents []) nil))
      (let ((turn (or (alist-get 'turn agent) 0)))
        (when (> turn max-turn) (setq max-turn turn))
        (puthash turn (append (gethash turn agent-groups) (list agent)) agent-groups)))
    ;; Check max-turn from task groups
    (maphash (lambda (turn _)
               (when (and (numberp turn) (> turn max-turn))
                 (setq max-turn turn)))
             task-groups)
    ;; Render if there are any turns or pre-prompt activity
    (when (or (> max-turn 0)
              (gethash 0 tool-groups)
              (gethash 0 agent-groups)
              (gethash 0 task-groups))
      (magit-insert-section (turns nil t)
        (magit-insert-heading
          (claude-gravity--section-divider (format "Turns (%d)" current-turn)))
        ;; Turn 0: pre-prompt activity
        (let ((t0-tools (gethash 0 tool-groups))
              (t0-agents (gethash 0 agent-groups))
              (t0-tasks (gethash 0 task-groups)))
          (when (or t0-tools t0-agents t0-tasks)
            (magit-insert-section (turn 0 t)
              (magit-insert-heading
                (format "%s  %s"
                        (propertize "Pre-prompt activity" 'face 'claude-gravity-detail-label)
                        (claude-gravity--turn-counts t0-tools t0-agents t0-tasks)))
              (claude-gravity--insert-turn-children t0-tools t0-agents t0-tasks))))
        ;; Turns 1..max-turn
        (let ((prev-turn-rendered nil))
          (dotimes (j max-turn)
            (let* ((i (1+ j))
                   (prompt-entry (when (and prompts (> (length prompts) 0)
                                            (<= i (length prompts)))
                                   (aref prompts (1- i))))
                   (turn-tools (gethash i tool-groups))
                   (turn-agents (gethash i agent-groups))
                   (turn-tasks (gethash i task-groups))
                   (is-current (= i current-turn))
                 (prompt-text (when prompt-entry
                                (claude-gravity--prompt-text prompt-entry)))
                 (is-question (when (listp prompt-entry)
                                (eq (alist-get 'type prompt-entry) 'question)))
                 (is-phase-boundary (when (listp prompt-entry)
                                      (eq (alist-get 'type prompt-entry) 'phase-boundary)))
                 (elapsed (when (listp prompt-entry)
                            (alist-get 'elapsed prompt-entry)))
                 (elapsed-str (claude-gravity--format-elapsed elapsed))
                 (indicator (cond (is-phase-boundary
                                   (propertize "→" 'face 'claude-gravity-detail-label))
                                  (is-question
                                   (propertize "?" 'face 'claude-gravity-question))
                                  (t
                                   (propertize "❯" 'face 'claude-gravity-prompt))))
                 (counts (claude-gravity--turn-counts turn-tools turn-agents turn-tasks))
                 (answer (when is-question (alist-get 'answer prompt-entry)))
                 (answer-suffix (if answer
                                    (format "  → %s" (claude-gravity--truncate answer 40))
                                  ""))
                 (prompt-face (cond (is-phase-boundary 'claude-gravity-detail-label)
                                    (is-question 'claude-gravity-question)
                                    (t 'claude-gravity-prompt)))
                 (_heading-parts (list indicator
                                       (propertize (claude-gravity--truncate (or prompt-text "(no prompt)") 60)
                                                   'face prompt-face)
                                       (propertize answer-suffix 'face 'claude-gravity-detail-label)
                                       (propertize counts 'face 'claude-gravity-detail-label)
                                       (propertize elapsed-str 'face 'claude-gravity-detail-label))))
            (when (or turn-tools turn-agents turn-tasks prompt-entry)
              ;; Turn separator between turns
              (when prev-turn-rendered
                (claude-gravity--turn-separator))
              (setq prev-turn-rendered t)
              ;; Full prompt text outside the collapsible section
              (when prompt-text
                (let* ((indent (claude-gravity--indent))
                       (cont-indent (+ (length indent) 2)))
                  (insert (format "%s%s " indent indicator))
                  (let ((start (point))
                        (fill-column (max 40 (- (or (window-width) 80) 2)))
                        (fill-prefix (make-string cont-indent ?\s)))
                    (insert prompt-text "\n")
                    (when (> (length prompt-text) (- fill-column cont-indent))
                      (fill-region start (point)))
                    (add-face-text-property start (point) prompt-face))))
              (magit-insert-section (turn i (not is-current))
                (magit-insert-heading
                  (format "%s%s%s  %s"
                          (claude-gravity--indent)
                          (propertize counts 'face 'claude-gravity-detail-label)
                          (propertize answer-suffix 'face 'claude-gravity-detail-label)
                          (propertize elapsed-str 'face 'claude-gravity-detail-label)))
                ;; Children: tools, agents, tasks
                (claude-gravity--insert-turn-children turn-tools turn-agents turn-tasks)
                ;; Trailing assistant text (conclusion after last tool)
                ;; Dedup against last tool's post_text/post_thinking
                (let* ((stop-think (and (listp prompt-entry) (alist-get 'stop_thinking prompt-entry)))
                       (stop-text (and (listp prompt-entry) (alist-get 'stop_text prompt-entry)))
                       (last-tool (and turn-tools
                                       (car (last turn-tools))))
                       (last-post-text (when last-tool (alist-get 'post_text last-tool)))
                       (last-post-think (when last-tool (alist-get 'post_thinking last-tool))))
                  ;; Dedup stop_thinking vs last tool's post_thinking
                  (when (and stop-think last-post-think
                             (claude-gravity--text-subsumes-p stop-think last-post-think))
                    (if (>= (length last-post-think) (length stop-think))
                        (setq stop-think nil) ; post_thinking covers it
                      (setf (alist-get 'post_thinking last-tool) nil))) ; stop has more
                  ;; Dedup stop_text vs last tool's post_text
                  (when (and stop-text last-post-text
                             (claude-gravity--text-subsumes-p stop-text last-post-text))
                    (if (>= (length last-post-text) (length stop-text))
                        (setq stop-text nil) ; post_text covers it
                      (setf (alist-get 'post_text last-tool) nil))) ; stop has more
                  (when (and stop-think (not (string-empty-p stop-think)))
                    (claude-gravity--insert-wrapped-with-margin
                     stop-think nil 'claude-gravity-thinking))
                  (when (and stop-text (not (string-empty-p stop-text)))
                    (claude-gravity--insert-wrapped-with-margin stop-text nil 'claude-gravity-assistant-text))))))))
        (insert "\n")))))

(defun claude-gravity--insert-agent-branch (item agent agent-lookup &optional depth)
  "Insert ITEM (a Task tool) as a sub-branch with AGENT's nested tools.
AGENT-LOOKUP is passed through for nested agent resolution.
DEPTH tracks nesting level (0 = first agent, 1+ = nested sub-agents).
The agent's tools are rendered using the same response-cycle grouping
as root tools, allowing recursive sub-branches for nested agents."
  (let* ((name (alist-get 'name item))
         (status (alist-get 'status item))
         (input (alist-get 'input item))
         (done-p (equal status "done"))
         (error-p (equal status "error"))
         (agent-done-p (equal (alist-get 'status agent) "done"))
         (indicator (propertize (cond (error-p "[!]")
                                      (done-p "[x]")
                                      (t "[/]"))
                                'face (cond (error-p 'claude-gravity-tool-error)
                                            (done-p 'claude-gravity-tool-done)
                                            (t 'claude-gravity-tool-running))))
         (desc (claude-gravity--tool-description input))
         (atype (alist-get 'type agent))
         (aid (alist-get 'agent_id agent))
         (short-id (if (and aid (> (length aid) 7))
                       (substring aid 0 7)
                     (or aid "?")))
         (adur (alist-get 'duration agent))
         (dur-str (if (and agent-done-p adur)
                      (format "  %s" (claude-gravity--format-duration adur))
                    ""))
         (agent-suffix (format "  %s %s (%s)%s"
                               (propertize "→" 'face 'claude-gravity-detail-label)
                               (propertize (or atype "?") 'face 'claude-gravity-tool-name)
                               (propertize short-id 'face 'claude-gravity-detail-label)
                               dur-str))
         (agent-tools (alist-get 'agent-tools agent))
         (tool-list (when agent-tools (append agent-tools nil)))
         ;; Filter AskUserQuestion from agent tools
         (tool-list (cl-remove-if
                     (lambda (t) (equal (alist-get 'name t) "AskUserQuestion"))
                     tool-list))
         ;; Collapse when agent is done
         (collapsed (and agent-done-p (not (null tool-list)))))
    (let ((section-start (point)))
      (magit-insert-section (tool item collapsed)
        (magit-insert-heading
          (if desc
              (format "%s🤖 %s %s\n%s%s%s"
                      (claude-gravity--indent) indicator
                      (propertize desc 'face 'claude-gravity-tool-description)
                      (claude-gravity--indent 2)
                      (propertize (claude-gravity--tool-signature name input)
                                  'face 'claude-gravity-tool-signature)
                      agent-suffix)
            (format "%s🤖 %s %s  %s%s"
                    (claude-gravity--indent) indicator
                    (propertize (or name "?") 'face 'claude-gravity-tool-name)
                    (propertize (claude-gravity--tool-summary name input)
                                'face 'claude-gravity-detail-label)
                    agent-suffix)))
        ;; Render agent's tools as response cycles with agent-specific styling
        (let ((claude-gravity--margin-char "┃")
              (claude-gravity--margin-face 'claude-gravity-agent-margin)
              (claude-gravity--agent-depth (1+ (or depth claude-gravity--agent-depth 0)))
              (body-start (point)))
          (when tool-list
            (claude-gravity--dedup-assistant-text tool-list)
            ;; Build nested agent lookup from agents that have the same turn
            ;; and were spawned within this agent's tools
            (let* ((nested-agents (claude-gravity--collect-nested-agents
                                   agent tool-list (plist-get
                                                    (claude-gravity--get-session
                                                     (when (boundp 'claude-gravity--buffer-session-id)
                                                       claude-gravity--buffer-session-id))
                                                    :agents)))
                   (nested-lookup (claude-gravity--build-agent-lookup nested-agents))
                   (cycles (claude-gravity--group-response-cycles tool-list))
                   (n-cycles (length cycles))
                   (cycle-idx 0))
              (dolist (cycle cycles)
                (let* ((is-last (= cycle-idx (1- n-cycles)))
                       (all-done (cl-every (lambda (ti) (equal (alist-get 'status ti) "done")) cycle))
                       (should-collapse (and (not is-last) all-done))
                       (first-item (car cycle))
                       (athink (alist-get 'assistant_thinking first-item))
                       (atext (alist-get 'assistant_text first-item))
                       (athink (when athink (string-trim athink)))
                       (atext (when atext (string-trim atext)))
                       (split (when (and atext (not (string-empty-p atext)))
                                (claude-gravity--split-margin-text atext 'claude-gravity-assistant-text)))
                       (tools-label (format "%d tool%s" (length cycle)
                                            (if (= (length cycle) 1) "" "s"))))
                  ;; Thinking stays outside the section (always visible)
                  (when (and athink (not (string-empty-p athink)))
                    (let* ((indent (or (* (claude-gravity--section-depth) claude-gravity--indent-step) 0))
                           (margin (propertize (concat claude-gravity--margin-char " ")
                                              'face claude-gravity--margin-face))
                           (prefix (concat (make-string indent ?\s) margin)))
                      (insert prefix (propertize "Thinking..." 'face 'claude-gravity-thinking) "\n")
                      (claude-gravity--insert-wrapped athink (+ indent 4) 'claude-gravity-thinking)))
                  ;; Response cycle section — heading is first line of assistant text
                  (magit-insert-section (response-cycle cycle-idx should-collapse)
                    (magit-insert-heading
                      (if split
                          (car split)
                        (format "%s%s%s"
                                (claude-gravity--indent)
                                (propertize (concat claude-gravity--margin-char " ")
                                            'face claude-gravity--margin-face)
                                (propertize tools-label 'face 'claude-gravity-detail-label))))
                    ;; Body: remaining assistant text lines (visible when collapsed)
                    (when (cdr split)
                      (insert (cdr split))
                      ;; Move content marker past assistant text so it stays
                      ;; visible when the section is collapsed
                      (oset magit-insert-section--current content
                            (if magit-section-inhibit-markers (point) (point-marker))))
                    ;; Body: tool count summary (when heading was assistant text)
                    (when split
                      (insert (format "%s%s%s\n"
                                      (claude-gravity--indent)
                                      (propertize (concat claude-gravity--margin-char " ")
                                                  'face claude-gravity--margin-face)
                                      (propertize tools-label 'face 'claude-gravity-detail-label))))
                    (claude-gravity--insert-tool-item first-item nested-lookup)
                    (dolist (ti (cdr cycle))
                      (claude-gravity--insert-tool-context ti)
                      (claude-gravity--insert-tool-item ti nested-lookup)))
                  (unless is-last
                    (claude-gravity--turn-separator)))
                (cl-incf cycle-idx))))
          ;; Agent's trailing summary text (final result returned to parent)
          (let ((agent-stop-think (alist-get 'stop_thinking agent))
                (agent-stop-text (alist-get 'stop_text agent)))
            (when (and agent-stop-think (stringp agent-stop-think)
                       (not (string-empty-p agent-stop-think)))
              (claude-gravity--insert-wrapped-with-margin
               agent-stop-think nil 'claude-gravity-thinking))
            (when (and agent-stop-text (stringp agent-stop-text)
                       (not (string-empty-p agent-stop-text)))
              (claude-gravity--insert-wrapped-with-margin
               agent-stop-text nil 'claude-gravity-assistant-text)))
          ;; If no agent tools yet but agent is running, show status
          (when (and (not tool-list) (not agent-done-p))
            (insert (format "%s%s\n"
                            (claude-gravity--indent)
                            (propertize "Agent running..." 'face 'claude-gravity-detail-label))))
          ;; Apply agent background tint to body (after heading)
          (add-face-text-property body-start (point)
                                  (if (> claude-gravity--agent-depth 1)
                                      'claude-gravity-agent-nested-bg
                                    'claude-gravity-agent-bg))))
      ;; Apply background tint to running tools (stacks with agent-bg)
      (unless done-p
        (add-face-text-property section-start (point) 'claude-gravity-running-bg)))))

(defun claude-gravity--collect-nested-agents (parent-agent parent-tools all-agents)
  "Collect agents that are nested within PARENT-AGENT.
PARENT-TOOLS are the tools belonging to the parent agent.
ALL-AGENTS is the session's full agents vector.
Returns a list of agents whose turn:type matches a Task tool in parent-tools."
  (let ((task-keys (make-hash-table :test 'equal))
        (result nil))
    ;; Collect turn:subagent_type keys from Task tool calls in parent's tools
    (dolist (tool parent-tools)
      (when (equal (alist-get 'name tool) "Task")
        (let* ((turn (or (alist-get 'turn tool) 0))
               (stype (or (alist-get 'subagent_type (alist-get 'input tool)) ""))
               (key (format "%d:%s" turn stype)))
          (puthash key t task-keys))))
    ;; Only include agents whose turn:type matches one of the parent's Task keys
    (when all-agents
      (dolist (agent (append all-agents nil))
        (let ((atools (alist-get 'agent-tools agent)))
          (when (and atools (> (length atools) 0)
                     (not (equal (alist-get 'agent_id agent)
                                 (alist-get 'agent_id parent-agent))))
            (let* ((aturn (or (alist-get 'turn agent) 0))
                   (atype (or (alist-get 'type agent) ""))
                   (key (format "%d:%s" aturn atype)))
              (when (gethash key task-keys)
                (push agent result)))))))
    (nreverse result)))

(defun claude-gravity--insert-tool-item (item &optional agent-lookup)
  "Insert a single tool ITEM as a magit-section.
When AGENT-LOOKUP is provided and ITEM is a Task tool, annotate with agent info."
  (let* ((name (alist-get 'name item))
         (status (alist-get 'status item))
         (input (alist-get 'input item))
         (result (alist-get 'result item))
         (done-p (equal status "done"))
         (error-p (equal status "error"))
         (indicator (propertize (cond (error-p "[!]")
                                      (done-p "[x]")
                                      (t "[/]"))
                                'face (cond (error-p 'claude-gravity-tool-error)
                                            (done-p 'claude-gravity-tool-done)
                                            (t 'claude-gravity-tool-running))))
         (tool-face (propertize (or name "?") 'face 'claude-gravity-tool-name))
         (summary (claude-gravity--tool-summary name input))
         (desc (claude-gravity--tool-description input))
         ;; Agent annotation for Task tools
         (agent (when (and agent-lookup (equal name "Task"))
                  (claude-gravity--pop-matching-agent
                   agent-lookup
                   (alist-get 'turn item)
                   (alist-get 'subagent_type input)))))
    ;; If agent has nested tools, render as sub-branch instead
    (if (and agent (let ((at (alist-get 'agent-tools agent)))
                     (and at (> (length at) 0))))
        (claude-gravity--insert-agent-branch item agent agent-lookup)
      (let* ((agent-suffix
              (if agent
                  (let* ((atype (alist-get 'type agent))
                         (aid (alist-get 'agent_id agent))
                         (short-id (if (and aid (> (length aid) 7))
                                       (substring aid 0 7)
                                     (or aid "?")))
                         (adur (alist-get 'duration agent))
                         (astatus (alist-get 'status agent))
                         (dur-str (if (and (equal astatus "done") adur)
                                      (format "  %s" (claude-gravity--format-duration adur))
                                    "")))
                    (format "  %s %s (%s)%s"
                            (propertize "→" 'face 'claude-gravity-detail-label)
                            (propertize (or atype "?") 'face 'claude-gravity-tool-name)
                            (propertize short-id 'face 'claude-gravity-detail-label)
                            dur-str))
                ""))
             (agent-icon (if agent "🤖 " ""))
             (section-start (point)))
      (magit-insert-section (tool item t)
        (magit-insert-heading
          (if desc
              (format "%s%s%s %s\n%s%s%s"
                      (claude-gravity--indent)
                      agent-icon
                      indicator
                      (propertize desc 'face 'claude-gravity-tool-description)
                      (claude-gravity--indent 2)
                      (propertize (claude-gravity--tool-signature name input)
                                  'face 'claude-gravity-tool-signature)
                      agent-suffix)
            (format "%s%s%s %s  %s%s"
                    (claude-gravity--indent)
                    agent-icon
                    indicator
                    tool-face
                    (propertize summary 'face 'claude-gravity-detail-label)
                    agent-suffix)))
        ;; Show permission-format signature in detail (only for single-line tools)
        (unless desc
          (let ((sig (claude-gravity--tool-signature name input)))
            (claude-gravity--insert-wrapped sig nil 'claude-gravity-tool-signature)))
        (insert "\n")
        (claude-gravity--insert-tool-detail name input result)
        ;; Post-tool text and thinking (text generated after this tool completes)
        (let ((post-think (alist-get 'post_thinking item))
              (post-text (alist-get 'post_text item)))
          (when (and post-think (not (string-empty-p post-think)))
            (claude-gravity--insert-wrapped-with-margin post-think nil 'claude-gravity-thinking)
            (insert "\n"))
          (when (and post-text (not (string-empty-p post-text)))
            (claude-gravity--insert-wrapped-with-margin post-text nil 'claude-gravity-assistant-text)
            (insert "\n")))
        ;; Agent detail (transcript, model, etc.) in expanded view
        (when agent
          (let ((tp (alist-get 'transcript_path agent))
                (parsed (alist-get 'transcript_parsed agent)))
            (when parsed
              (let ((prompt (alist-get 'transcript_prompt agent))
                    (model (alist-get 'transcript_model agent))
                    (tc (alist-get 'transcript_tool_count agent)))
                (when (and prompt (not (string-empty-p prompt)))
                  (claude-gravity--insert-label "Agent task: ")
                  (claude-gravity--insert-wrapped prompt nil))
                (when (and model (not (string-empty-p model)))
                  (claude-gravity--insert-label "Model: ")
                  (insert model "\n"))
                (when (and tc (> tc 0))
                  (claude-gravity--insert-label "Agent tools: ")
                  (insert (format "%d" tc) "\n"))))
            (when tp
              (claude-gravity--insert-label "Transcript: ")
              (claude-gravity--insert-wrapped tp nil 'claude-gravity-detail-label)))))
      ;; Apply background tint to running tools
      (unless done-p
        (add-face-text-property section-start (point) 'claude-gravity-running-bg))))))


(defun claude-gravity--task-sort-key (status)
  "Return sort key for task STATUS.  Lower = higher priority."
  (pcase status
    ("in_progress" 0)
    ("pending" 1)
    ("completed" 2)
    (_ 3)))

(defun claude-gravity--strip-system-xml (text)
  "Strip system-injected XML tags from TEXT.
Claude Code injects tags like <system-reminder>, <task-notification>,
<local-command-caveat>, <command-name>, <command-message>, <command-args>,
<local-command-stdout> into user prompt text.  Remove them and any
surrounding whitespace so the UI shows only the actual user prompt."
  (if (and text (string-match-p "<" text))
      (let ((result text)
            (tag-re "system-reminder\\|task-notification\\|local-command-caveat\\|command-name\\|command-message\\|command-args\\|local-command-stdout"))
        ;; Remove matched pairs: <tag ...>content</tag>
        ;; Use \\(?:.\\|\n\\)*? for multi-line non-greedy match
        (setq result (replace-regexp-in-string
                      (concat "<\\(" tag-re "\\)[^>]*>"
                              "\\(?:.\\|\n\\)*?"
                              "</\\1>")
                      "" result))
        ;; Remove any remaining unpaired/self-closing tags
        (setq result (replace-regexp-in-string
                      (concat "</?\\(" tag-re "\\)[^>]*>")
                      "" result))
        ;; Collapse excessive blank lines left behind
        (setq result (replace-regexp-in-string "\n\\{3,\\}" "\n\n" result))
        (setq result (string-trim result))
        (if (string-empty-p result) nil result))
    text))

(defun claude-gravity--prompt-text (prompt-entry)
  "Extract text from PROMPT-ENTRY (alist or legacy string)."
  (if (listp prompt-entry)
      (or (alist-get 'text prompt-entry) "")
    (or prompt-entry "")))


(defun claude-gravity--parse-agent-transcript (path)
  "Parse agent transcript JSONL at PATH.
Returns alist with prompt, model, and tool-count.
The JSONL format uses top-level `type` (user/assistant) and nests
the message under `message` with `role`, `content`, and `model`."
  (when (and path (file-exists-p path))
    (condition-case err
        (let ((prompt nil)
              (model nil)
              (tool-count 0))
          (with-temp-buffer
            (insert-file-contents path)
            (goto-char (point-min))
            (while (not (eobp))
              (let ((line (buffer-substring-no-properties
                           (line-beginning-position) (line-end-position))))
                (when (> (length line) 0)
                  (condition-case nil
                      (let* ((json-object-type 'alist)
                             (json-array-type 'vector)
                             (obj (json-read-from-string line))
                             (entry-type (alist-get 'type obj))
                             (msg (alist-get 'message obj))
                             (content (when msg (alist-get 'content msg))))
                        ;; Extract prompt from first user entry
                        (when (and (equal entry-type "user") (not prompt))
                          (cond
                           ((and msg (stringp content))
                            (setq prompt (car (split-string content "\n" t))))
                           ((and msg (vectorp content))
                            (dotimes (i (length content))
                              (let ((block (aref content i)))
                                (when (and (not prompt)
                                           (equal (alist-get 'type block) "text"))
                                  (setq prompt (car (split-string
                                                     (alist-get 'text block) "\n" t)))))))))
                        ;; Extract model from first assistant entry
                        (when (and (equal entry-type "assistant") msg (not model))
                          (setq model (alist-get 'model msg)))
                        ;; Count tool_use blocks in assistant messages
                        (when (and (equal entry-type "assistant") msg (vectorp content))
                          (dotimes (i (length content))
                            (let ((block (aref content i)))
                              (when (equal (alist-get 'type block) "tool_use")
                                (cl-incf tool-count))))))
                    (error nil))))
              (forward-line 1)))
          (list (cons 'prompt (or prompt ""))
                (cons 'model (or model ""))
                (cons 'tool-count tool-count)))
      (error
       (message "Claude Gravity: failed to parse transcript %s: %s" path err)
       nil))))

(defun claude-gravity-view-agent-transcript ()
  "Parse and display transcript for the agent at point."
  (interactive)
  (let ((section (magit-current-section)))
    (when section
      (let ((val (oref section value)))
        (when (and val (listp val) (alist-get 'agent_id val))
          (let ((tp (alist-get 'transcript_path val))
                (agent-id (alist-get 'agent_id val)))
            (if (not tp)
                (message "No transcript path for this agent")
              (if (alist-get 'transcript_parsed val)
                  ;; Already parsed, just refresh
                  (claude-gravity-refresh)
                ;; Parse and store into the session's agent alist directly.
                ;; We must look up the agent in session data because setf on
                ;; alist-get with new keys only rebinds the local variable.
                (let* ((session (claude-gravity--get-session
                                 claude-gravity--buffer-session-id))
                       (agents (when session (plist-get session :agents)))
                       (idx (when agents
                              (claude-gravity--find-agent-by-id agents agent-id)))
                       (agent (when idx (aref agents idx)))
                       (info (claude-gravity--parse-agent-transcript tp)))
                  (when (and agent info)
                    (setf (alist-get 'transcript_prompt agent)
                          (alist-get 'prompt info))
                    (setf (alist-get 'transcript_model agent)
                          (alist-get 'model info))
                    (setf (alist-get 'transcript_tool_count agent)
                          (alist-get 'tool-count info))
                    (setf (alist-get 'transcript_parsed agent) t)
                    (aset agents idx agent))
                  (claude-gravity-refresh))))))))))


(defun claude-gravity-open-agent-transcript ()
  "Open the raw transcript JSONL file for the agent at point."
  (interactive)
  (let ((section (magit-current-section)))
    (when section
      (let ((val (oref section value)))
        (when (and val (listp val) (alist-get 'agent_id val))
          (let ((tp (alist-get 'transcript_path val)))
            (if (and tp (file-exists-p tp))
                (find-file tp)
              (message "No transcript file available"))))))))


(defun claude-gravity--insert-agent-item (agent)
  "Insert a single AGENT as a magit-section with expandable detail."
  (let* ((agent-type (alist-get 'type agent))
         (agent-id (alist-get 'agent_id agent))
         (status (alist-get 'status agent))
         (duration (alist-get 'duration agent))
         (done-p (equal status "done"))
         (indicator (propertize (if done-p "[x]" "[/]")
                                'face (if done-p
                                           'claude-gravity-tool-done
                                         'claude-gravity-tool-running)))
         (short-id (if (and agent-id (> (length agent-id) 7))
                       (substring agent-id 0 7)
                     (or agent-id "?")))
         (duration-str (if (and done-p duration)
                           (format "  %s" (claude-gravity--format-duration duration))
                         "")))
    (let ((section-start (point)))
      (magit-insert-section (agent agent t)
        (magit-insert-heading
          (format "%s🤖 %s %s  (%s)%s"
                  (claude-gravity--indent)
                  indicator
                  (propertize (or agent-type "?") 'face 'claude-gravity-tool-name)
                  (propertize short-id 'face 'claude-gravity-detail-label)
                  duration-str))
        ;; Expanded detail
        (let ((tp (alist-get 'transcript_path agent))
              (parsed (alist-get 'transcript_parsed agent)))
          (when parsed
            (let ((prompt (alist-get 'transcript_prompt agent))
                  (model (alist-get 'transcript_model agent))
                  (tc (alist-get 'transcript_tool_count agent)))
              (when (and prompt (not (string-empty-p prompt)))
                (claude-gravity--insert-label "Task: ")
                (claude-gravity--insert-wrapped prompt nil))
              (when (and model (not (string-empty-p model)))
                (claude-gravity--insert-label "Model: ")
                (insert model "\n"))
              (when (and tc (> tc 0))
                (claude-gravity--insert-label "Tools: ")
                (insert (format "%d" tc) "\n"))))
          (when tp
            (claude-gravity--insert-label "Transcript: ")
            (claude-gravity--insert-wrapped tp nil 'claude-gravity-detail-label))
          (unless parsed
            (when tp
              (claude-gravity--insert-label "RET to parse transcript\n")))))
      ;; Apply background tint to running agents
      (unless done-p
        (add-face-text-property section-start (point) 'claude-gravity-running-bg)))))

(defun claude-gravity--format-duration (seconds)
  "Format SECONDS as a compact duration string like 12.3s or 1m23s."
  (cond
   ((< seconds 60) (format "%.1fs" seconds))
   ((< seconds 3600) (format "%dm%02ds" (truncate (/ seconds 60))
                              (truncate (mod seconds 60))))
   (t (format "%dh%02dm" (truncate (/ seconds 3600))
              (truncate (/ (mod seconds 3600) 60))))))

(defun claude-gravity-insert-files (session)
  "Insert files section for SESSION."
  (let ((files-ht (plist-get session :files)))
    (when (and files-ht (> (hash-table-count files-ht) 0))
      (let ((file-list nil))
        ;; Collect into list for sorting
        (maphash (lambda (path entry)
                   (push (list path
                               (alist-get 'ops entry)
                               (alist-get 'last-touched entry))
                         file-list))
                 files-ht)
        ;; Sort by last-touched, most recent first
        (setq file-list (sort file-list
                              (lambda (a b)
                                (time-less-p (nth 2 b) (nth 2 a)))))
        (magit-insert-section (files nil t)
          (magit-insert-heading
            (claude-gravity--section-divider (format "Files (%d)" (length file-list))))
          (dolist (entry file-list)
            (let* ((path (nth 0 entry))
                   (ops (nth 1 entry))
                   (basename (file-name-nondirectory path))
                   (ops-str (string-join (reverse ops) ", ")))
              (magit-insert-section (file-entry path t)
                (magit-insert-heading
                  (format "%s%-30s %s"
                          (claude-gravity--indent)
                          (propertize basename 'face 'claude-gravity-tool-name)
                          (propertize ops-str 'face 'claude-gravity-file-ops)))
                (claude-gravity--insert-wrapped path nil 'claude-gravity-detail-label))))
          (insert "\n"))))))

(defun claude-gravity-insert-allow-patterns (session)
  "Insert allow patterns section for SESSION."
  (let ((patterns (plist-get session :allow-patterns)))
    (when patterns
      (magit-insert-section (allow-patterns nil t)
        (magit-insert-heading
          (claude-gravity--section-divider (format "Allow Patterns (%d)" (length patterns))))
        (dolist (pat patterns)
          (magit-insert-section (allow-pattern pat)
            (insert (format "%s%s\n" (claude-gravity--indent) (propertize pat 'face 'claude-gravity-detail-label)))))
        (insert "\n")))))

;;; Overview Buffer

(defun claude-gravity--render-overview ()
  "Render the overview buffer with all sessions grouped by project."
  (let ((buf (get-buffer claude-gravity-buffer-name)))
    (when buf
      (with-current-buffer buf
        (let ((inhibit-read-only t)
              (pos (point))
              (projects (make-hash-table :test 'equal)))
          ;; Group sessions by project
          (maphash (lambda (_id session)
                     (let ((proj (plist-get session :project)))
                       (puthash proj
                                (cons session (gethash proj projects nil))
                                projects)))
                   claude-gravity--sessions)
          (erase-buffer)
          (magit-insert-section (root)
            (let* ((total-count (hash-table-count claude-gravity--sessions))
                   (width (max 40 (- (or (window-width) 80) 2)))
                   (top-line (make-string width ?━)))
              (magit-insert-section (header)
                (insert (propertize top-line 'face 'claude-gravity-divider) "\n")
                (magit-insert-heading
                  (format "%s%s"
                          (propertize "Structured Claude Sessions" 'face 'claude-gravity-header-title)
                          (propertize (format "  ◆ %d sessions" total-count) 'face 'claude-gravity-detail-label)))
                (insert (propertize top-line 'face 'claude-gravity-divider) "\n\n")))
            ;; Inbox: Attention Required (permission, question, plan-review)
            (let ((attention (cl-remove-if
                              (lambda (item) (eq (alist-get 'type item) 'idle))
                              claude-gravity--inbox)))
              (when attention
                (magit-insert-section (inbox-attention nil t)
                  (magit-insert-heading
                    (propertize (format "Attention Required (%d)" (length attention))
                                'face 'claude-gravity-question))
                  (dolist (item attention)
                    (claude-gravity--insert-inbox-item item)))
                (insert "\n")))
            ;; Inbox: Waiting for Input (idle sessions)
            (let ((waiting (cl-remove-if-not
                            (lambda (item) (eq (alist-get 'type item) 'idle))
                            claude-gravity--inbox)))
              (when waiting
                (magit-insert-section (inbox-waiting nil t)
                  (magit-insert-heading
                    (propertize (format "Waiting for Input (%d)" (length waiting))
                                'face 'claude-gravity-status-idle))
                  (dolist (item waiting)
                    (claude-gravity--insert-inbox-item item)))
                (insert "\n")))
            (if (= (hash-table-count claude-gravity--sessions) 0)
                (insert (propertize "  No sessions.\n" 'face 'claude-gravity-detail-label))  ;; static text, not a section
              (maphash
               (lambda (proj-name sessions)
                 (magit-insert-section (project proj-name t)
                   (magit-insert-heading
                     (format "%s (%d)" proj-name (length sessions)))
                   (dolist (session (sort sessions
                                         (lambda (a b)
                                           (time-less-p (plist-get b :start-time)
                                                        (plist-get a :start-time)))))
                     (let* ((sid (plist-get session :session-id))
                            (label (claude-gravity--session-label session))
                            (status (plist-get session :status))
                            (claude-st (plist-get session :claude-status))
                            (tools (alist-get 'tools (plist-get session :state)))
                            (n-tools (length tools))
                            (indicator (if (eq status 'active)
                                           (propertize "●" 'face 'claude-gravity-tool-running)
                                         (propertize "○" 'face 'claude-gravity-session-ended)))
                            (last-event (plist-get session :last-event-time))
                            (idle-time (when (and last-event (eq claude-st 'idle))
                                         (float-time (time-subtract (current-time) last-event))))
                            (idle-str (when idle-time
                                        (cond
                                         ((< idle-time 60) "")
                                         ((< idle-time 3600) (format " %dm" (truncate (/ idle-time 60))))
                                         (t (format " %dh" (truncate (/ idle-time 3600)))))))
                            (status-label
                             (when (eq status 'active)
                               (if (eq claude-st 'responding)
                                   (propertize "responding" 'face 'claude-gravity-status-responding)
                                 (propertize (concat "idle" (or idle-str ""))
                                             'face 'claude-gravity-status-idle)))))
                       (let ((tmux-badge (if (gethash sid claude-gravity--tmux-sessions)
                                                (propertize " [tmux]" 'face 'claude-gravity-detail-label)
                                              "")))
                         (magit-insert-section (session-entry sid)
                           (magit-insert-heading
                             (format "%s%s%s %s  %s  [%d tools]"
                                     (claude-gravity--indent)
                                     indicator tmux-badge label
                                     (or status-label "")
                                     n-tools))))))))
               projects)))
          (goto-char (min pos (point-max)))
          (claude-gravity--apply-visibility))))))

(defun claude-gravity--insert-inbox-item (item)
  "Render a single inbox ITEM as a magit-section line."
  (let* ((id (alist-get 'id item))
         (type (alist-get 'type item))
         (project (or (alist-get 'project item) "?"))
         (summary (or (alist-get 'summary item) ""))
         (timestamp (alist-get 'timestamp item))
         (icon (pcase type
                 ('permission "!")
                 ('question "?")
                 ('plan-review "P")
                 ('idle ".")
                 (_ " ")))
         (icon-face (pcase type
                      ('permission 'claude-gravity-question)
                      ('question 'claude-gravity-status-responding)
                      ('plan-review 'claude-gravity-question)
                      ('idle 'claude-gravity-status-idle)
                      (_ 'default)))
         (age (when timestamp
                (let ((secs (float-time (time-subtract (current-time) timestamp))))
                  (cond
                   ((< secs 60) "<1m")
                   ((< secs 3600) (format "%dm" (truncate (/ secs 60))))
                   (t (format "%dh" (truncate (/ secs 3600)))))))))
    (magit-insert-section (inbox-item id)
      (magit-insert-heading
        (format "  %s [%s] %-60s %s"
                (propertize icon 'face icon-face)
                (propertize project 'face 'claude-gravity-detail-label)
                summary
                (propertize (or age "") 'face 'claude-gravity-detail-label))))))

(defun claude-gravity-inbox-dismiss ()
  "Dismiss the inbox item at point.
Only idle items can be dismissed.  Bidirectional items need a response."
  (interactive)
  (let ((section (magit-current-section)))
    (if (and section (eq (oref section type) 'inbox-item))
        (let* ((item-id (oref section value))
               (item (claude-gravity--inbox-find item-id)))
          (if (null item)
              (message "Inbox item not found")
            (if (eq (alist-get 'type item) 'idle)
                (progn
                  (claude-gravity--inbox-remove item-id)
                  (message "Dismissed"))
              (message "Cannot dismiss — this item needs a response (use RET to act on it)"))))
      (message "No inbox item at point"))))

;;; Per-Session Buffer

(defun claude-gravity--session-buffer-name (session)
  "Return buffer name for SESSION."
  (format "*Structured Claude Session %s*" (claude-gravity--session-label session)))

(defun claude-gravity-open-session (session-id)
  "Open or switch to the buffer for SESSION-ID."
  (interactive)
  (let* ((session (claude-gravity--get-session session-id))
         (buf-name (claude-gravity--session-buffer-name session))
         (existing (get-buffer buf-name)))
    (if existing
        (pop-to-buffer existing)
      (with-current-buffer (get-buffer-create buf-name)
        (claude-gravity-session-mode)
        (setq claude-gravity--buffer-session-id session-id)
        (plist-put session :buffer (current-buffer))
        (claude-gravity--render-session-buffer session)
        (pop-to-buffer (current-buffer))))))

(defun claude-gravity--apply-visibility ()
  "Apply overlay-based hiding for sections marked hidden.
magit-section caches visibility but relies on paint hooks to apply
overlays.  Since we render from timers, we must apply them manually."
  (when magit-root-section
    (cl-labels ((walk (section)
                  (when (oref section hidden)
                    (magit-section-hide section))
                  (dolist (child (oref section children))
                    (walk child))))
      (walk magit-root-section))))

(defun claude-gravity--render-session-buffer (session)
  "Render the magit-section UI for SESSION into its buffer."
  (let* ((state (plist-get session :state))
         (buf (or (let ((b (plist-get session :buffer)))
                    (and b (buffer-live-p b) b))
                  (get-buffer (claude-gravity--session-buffer-name session)))))
    (when buf
      (with-current-buffer buf
        (let ((inhibit-read-only t)
              (pos (point)))
          (erase-buffer)
          (magit-insert-section (root)
            (claude-gravity-insert-header session)
            (claude-gravity-insert-plan session)
            (claude-gravity-insert-streaming-text session)
            (claude-gravity-insert-turns session)
            (claude-gravity-insert-files session)
            (claude-gravity-insert-allow-patterns session))
          (goto-char (min pos (point-max)))
          (claude-gravity--apply-visibility))))))

;;; Modes

(defvar claude-gravity-mode-map (make-sparse-keymap)
  "Keymap for `claude-gravity-mode'.")
(set-keymap-parent claude-gravity-mode-map magit-section-mode-map)
(define-key claude-gravity-mode-map (kbd "g") 'claude-gravity-refresh)
(define-key claude-gravity-mode-map (kbd "c") 'claude-gravity-comment-at-point)
(define-key claude-gravity-mode-map (kbd "P") 'claude-gravity-show-plan)
(define-key claude-gravity-mode-map (kbd "?") 'claude-gravity-menu)
(define-key claude-gravity-mode-map (kbd "TAB") 'magit-section-toggle)
(define-key claude-gravity-mode-map (kbd "<return>") 'claude-gravity-visit-or-toggle)
(define-key claude-gravity-mode-map (kbd "D") 'claude-gravity-cleanup-sessions)
(define-key claude-gravity-mode-map (kbd "R") 'claude-gravity-reset-status)
(define-key claude-gravity-mode-map (kbd "X") 'claude-gravity-detect-dead-sessions)
(define-key claude-gravity-mode-map (kbd "d") 'claude-gravity-delete-session)
(define-key claude-gravity-mode-map (kbd "A") 'claude-gravity-add-allow-pattern)
(define-key claude-gravity-mode-map (kbd "a") 'claude-gravity-add-allow-pattern-to-settings)
(define-key claude-gravity-mode-map (kbd "F") 'claude-gravity-open-plan-file)
(define-key claude-gravity-mode-map (kbd "t") 'claude-gravity-tail)
(define-key claude-gravity-mode-map (kbd "f") 'claude-gravity-follow-mode)
(define-key claude-gravity-mode-map (kbd "k") 'claude-gravity-inbox-dismiss)

(define-derived-mode claude-gravity-mode magit-section-mode "StructuredSessions"
  "Major mode for Structured Claude Sessions overview.

\\{claude-gravity-mode-map}"
  (font-lock-mode -1)
  (visual-line-mode 1))

(define-key claude-gravity-mode-map (kbd "T") 'claude-gravity-view-agent-transcript)
(define-key claude-gravity-mode-map (kbd "V") 'claude-gravity-open-agent-transcript)

(define-derived-mode claude-gravity-session-mode claude-gravity-mode "StructuredSession"
  "Major mode for a single Structured Claude Session buffer."
  (setq mode-name '(:eval (if claude-gravity--follow-mode
                              "StructuredSession[F]"
                            "StructuredSession"))))

(defun claude-gravity-visit-or-toggle ()
  "If on a session entry, open it.  On an inbox item, act on it.
On an agent, parse transcript.  Otherwise toggle."
  (interactive)
  (let ((section (magit-current-section)))
    (cond
     ((and section (eq (oref section type) 'inbox-item))
      (let* ((item-id (oref section value))
             (item (claude-gravity--inbox-find item-id)))
        (when item
          (pcase (alist-get 'type item)
            ('permission (claude-gravity--inbox-act-permission item))
            ('question (claude-gravity--inbox-act-question item))
            ('plan-review (claude-gravity--inbox-act-plan-review item))
            ('idle (claude-gravity--inbox-act-idle item))))))
     ((and section (eq (oref section type) 'session-entry))
      (claude-gravity-open-session (oref section value)))
     ((and section (eq (oref section type) 'agent)
           (let ((val (oref section value)))
             (and val (listp val) (alist-get 'agent_id val))))
      (claude-gravity-view-agent-transcript))
     (t (magit-section-toggle section)))))

(defun claude-gravity-refresh ()
  "Refresh the current buffer."
  (interactive)
  (if claude-gravity--buffer-session-id
      ;; Per-session buffer
      (let ((session (claude-gravity--get-session claude-gravity--buffer-session-id)))
        (when session
          (claude-gravity--render-session-buffer session)))
    ;; Overview buffer
    (claude-gravity--render-overview)))

;;; Permission pattern commands

(defun claude-gravity--suggest-patterns (name input)
  "Generate candidate allow patterns for tool NAME with INPUT.
Returns a list from most specific to most general, with nils removed."
  (delq nil
        (pcase name
          ("Bash"
           (let* ((cmd (or (alist-get 'command input) ""))
                  (parts (split-string cmd " " t))
                  (prog (car parts)))
             (list (format "Bash(%s)" cmd)
                   (when (> (length cmd) 0) (format "Bash(%s:*)" cmd))
                   (when prog (format "Bash(%s:*)" prog)))))
          ((or "Edit" "Write")
           (let* ((path (or (alist-get 'file_path input) ""))
                  (dir (file-name-directory path)))
             (list (format "%s(%s)" name path)
                   (when dir (format "%s(%s*)" name dir)))))
          ("Read"
           (let* ((path (or (alist-get 'file_path input) ""))
                  (dir (file-name-directory path)))
             (list (format "Read(%s)" path)
                   (when dir (format "Read(%s*)" dir)))))
          ("WebFetch"
           (let* ((url (or (alist-get 'url input) ""))
                  (host (when (string-match "https?://\\([^/]+\\)" url)
                          (match-string 1 url))))
             (list (when host (format "WebFetch(domain:%s)" host))
                   "WebFetch")))
          ((or "Grep" "Glob")
           (list (format "%s(%s)" name (or (alist-get 'pattern input) ""))
                 name))
          (_
           (list name)))))

(defun claude-gravity--tool-item-at-point ()
  "Return the tool item alist at point, or nil."
  (let ((section (magit-current-section)))
    (when section
      (let ((val (oref section value)))
        (when (and val (listp val) (alist-get 'name val))
          val)))))

(defun claude-gravity-add-allow-pattern ()
  "Generate allow pattern suggestions for the tool at point and copy to kill ring."
  (interactive)
  (let ((item (claude-gravity--tool-item-at-point)))
    (if (not item)
        (message "No tool at point")
      (let* ((name (alist-get 'name item))
             (input (alist-get 'input item))
             (suggestions (claude-gravity--suggest-patterns name input)))
        (if (not suggestions)
            (message "No pattern suggestions for %s" name)
          (let ((chosen (completing-read "Allow pattern: " suggestions nil nil
                                         (car suggestions))))
            (kill-new chosen)
            (message "Copied: %s" chosen)))))))

(defun claude-gravity-add-allow-pattern-to-settings ()
  "Add an allow pattern for the tool at point to settings.local.json."
  (interactive)
  (let ((item (claude-gravity--tool-item-at-point)))
    (if (not item)
        (message "No tool at point")
      (let* ((sid (or claude-gravity--buffer-session-id ""))
             (session (claude-gravity--get-session sid)))
        (if (not session)
            (message "No session found")
          (let* ((name (alist-get 'name item))
                 (input (alist-get 'input item))
                 (suggestions (claude-gravity--suggest-patterns name input)))
            (if (not suggestions)
                (message "No pattern suggestions for %s" name)
              (let* ((chosen (completing-read "Allow pattern to add: " suggestions nil nil
                                              (car suggestions)))
                     (cwd (plist-get session :cwd))
                     (settings-path (expand-file-name ".claude/settings.local.json" cwd)))
                (when (y-or-n-p (format "Add \"%s\" to %s? " chosen
                                        (file-name-nondirectory settings-path)))
                  (let* ((json-object-type 'alist)
                         (json-array-type 'list)
                         (data (if (file-exists-p settings-path)
                                   (json-read-file settings-path)
                                 (list (cons 'permissions (list (cons 'allow nil))))))
                         (perms (or (alist-get 'permissions data)
                                    (list (cons 'allow nil))))
                         (allow (or (alist-get 'allow perms) nil)))
                    (if (member chosen allow)
                        (message "Pattern already exists: %s" chosen)
                      (setf (alist-get 'allow perms) (append allow (list chosen)))
                      (setf (alist-get 'permissions data) perms)
                      (let ((dir (file-name-directory settings-path)))
                        (unless (file-exists-p dir)
                          (make-directory dir t)))
                      (with-temp-file settings-path
                        (let ((json-encoding-pretty-print t))
                          (insert (json-encode data))))
                      (claude-gravity--load-allow-patterns session)
                      (claude-gravity-refresh)
                      (message "Added: %s" chosen))))))))))))

;;; Commands

;;;###autoload (autoload 'claude-gravity-menu "claude-gravity" nil t)
(transient-define-prefix claude-gravity-menu ()
  "Interactions with Claude Code."
  ["Actions"
   ("c" "Comment" claude-gravity-comment-at-point)
   ("g" "Refresh" claude-gravity-refresh)
   ("t" "Tail" claude-gravity-tail)
   ("f" "Follow mode" claude-gravity-follow-mode)
   ("P" "Show Plan" claude-gravity-show-plan)
   ("F" "Open plan file" claude-gravity-open-plan-file)
   ("T" "Parse agent transcript" claude-gravity-view-agent-transcript)
   ("V" "Open agent transcript file" claude-gravity-open-agent-transcript)]
  ["Permissions"
   ("A" "Copy allow pattern" claude-gravity-add-allow-pattern)
   ("a" "Add to settings" claude-gravity-add-allow-pattern-to-settings)]
  ["Sessions"
   ("S" "Start session (tmux)" claude-gravity-start-session)
   ("s" "Send prompt" claude-gravity-send-prompt
    :inapt-if-not claude-gravity--current-session-tmux-p)
   ("/" "Slash command" claude-gravity-slash-command
    :inapt-if-not claude-gravity--current-session-tmux-p)
   ("r" "Resume session" claude-gravity-resume-session)
   ("$" "Terminal" claude-gravity-terminal-session
    :inapt-if-not claude-gravity--current-session-tmux-p)
   ("K" "Stop session" claude-gravity-stop-session
    :inapt-if-not claude-gravity--current-session-tmux-p)
   ("D" "Remove ended sessions" claude-gravity-cleanup-sessions)
   ("R" "Reset all status to idle" claude-gravity-reset-status)
   ("X" "Detect dead sessions" claude-gravity-detect-dead-sessions)
   ("d" "Delete session" claude-gravity-delete-session)]
  ["Navigation"
   ("RET" "Visit or toggle" claude-gravity-visit-or-toggle)
   ("TAB" "Toggle section" magit-section-toggle)
   ("k" "Dismiss inbox item" claude-gravity-inbox-dismiss)])

(defun claude-gravity-cleanup-sessions ()
  "Remove all ended sessions from the registry."
  (interactive)
  (let ((to-remove nil))
    (maphash (lambda (id session)
               (when (eq (plist-get session :status) 'ended)
                 (push id to-remove)))
             claude-gravity--sessions)
    (dolist (id to-remove)
      (let ((session (gethash id claude-gravity--sessions)))
        (when session
          (let ((buf (get-buffer (claude-gravity--session-buffer-name session))))
            (when buf (kill-buffer buf)))))
      (remhash id claude-gravity--sessions))
    (message "Removed %d ended session(s)" (length to-remove))
    (claude-gravity--render-overview)))

(defun claude-gravity-reset-status ()
  "Reset claude-status to idle for all active sessions."
  (interactive)
  (let ((count 0))
    (maphash (lambda (_id session)
               (when (and (eq (plist-get session :status) 'active)
                          (eq (plist-get session :claude-status) 'responding))
                 (plist-put session :claude-status 'idle)
                 (cl-incf count)))
             claude-gravity--sessions)
    (message "Reset %d session(s) to idle" count)
    (claude-gravity--render-overview)))

(defun claude-gravity--process-alive-p (pid)
  "Return non-nil if process PID is alive."
  (condition-case nil
      (progn (signal-process pid 0) t)
    (error nil)))

(defun claude-gravity-detect-dead-sessions ()
  "Detect and mark dead sessions as ended.
Checks PID liveness when available, falls back to last-event-time staleness."
  (interactive)
  (let ((count 0)
        (dead-ids nil))
    (maphash
     (lambda (id session)
       (when (eq (plist-get session :status) 'active)
         (let ((pid (plist-get session :pid))
               (last-event (plist-get session :last-event-time)))
           (cond
            ;; PID known: check if process is alive
            ((and pid (numberp pid) (> pid 0))
             (unless (claude-gravity--process-alive-p pid)
               (plist-put session :status 'ended)
               (push id dead-ids)
               (cl-incf count)))
            ;; No PID, has last-event: use staleness (>5 min since last event)
            ((and last-event
                  (> (float-time (time-subtract (current-time) last-event)) 300))
             (plist-put session :status 'ended)
             (push id dead-ids)
             (cl-incf count))
            ;; No PID, no last-event: legacy session with no way to verify
            ((null last-event)
             (plist-put session :status 'ended)
             (push id dead-ids)
             (cl-incf count))))))
     claude-gravity--sessions)
    ;; Clean up inbox items for dead sessions
    (dolist (id dead-ids)
      (claude-gravity--inbox-remove-for-session id))
    (message "Marked %d dead session(s) as ended" count)
    (claude-gravity--render-overview)))

(defun claude-gravity-delete-session ()
  "Delete the session at point from the registry."
  (interactive)
  (let ((section (magit-current-section)))
    (when section
      (let ((sid (and (eq (oref section type) 'session-entry)
                      (oref section value))))
        (if (not sid)
            (message "No session at point")
          (let ((session (gethash sid claude-gravity--sessions)))
            (when session
              (let ((buf (get-buffer (claude-gravity--session-buffer-name session))))
                (when buf (kill-buffer buf)))))
          (remhash sid claude-gravity--sessions)
          (message "Deleted session %s" (claude-gravity--session-short-id sid))
          (claude-gravity--render-overview))))))

(defun claude-gravity-follow-mode ()
  "Toggle follow mode in the current session buffer.
When active, the buffer automatically tails after each refresh.
Disables when you manually scroll or navigate."
  (interactive)
  (setq claude-gravity--follow-mode (not claude-gravity--follow-mode))
  (if claude-gravity--follow-mode
      (progn
        (add-hook 'post-command-hook #'claude-gravity--follow-detect-manual nil t)
        (claude-gravity-tail)
        (message "Follow mode ON"))
    (remove-hook 'post-command-hook #'claude-gravity--follow-detect-manual t)
    (message "Follow mode OFF"))
  (force-mode-line-update))

(defun claude-gravity--follow-detect-manual ()
  "Disable follow mode when user scrolls or navigates manually."
  (when (and claude-gravity--follow-mode
             (memq this-command '(scroll-up-command scroll-down-command
                                  scroll-up scroll-down
                                  beginning-of-buffer end-of-buffer
                                  previous-line next-line
                                  magit-section-forward magit-section-backward
                                  magit-section-toggle)))
    (setq claude-gravity--follow-mode nil)
    (remove-hook 'post-command-hook #'claude-gravity--follow-detect-manual t)
    (force-mode-line-update)
    (message "Follow mode OFF (manual navigation)")))

(defun claude-gravity-tail ()
  "Collapse all sections and focus on the tail of the latest turn.
Hides Plan, Files, Allow Patterns, and past turns, then expands
the most recent turn and scrolls to its last response cycle or
final reply text."
  (interactive)
  (when magit-root-section
    ;; Collapse all top-level sections
    (dolist (child (oref magit-root-section children))
      (magit-section-hide child))
    ;; Find and expand the turns section
    (let ((turns-section nil)
          (last-turn nil))
      (dolist (child (oref magit-root-section children))
        (when (eq (oref child type) 'turns)
          (setq turns-section child)))
      (when turns-section
        (magit-section-show turns-section)
        ;; Hide all turns, keep track of the last one
        (dolist (child (oref turns-section children))
          (when (eq (oref child type) 'turn)
            (magit-section-hide child)
            (setq last-turn child)))
        (when last-turn
          (magit-section-show last-turn)
          ;; Collapse earlier response cycles, show the last one
          (let ((last-cycle nil))
            (dolist (child (oref last-turn children))
              (when (eq (oref child type) 'response-cycle)
                (magit-section-hide child)
                (setq last-cycle child)))
            (when last-cycle
              (magit-section-show last-cycle)))
          ;; Jump to the end of the turn content, then recenter
          ;; so the latest activity is visible at bottom of window
          (goto-char (1- (oref last-turn end)))
          (recenter -3))))))

;;;###autoload
(defun claude-gravity-status ()
  "Show the Claude Code overview buffer."
  (interactive)
  (claude-gravity-setup-buffer))

(defun claude-gravity-setup-buffer ()
  "Create and display the overview buffer."
  (with-current-buffer (get-buffer-create claude-gravity-buffer-name)
    (claude-gravity-mode)
    (claude-gravity--render-overview)
    (switch-to-buffer (current-buffer))))

;;; Socket Server

;;; Notification mode-line indicator

(defvar claude-gravity--notification-indicator ""
  "Mode-line string showing Claude notification status.
Added to `global-mode-string' by `claude-gravity-server-start'.")

(put 'claude-gravity--notification-indicator 'risky-local-variable t)

(defun claude-gravity--update-notification-indicator (notification-type label)
  "Update the mode-line indicator based on NOTIFICATION-TYPE and LABEL."
  (setq claude-gravity--notification-indicator
        (cond
         ((equal notification-type "permission")
          (propertize (format " [Claude: permission]") 'face 'claude-gravity-question))
         ((or (equal notification-type "waiting")
              (equal notification-type "tool"))
          (propertize (format " [Claude: waiting]") 'face 'claude-gravity-status-responding))
         (notification-type
          (propertize (format " [Claude: %s]" notification-type) 'face 'claude-gravity-status-responding))
         (t
          (propertize " [Claude: notice]" 'face 'claude-gravity-status-responding))))
  (force-mode-line-update t))

(defun claude-gravity--clear-notification-indicator ()
  "Clear the mode-line notification indicator.
If there are inbox items needing attention, shows the inbox indicator instead."
  (let ((attention (cl-count-if
                    (lambda (item) (not (eq (alist-get 'type item) 'idle)))
                    claude-gravity--inbox)))
    (if (> attention 0)
        (claude-gravity--update-inbox-indicator)
      (when (not (string-empty-p claude-gravity--notification-indicator))
        (setq claude-gravity--notification-indicator "")
        (force-mode-line-update t)))))

(defun claude-gravity--update-inbox-indicator ()
  "Update the mode-line indicator based on inbox contents."
  (let ((attention (cl-count-if
                    (lambda (item) (not (eq (alist-get 'type item) 'idle)))
                    claude-gravity--inbox))
        (has-sessions (> (hash-table-count claude-gravity--sessions) 0)))
    (setq claude-gravity--notification-indicator
          (cond
           ((> attention 0)
            (propertize (format " [Claude: %d!]" attention)
                        'face 'claude-gravity-question))
           (has-sessions
            (propertize " [Claude]" 'face 'claude-gravity-status-idle))
           (t "")))
    (force-mode-line-update t)))

(defun claude-gravity--inbox-notify (item)
  "Flash a message and update mode-line for new inbox ITEM."
  (claude-gravity--update-inbox-indicator)
  (let ((type-label (pcase (alist-get 'type item)
                      ('permission "Permission")
                      ('question "Question")
                      ('plan-review "Plan review")
                      ('idle "Idle")
                      (_ "Notice")))
        (project (or (alist-get 'project item) "?")))
    (message "Claude: %s from %s" type-label project)))

(defvar claude-gravity-server-sock-path
  (expand-file-name "claude-gravity.sock" (file-name-directory (or load-file-name buffer-file-name)))
  "Path to the Unix socket for Claude Gravity communication.")

(defvar claude-gravity-server-process nil
  "The current Claude Gravity server process.")

(defun claude-gravity--server-filter (proc string)
  "Process incoming JSON events from PROC in STRING.
Accumulates partial data per connection until complete lines arrive."
  (let ((buf (concat (or (process-get proc 'claude-gravity--buffer) "") string)))
    ;; Process all complete lines (terminated by newline)
    (while (string-match "\n" buf)
      (let ((line (substring buf 0 (match-beginning 0))))
        (setq buf (substring buf (match-end 0)))
        (when (> (length line) 0)
          (condition-case err
              (let* ((json-object-type 'alist)
                     (data (json-read-from-string line)))
                (message "Claude Gravity received: %s" (alist-get 'event data))
                (let ((event (alist-get 'event data))
                      (session-id (alist-get 'session_id data))
                      (cwd (alist-get 'cwd data))
                      (pid (alist-get 'pid data))
                      (payload (alist-get 'data data))
                      (needs-response (alist-get 'needs_response data)))
                  (if needs-response
                      ;; Bidirectional: queue in inbox instead of stealing focus
                      (let ((tool-name (alist-get 'tool_name payload)))
                        ;; Fire PreToolUse to update session state
                        (claude-gravity-handle-event "PreToolUse" session-id cwd payload pid)
                        (cond
                         ((equal tool-name "AskUserQuestion")
                          (claude-gravity--inbox-add 'question session-id payload proc))
                         ((equal tool-name "ExitPlanMode")
                          (claude-gravity--inbox-add 'plan-review session-id payload proc))
                         (t
                          (claude-gravity--inbox-add 'permission session-id payload proc))))
                    (when event
                      (claude-gravity-handle-event event session-id cwd payload pid)))))
            (error
             (message "Claude Gravity JSON error: %s" err))))))
    ;; Store any remaining partial data
    (process-put proc 'claude-gravity--buffer buf)))

(defun claude-gravity-server-alive-p ()
  "Return non-nil if the socket server is running."
  (and claude-gravity-server-process
       (process-live-p claude-gravity-server-process)))

(defun claude-gravity--ensure-server ()
  "Start the socket server if it is not already running."
  (unless (claude-gravity-server-alive-p)
    (claude-gravity-server-start)))

(defun claude-gravity-server-start ()
  "Start the Unix socket server for Claude Gravity."
  (interactive)
  (claude-gravity-server-stop)
  (when (file-exists-p claude-gravity-server-sock-path)
    (delete-file claude-gravity-server-sock-path))
  (setq claude-gravity-server-process
        (make-network-process
         :name "claude-gravity-server"
         :server t
         :family 'local
         :service claude-gravity-server-sock-path
         :filter 'claude-gravity--server-filter))
  ;; Register notification indicator in mode-line
  (unless (memq 'claude-gravity--notification-indicator global-mode-string)
    (push 'claude-gravity--notification-indicator global-mode-string))
  (message "Claude Gravity server started at %s" claude-gravity-server-sock-path))

(defun claude-gravity-server-stop ()
  "Stop the Claude Gravity server."
  (interactive)
  (when claude-gravity-server-process
    (delete-process claude-gravity-server-process)
    (setq claude-gravity-server-process nil))
  (when (file-exists-p claude-gravity-server-sock-path)
    (delete-file claude-gravity-server-sock-path))
  (message "Claude Gravity server stopped"))

;;; Plan Review Mode

(defvar claude-gravity--plan-review-queue nil
  "Queue of pending plan review requests.
Each entry is (event-data proc session-id).")

(defvar-local claude-gravity--plan-review-proc nil
  "The socket process to send the review decision back on.")

(defvar-local claude-gravity--plan-review-original nil
  "Original plan content for computing diffs.")

(defvar-local claude-gravity--plan-review-session-id nil
  "Session ID associated with this plan review.")

(defvar-local claude-gravity--plan-review-comments nil
  "List of inline comments in the plan review buffer.
Each entry is an alist with keys: line, text, overlay, context.")

(defvar-local claude-gravity--plan-review-inbox-id nil
  "Inbox item ID associated with this plan review buffer, if any.")


(defvar claude-gravity-plan-review-mode-map (make-sparse-keymap)
  "Keymap for `claude-gravity-plan-review-mode'.")
(define-key claude-gravity-plan-review-mode-map (kbd "C-c C-c") #'claude-gravity-plan-review-approve)
(define-key claude-gravity-plan-review-mode-map (kbd "C-c C-k") #'claude-gravity-plan-review-deny)
(define-key claude-gravity-plan-review-mode-map (kbd "C-c C-d") #'claude-gravity-plan-review-diff)
(define-key claude-gravity-plan-review-mode-map (kbd "C-c ;") #'claude-gravity-plan-review-comment)
(define-key claude-gravity-plan-review-mode-map (kbd "C-c ?") #'claude-gravity-plan-review-menu)

(transient-define-prefix claude-gravity-plan-review-menu ()
  "Plan review commands."
  ["Review"
   ("C-c C-c" "Approve plan" claude-gravity-plan-review-approve)
   ("C-c C-k" "Deny with feedback" claude-gravity-plan-review-deny)]
  ["Annotate"
   ("C-c ;" "Inline comment" claude-gravity-plan-review-comment)
   ("C-c C-d" "Show diff" claude-gravity-plan-review-diff)])

(define-minor-mode claude-gravity-plan-review-mode
  "Minor mode for reviewing Claude Code plans.
\\{claude-gravity-plan-review-mode-map}"
  :lighter " PlanReview[C-c ?:menu]"
  :keymap claude-gravity-plan-review-mode-map)

(defun claude-gravity--handle-ask-user-question (event-data proc session-id cwd)
  "Handle bidirectional AskUserQuestion from EVENT-DATA.
PROC is the socket to respond on.  SESSION-ID and CWD identify the session."
  ;; Fire normal PreToolUse handler to update session state
  (claude-gravity-handle-event "PreToolUse" session-id cwd event-data)
  ;; Defer interactive part to the command loop — completing-read
  ;; cannot run inside a process filter.
  (let ((tool-input (alist-get 'tool_input event-data))
        (tid (alist-get 'tool_use_id event-data)))
    (run-at-time 0 nil
                 #'claude-gravity--ask-user-question-prompt
                 tool-input proc session-id tid)))

(defun claude-gravity--ask-user-question-prompt (tool-input proc session-id tid)
  "Prompt user for AskUserQuestion answer via minibuffer.
TOOL-INPUT is the question data, PROC the socket, SESSION-ID the session,
TID the tool_use_id for updating prompts."
  (let* ((questions (alist-get 'questions tool-input))
         (first-q (and (vectorp questions) (> (length questions) 0) (aref questions 0)))
         (q-text (and first-q (alist-get 'question first-q)))
         (header (and first-q (alist-get 'header first-q)))
         (options (and first-q (alist-get 'options first-q)))
         (choices (when (vectorp options)
                    (cl-loop for i from 0 below (length options)
                             for opt = (aref options i)
                             for label = (alist-get 'label opt)
                             for desc = (alist-get 'description opt)
                             collect (if desc (format "%s -- %s" label desc) label))))
         (prompt-str (format "Claude asks [%s]: %s " (or header "?") (or q-text "?")))
         (answer (if choices
                     (completing-read prompt-str choices nil t)
                   (read-string prompt-str)))
         ;; Extract just the label (before " -- ")
         (answer-label (if (string-match "\\`\\(.+?\\) -- " answer)
                           (match-string 1 answer)
                         answer)))
    ;; Build deny response with the answer
    (let ((response `((hookSpecificOutput
                       . ((hookEventName . "PreToolUse")
                          (permissionDecision . "deny")
                          (permissionDecisionReason
                           . ,(format "User answered from Emacs: %s\nQuestion: %s\nAnswer: %s"
                                      answer-label (or q-text "") answer-label)))))))
      (claude-gravity--send-bidirectional-response proc response))
    ;; Update the prompt entry with the answer
    (let* ((session (claude-gravity--get-session session-id))
           (prompts (when session (plist-get session :prompts)))
           (tid tid))
      (when (and prompts tid)
        (dotimes (i (length prompts))
          (let ((p (aref prompts i)))
            (when (and (equal (alist-get 'type p) 'question)
                       (equal (alist-get 'tool_use_id p) tid))
              (setf (alist-get 'answer p) answer-label)
              (aset prompts i p))))))
    (message "Answered: %s" answer-label)))

;;; Tool Permission Requests

(defun claude-gravity--handle-tool-permission (event-data proc session-id cwd)
  "Handle a tool permission request from Claude Code.
EVENT-DATA is the PermissionRequest payload, PROC the socket to respond on,
SESSION-ID and CWD identify the session."
  ;; Record the tool in session state for UI display
  (claude-gravity-handle-event "PreToolUse" session-id cwd event-data)
  ;; Defer interactive prompt out of process filter
  (run-at-time 0 nil
               #'claude-gravity--tool-permission-prompt
               event-data proc session-id cwd))

(defun claude-gravity--tool-permission-prompt (event-data proc session-id cwd)
  "Prompt user to approve/deny a tool permission request.
EVENT-DATA is the PermissionRequest payload, PROC the socket,
SESSION-ID and CWD identify the session."
  (let* ((tool-name (alist-get 'tool_name event-data))
         (tool-input (alist-get 'tool_input event-data))
         (signature (claude-gravity--tool-signature tool-name tool-input))
         (choices '("Allow" "Allow always" "Deny"))
         (answer (completing-read
                  (format "Claude permission [%s]: " signature)
                  choices nil t)))
    (pcase answer
      ("Allow"
       (claude-gravity--send-permission-response proc "allow"))
      ("Allow always"
       (claude-gravity--send-permission-response proc "allow")
       (claude-gravity--write-allow-pattern-for-tool tool-name tool-input session-id cwd))
      ("Deny"
       (let ((reason (read-string "Reason (optional): ")))
         (if (string-empty-p reason)
             (claude-gravity--send-permission-response proc "deny")
           (claude-gravity--send-permission-response proc "deny" reason)))))))

(defun claude-gravity--send-permission-response (proc behavior &optional message)
  "Send allow/deny response for a PermissionRequest.
PROC is the socket, BEHAVIOR is \"allow\" or \"deny\", MESSAGE is optional reason."
  (let* ((decision (if message
                       `((behavior . ,behavior) (message . ,message))
                     `((behavior . ,behavior))))
         (response `((hookSpecificOutput
                      . ((hookEventName . "PermissionRequest")
                         (decision . ,decision))))))
    (claude-gravity--send-bidirectional-response proc response)))

(defun claude-gravity--write-allow-pattern-for-tool (tool-name tool-input session-id cwd)
  "Write an allow pattern for TOOL-NAME/TOOL-INPUT to settings.local.json.
SESSION-ID and CWD identify the session project."
  (let* ((suggestions (claude-gravity--suggest-patterns tool-name tool-input))
         (chosen (if (cdr suggestions)
                     (completing-read "Allow pattern: " suggestions nil nil
                                      (car suggestions))
                   (car suggestions)))
         (settings-path (expand-file-name ".claude/settings.local.json" cwd)))
    (when chosen
      (let* ((json-object-type 'alist)
             (json-array-type 'list)
             (data (if (file-exists-p settings-path)
                       (json-read-file settings-path)
                     (list (cons 'permissions (list (cons 'allow nil))))))
             (perms (or (alist-get 'permissions data)
                        (list (cons 'allow nil))))
             (allow (or (alist-get 'allow perms) nil)))
        (if (member chosen allow)
            (message "Pattern already exists: %s" chosen)
          (setf (alist-get 'allow perms) (append allow (list chosen)))
          (setf (alist-get 'permissions data) perms)
          (let ((dir (file-name-directory settings-path)))
            (unless (file-exists-p dir)
              (make-directory dir t)))
          (with-temp-file settings-path
            (let ((json-encoding-pretty-print t))
              (insert (json-encode data))))
          (let ((session (claude-gravity--get-session session-id)))
            (when session
              (claude-gravity--load-allow-patterns session)))
          (message "Added allow pattern: %s" chosen))))))

(defun claude-gravity--handle-plan-review (event-data proc session-id)
  "Open a plan review buffer for EVENT-DATA.
PROC is the socket connection to respond on.
SESSION-ID identifies the Claude Code session."
  (let* ((tool-input (alist-get 'tool_input event-data))
         (plan-content (or (alist-get 'planContent tool-input)
                           ;; ExitPlanMode may have allowedPrompts but plan
                           ;; content could be in different fields
                           (alist-get 'plan tool-input)
                           ;; Fallback: stringify the whole tool_input
                           (json-encode tool-input)))
         (allowed-prompts (alist-get 'allowedPrompts tool-input))
         (session (claude-gravity--get-session session-id))
         (label (if session
                    (claude-gravity--session-label session)
                  (or session-id "unknown")))
         (buf-name (format "*Claude Plan Review: %s*" label))
         (buf (get-buffer-create buf-name)))
    (with-current-buffer buf
      (erase-buffer)
      ;; Insert plan content
      (insert plan-content)
      (insert "\n")
      ;; Insert allowed prompts section if present
      (when (and allowed-prompts (> (length allowed-prompts) 0))
        (insert "\n## Allowed Prompts\n\n")
        (seq-doseq (p allowed-prompts)
          (let ((tool (alist-get 'tool p))
                (prompt (alist-get 'prompt p)))
            (insert (format "- **%s**: %s\n" (or tool "?") (or prompt "?"))))))
      ;; Set up modes
      (when (fboundp 'markdown-mode)
        (markdown-mode))
      (claude-gravity-plan-review-mode 1)
      ;; Store buffer-local state
      (setq-local claude-gravity--plan-review-proc proc)
      (setq-local claude-gravity--plan-review-original plan-content)
      (setq-local claude-gravity--plan-review-session-id session-id)
      ;; Kill hook to handle buffer closed without decision
      (add-hook 'kill-buffer-hook #'claude-gravity--plan-review-on-kill nil t)
      (goto-char (point-min))
      (set-buffer-modified-p nil))
    ;; Display prominently
    (display-buffer-in-side-window buf '((side . bottom) (window-height . 0.5)))
    (select-window (get-buffer-window buf))
    (message "Plan review: C-c C-c approve | C-c C-k deny | c comment | C-c C-d diff")))

(defun claude-gravity--send-bidirectional-response (proc response)
  "Send RESPONSE JSON to the bridge via socket PROC, then close."
  (if (and proc (process-live-p proc))
      (progn
        (process-send-string proc (concat (json-encode response) "\n"))
        (run-at-time 0.1 nil (lambda (p) (when (process-live-p p) (delete-process p))) proc))
    (message "Claude Gravity: bridge connection lost. Response not sent.")))

(defun claude-gravity--plan-review-send-response (response)
  "Send RESPONSE JSON to the bridge via the stored socket proc."
  (claude-gravity--send-bidirectional-response claude-gravity--plan-review-proc response)
  (setq-local claude-gravity--plan-review-proc nil))

(defun claude-gravity--plan-review-compute-diff ()
  "Compute a unified diff between original and current buffer content.
Returns the diff string or nil if no changes."
  (let ((original claude-gravity--plan-review-original)
        (current (buffer-substring-no-properties (point-min) (point-max))))
    ;; Trim trailing whitespace for comparison
    (setq current (string-trim-right current))
    (setq original (string-trim-right original))
    (if (string= original current)
        nil
      (let ((orig-file (make-temp-file "plan-orig"))
            (curr-file (make-temp-file "plan-curr")))
        (unwind-protect
            (progn
              (with-temp-file orig-file (insert original))
              (with-temp-file curr-file (insert current))
              (with-temp-buffer
                (call-process "diff" nil t nil "-u" orig-file curr-file)
                (buffer-string)))
          (delete-file orig-file)
          (delete-file curr-file))))))

(defun claude-gravity-plan-review-comment ()
  "Add an inline comment on the current line in the plan review buffer."
  (interactive)
  (let* ((text (read-string "Comment: "))
         (line-num (line-number-at-pos))
         (line-text (string-trim
                     (buffer-substring-no-properties
                      (line-beginning-position) (line-end-position))))
         (beg (line-beginning-position))
         (end (line-end-position))
         (ov (make-overlay beg end)))
    (when (string-empty-p text)
      (delete-overlay ov)
      (user-error "Empty comment, cancelled"))
    (overlay-put ov 'face '(:underline (:style wave :color "orange")))
    (overlay-put ov 'help-echo text)
    (overlay-put ov 'after-string
                 (propertize (format "  « %s »" text)
                             'face '(:foreground "orange" :slant italic)))
    (overlay-put ov 'claude-plan-comment t)
    (push `((line . ,line-num)
            (text . ,text)
            (overlay . ,ov)
            (context . ,line-text))
          claude-gravity--plan-review-comments)
    (message "Comment added on line %d" line-num)))

(defun claude-gravity--plan-review-collect-feedback ()
  "Collect inline comments from `claude-gravity--plan-review-comments'.
Returns a formatted markdown string or nil if no comments."
  (when claude-gravity--plan-review-comments
    (let ((sorted (sort (copy-sequence claude-gravity--plan-review-comments)
                        (lambda (a b)
                          (< (alist-get 'line a) (alist-get 'line b))))))
      (concat "## Inline comments:\n"
              (mapconcat
               (lambda (entry)
                 (let ((line (alist-get 'line entry))
                       (text (alist-get 'text entry))
                       (ctx (alist-get 'context entry)))
                   (format "- Line %d (near \"%s\"): \"%s\""
                           line
                           (if (> (length ctx) 60)
                               (concat (substring ctx 0 57) "...")
                             ctx)
                           text)))
               sorted "\n")
              "\n"))))

(defun claude-gravity--plan-review-scan-markers ()
  "Scan the buffer for @claude text markers.
Returns a formatted markdown string or nil if none found."
  (save-excursion
    (goto-char (point-min))
    (let ((markers nil))
      (while (re-search-forward "^.*@[Cc]laude:?\\s-*\\(.+\\)" nil t)
        (let* ((marker-text (string-trim (match-string 1)))
               (line-num (line-number-at-pos (match-beginning 0)))
               ;; Get surrounding context: previous non-empty line
               (ctx (save-excursion
                      (forward-line -1)
                      (while (and (not (bobp))
                                  (looking-at-p "^\\s-*$"))
                        (forward-line -1))
                      (string-trim
                       (buffer-substring-no-properties
                        (line-beginning-position) (line-end-position))))))
          (push `((line . ,line-num)
                  (text . ,marker-text)
                  (context . ,ctx))
                markers)))
      (when markers
        (let ((sorted (sort (nreverse markers)
                            (lambda (a b)
                              (< (alist-get 'line a) (alist-get 'line b))))))
          (concat "## @claude markers:\n"
                  (mapconcat
                   (lambda (entry)
                     (let ((line (alist-get 'line entry))
                           (text (alist-get 'text entry))
                           (ctx (alist-get 'context entry)))
                       (format "- Line %d (near \"%s\"): \"%s\""
                               line
                               (if (> (length ctx) 60)
                                   (concat (substring ctx 0 57) "...")
                                 ctx)
                               text)))
                   sorted "\n")
                  "\n"))))))

(defun claude-gravity--plan-review-build-feedback-message (diff comments markers general-comment)
  "Build a structured feedback message from DIFF, COMMENTS, MARKERS, and GENERAL-COMMENT.
Returns the formatted markdown string.  Omits empty sections."
  (let ((parts (list "# Plan Feedback\n")))
    (when comments
      (push (concat comments "\n") parts))
    (when markers
      (push (concat markers "\n") parts))
    (when diff
      (push (format "## Changes requested:\n```diff\n%s\n```\n" diff) parts))
    (when (and general-comment (not (string-empty-p general-comment)))
      (push (format "## General comment:\n%s\n" general-comment) parts))
    (when (and (not diff) (not comments) (not markers)
               (or (not general-comment) (string-empty-p general-comment)))
      (push "Plan was rejected without specific feedback.\n" parts))
    (string-join (nreverse parts) "\n")))

(defun claude-gravity--plan-review-cleanup-and-close ()
  "Remove inbox item, kill-buffer hook, and close the plan review buffer."
  (when claude-gravity--plan-review-inbox-id
    (claude-gravity--inbox-remove claude-gravity--plan-review-inbox-id))
  (remove-hook 'kill-buffer-hook #'claude-gravity--plan-review-on-kill t)
  (let ((buf (current-buffer)))
    (quit-window)
    (kill-buffer buf)))

(defun claude-gravity-plan-review-approve ()
  "Approve the plan and send allow decision to Claude Code.
If the user has made edits, added inline comments, or inserted
@claude markers, automatically deny instead so Claude can
incorporate the feedback (the allow channel cannot carry feedback)."
  (interactive)
  (let ((diff (claude-gravity--plan-review-compute-diff))
        (comments (claude-gravity--plan-review-collect-feedback))
        (markers (claude-gravity--plan-review-scan-markers)))
    (if (or diff comments markers)
        ;; Feedback exists — deny so Claude sees it
        (let ((deny-message (claude-gravity--plan-review-build-feedback-message
                             diff comments markers
                             "I edited/annotated the plan. Please update your plan to match these changes and call ExitPlanMode again.")))
          (let ((response `((hookSpecificOutput
                             . ((hookEventName . "PermissionRequest")
                                (decision . ((behavior . "deny")
                                             (message . ,deny-message))))))))
            (claude-gravity--plan-review-send-response response))
          (claude-gravity--plan-review-cleanup-and-close)
          (message "Feedback detected — denied for revision"))
      ;; No feedback — clean approve
      (let ((response `((hookSpecificOutput
                         . ((hookEventName . "PermissionRequest")
                            (decision . ((behavior . "allow"))))))))
        (claude-gravity--plan-review-send-response response))
      (claude-gravity--plan-review-cleanup-and-close)
      (message "Plan approved"))))

(defun claude-gravity-plan-review-deny ()
  "Deny the plan and send feedback to Claude Code.
Collects inline comments, @claude markers, diff, and a general comment."
  (interactive)
  (let* ((diff (claude-gravity--plan-review-compute-diff))
         (comments (claude-gravity--plan-review-collect-feedback))
         (markers (claude-gravity--plan-review-scan-markers))
         (general-comment (read-string "Feedback comment (optional): "))
         (deny-message (claude-gravity--plan-review-build-feedback-message
                        diff comments markers general-comment)))
    (let ((response `((hookSpecificOutput
                       . ((hookEventName . "PermissionRequest")
                          (decision . ((behavior . "deny")
                                       (message . ,deny-message))))))))
      (claude-gravity--plan-review-send-response response))
    (claude-gravity--plan-review-cleanup-and-close)
    (message "Plan denied with feedback")))

(defun claude-gravity-plan-review-diff ()
  "Show a diff between the original plan and current edits."
  (interactive)
  (let ((diff (claude-gravity--plan-review-compute-diff)))
    (if diff
        (let ((buf (get-buffer-create "*Claude Plan Diff*")))
          (with-current-buffer buf
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert diff))
            (diff-mode)
            (goto-char (point-min))
            (setq buffer-read-only t))
          (display-buffer buf))
      (message "No changes from original plan"))))

(defun claude-gravity--plan-review-on-kill ()
  "Handle plan review buffer being killed without explicit decision.
Sends a deny response and cleans up inbox item."
  (when claude-gravity--plan-review-inbox-id
    (claude-gravity--inbox-remove claude-gravity--plan-review-inbox-id))
  (when claude-gravity--plan-review-proc
    (let ((response `((hookSpecificOutput
                       . ((hookEventName . "PermissionRequest")
                          (decision . ((behavior . "deny")
                                       (message . "Plan review cancelled (buffer closed)"))))))))
      (claude-gravity--plan-review-send-response response))))

(defun claude-gravity-test-plan-review ()
  "Open a simulated plan review buffer for testing.
No socket proc is attached — approve/deny will just message."
  (interactive)
  (claude-gravity--handle-plan-review
   '((tool_input . ((planContent . "## Test Plan\n\n1. Step one\n2. Step two\n3. Step three\n")
                     (allowedPrompts . [((tool . "Bash") (prompt . "npm test"))]))))
   nil
   "test-session"))

;;; ============================================================================
;;; Inbox Action Buffers
;;; ============================================================================

;; --- Permission Action Buffer ---

(defvar claude-gravity-permission-action-mode-map (make-sparse-keymap)
  "Keymap for `claude-gravity-permission-action-mode'.")
(define-key claude-gravity-permission-action-mode-map (kbd "a") #'claude-gravity-permission-action-allow)
(define-key claude-gravity-permission-action-mode-map (kbd "A") #'claude-gravity-permission-action-allow-always)
(define-key claude-gravity-permission-action-mode-map (kbd "d") #'claude-gravity-permission-action-deny)
(define-key claude-gravity-permission-action-mode-map (kbd "q") #'claude-gravity-permission-action-quit)

(define-minor-mode claude-gravity-permission-action-mode
  "Minor mode for permission action buffers.
\\{claude-gravity-permission-action-mode-map}"
  :lighter " PermAction"
  :keymap claude-gravity-permission-action-mode-map)

(defvar-local claude-gravity--action-inbox-item nil
  "The inbox item associated with this action buffer.")

(defun claude-gravity--inbox-act-permission (item)
  "Open a permission action buffer for inbox ITEM."
  (let* ((data (alist-get 'data item))
         (label (alist-get 'label item))
         (tool-name (alist-get 'tool_name data))
         (tool-input (alist-get 'tool_input data))
         (signature (claude-gravity--tool-signature tool-name tool-input))
         (buf (get-buffer-create "*Claude Action: Permission*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize (format "Permission Request — %s\n" label)
                            'face 'claude-gravity-header-title))
        (insert (make-string 60 ?─) "\n\n")
        (insert (propertize "Tool: " 'face 'claude-gravity-detail-label)
                (propertize signature 'face 'claude-gravity-tool-name) "\n\n")
        ;; Show tool input details
        (when tool-input
          (insert (propertize "Input:\n" 'face 'claude-gravity-detail-label))
          (let ((json-encoding-pretty-print t))
            (insert (json-encode tool-input) "\n\n")))
        (insert (make-string 60 ?─) "\n")
        (insert (propertize "  a" 'face 'claude-gravity-tool-name) " Allow  "
                (propertize "  A" 'face 'claude-gravity-tool-name) " Allow always  "
                (propertize "  d" 'face 'claude-gravity-tool-name) " Deny  "
                (propertize "  q" 'face 'claude-gravity-tool-name) " Close\n"))
      (setq buffer-read-only t)
      (goto-char (point-min))
      (claude-gravity-permission-action-mode 1)
      (setq-local claude-gravity--action-inbox-item item))
    (display-buffer-in-side-window buf '((side . bottom) (window-height . 0.35)))
    (select-window (get-buffer-window buf))))

(defun claude-gravity--permission-action-finish ()
  "Clean up after a permission action: remove inbox item, kill buffer."
  (let ((item claude-gravity--action-inbox-item))
    (when item
      (claude-gravity--inbox-remove (alist-get 'id item))))
  (let ((buf (current-buffer)))
    (quit-window)
    (kill-buffer buf)))

(defun claude-gravity-permission-action-allow ()
  "Allow the permission request."
  (interactive)
  (let* ((item claude-gravity--action-inbox-item)
         (proc (alist-get 'socket-proc item)))
    (claude-gravity--send-permission-response proc "allow")
    (claude-gravity--permission-action-finish)
    (message "Permission allowed")))

(defun claude-gravity-permission-action-allow-always ()
  "Allow the permission and write an allow pattern."
  (interactive)
  (let* ((item claude-gravity--action-inbox-item)
         (data (alist-get 'data item))
         (proc (alist-get 'socket-proc item))
         (tool-name (alist-get 'tool_name data))
         (tool-input (alist-get 'tool_input data))
         (session-id (alist-get 'session-id item))
         (session (claude-gravity--get-session session-id))
         (cwd (when session (plist-get session :cwd))))
    (claude-gravity--send-permission-response proc "allow")
    (when cwd
      (claude-gravity--write-allow-pattern-for-tool tool-name tool-input session-id cwd))
    (claude-gravity--permission-action-finish)
    (message "Permission allowed + pattern written")))

(defun claude-gravity-permission-action-deny ()
  "Deny the permission request."
  (interactive)
  (let* ((item claude-gravity--action-inbox-item)
         (proc (alist-get 'socket-proc item))
         (reason (read-string "Reason (optional): ")))
    (if (string-empty-p reason)
        (claude-gravity--send-permission-response proc "deny")
      (claude-gravity--send-permission-response proc "deny" reason))
    (claude-gravity--permission-action-finish)
    (message "Permission denied")))

(defun claude-gravity-permission-action-quit ()
  "Close action buffer without responding."
  (interactive)
  (let ((buf (current-buffer)))
    (quit-window)
    (kill-buffer buf)))

;; --- Question Action Buffer ---

(defvar claude-gravity-question-action-mode-map (make-sparse-keymap)
  "Keymap for `claude-gravity-question-action-mode'.")
(define-key claude-gravity-question-action-mode-map (kbd "1") #'claude-gravity-question-action-1)
(define-key claude-gravity-question-action-mode-map (kbd "2") #'claude-gravity-question-action-2)
(define-key claude-gravity-question-action-mode-map (kbd "3") #'claude-gravity-question-action-3)
(define-key claude-gravity-question-action-mode-map (kbd "4") #'claude-gravity-question-action-4)
(define-key claude-gravity-question-action-mode-map (kbd "o") #'claude-gravity-question-action-other)
(define-key claude-gravity-question-action-mode-map (kbd "q") #'claude-gravity-question-action-quit)

(define-minor-mode claude-gravity-question-action-mode
  "Minor mode for question action buffers.
\\{claude-gravity-question-action-mode-map}"
  :lighter " QuestionAction"
  :keymap claude-gravity-question-action-mode-map)

(defvar-local claude-gravity--question-choices nil
  "List of (label . description) choices for the current question buffer.")

(defun claude-gravity--inbox-act-question (item)
  "Open a question action buffer for inbox ITEM."
  (let* ((data (alist-get 'data item))
         (label (alist-get 'label item))
         (tool-input (alist-get 'tool_input data))
         (questions (alist-get 'questions tool-input))
         (first-q (and (vectorp questions) (> (length questions) 0) (aref questions 0)))
         (q-text (and first-q (alist-get 'question first-q)))
         (header (and first-q (alist-get 'header first-q)))
         (options (and first-q (alist-get 'options first-q)))
         (choices (when (vectorp options)
                    (cl-loop for i from 0 below (length options)
                             for opt = (aref options i)
                             for opt-label = (alist-get 'label opt)
                             for desc = (alist-get 'description opt)
                             collect (cons opt-label desc))))
         (buf (get-buffer-create "*Claude Action: Question*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize (format "Question — %s\n" label)
                            'face 'claude-gravity-header-title))
        (insert (make-string 60 ?─) "\n\n")
        (when header
          (insert (propertize (format "[%s]\n" header) 'face 'claude-gravity-detail-label)))
        (insert (propertize (or q-text "?") 'face 'default) "\n\n")
        (when choices
          (cl-loop for i from 0
                   for (clabel . desc) in choices
                   do (insert (propertize (format "  %d" (1+ i)) 'face 'claude-gravity-tool-name)
                              " " clabel
                              (if desc (format "  (%s)" desc) "")
                              "\n")))
        (insert "\n" (make-string 60 ?─) "\n")
        (insert (propertize "  1-4" 'face 'claude-gravity-tool-name) " Select option  "
                (propertize "  o" 'face 'claude-gravity-tool-name) " Free text  "
                (propertize "  q" 'face 'claude-gravity-tool-name) " Close\n"))
      (setq buffer-read-only t)
      (goto-char (point-min))
      (claude-gravity-question-action-mode 1)
      (setq-local claude-gravity--action-inbox-item item)
      (setq-local claude-gravity--question-choices choices))
    (display-buffer-in-side-window buf '((side . bottom) (window-height . 0.35)))
    (select-window (get-buffer-window buf))))

(defun claude-gravity--question-action-respond (answer-label)
  "Send ANSWER-LABEL as the question response and clean up."
  (let* ((item claude-gravity--action-inbox-item)
         (proc (alist-get 'socket-proc item))
         (data (alist-get 'data item))
         (q-text (let* ((tool-input (alist-get 'tool_input data))
                        (questions (alist-get 'questions tool-input))
                        (first-q (and (vectorp questions) (> (length questions) 0) (aref questions 0))))
                   (and first-q (alist-get 'question first-q))))
         (tid (alist-get 'tool_use_id data))
         (session-id (alist-get 'session-id item)))
    ;; Send deny response with the answer
    (let ((response `((hookSpecificOutput
                       . ((hookEventName . "PreToolUse")
                          (permissionDecision . "deny")
                          (permissionDecisionReason
                           . ,(format "User answered from Emacs: %s\nQuestion: %s\nAnswer: %s"
                                      answer-label (or q-text "") answer-label)))))))
      (claude-gravity--send-bidirectional-response proc response))
    ;; Update prompt entry with answer
    (let* ((session (claude-gravity--get-session session-id))
           (prompts (when session (plist-get session :prompts))))
      (when (and prompts tid)
        (dotimes (i (length prompts))
          (let ((p (aref prompts i)))
            (when (and (equal (alist-get 'type p) 'question)
                       (equal (alist-get 'tool_use_id p) tid))
              (setf (alist-get 'answer p) answer-label)
              (aset prompts i p))))))
    ;; Clean up
    (claude-gravity--inbox-remove (alist-get 'id item))
    (let ((buf (current-buffer)))
      (quit-window)
      (kill-buffer buf))
    (message "Answered: %s" answer-label)))

(defun claude-gravity--question-action-select (n)
  "Select option N (1-based) from the question choices."
  (let ((choices claude-gravity--question-choices))
    (if (and choices (>= n 1) (<= n (length choices)))
        (claude-gravity--question-action-respond (car (nth (1- n) choices)))
      (message "No option %d" n))))

(defun claude-gravity-question-action-1 () "Select option 1." (interactive) (claude-gravity--question-action-select 1))
(defun claude-gravity-question-action-2 () "Select option 2." (interactive) (claude-gravity--question-action-select 2))
(defun claude-gravity-question-action-3 () "Select option 3." (interactive) (claude-gravity--question-action-select 3))
(defun claude-gravity-question-action-4 () "Select option 4." (interactive) (claude-gravity--question-action-select 4))

(defun claude-gravity-question-action-other ()
  "Enter free text answer."
  (interactive)
  (let ((answer (read-string "Your answer: ")))
    (unless (string-empty-p answer)
      (claude-gravity--question-action-respond answer))))

(defun claude-gravity-question-action-quit ()
  "Close question action buffer without responding."
  (interactive)
  (let ((buf (current-buffer)))
    (quit-window)
    (kill-buffer buf)))

;; --- Plan Review Action (reuse existing) ---

(defun claude-gravity--inbox-act-plan-review (item)
  "Open a plan review buffer for inbox ITEM.
Reuses existing `claude-gravity--handle-plan-review', modified to
remove the inbox item when plan review is acted on."
  (let ((data (alist-get 'data item))
        (proc (alist-get 'socket-proc item))
        (session-id (alist-get 'session-id item))
        (inbox-id (alist-get 'id item)))
    (claude-gravity--handle-plan-review data proc session-id)
    ;; Store inbox-id on the plan review buffer so approve/deny can clean up
    (let ((buf (get-buffer (format "*Claude Plan Review: %s*"
                                    (or (alist-get 'label item)
                                        session-id "unknown")))))
      (when buf
        (with-current-buffer buf
          (setq-local claude-gravity--plan-review-inbox-id inbox-id))))))

;; --- Idle Session Action ---

(defun claude-gravity--inbox-act-idle (item)
  "Open the session buffer for an idle inbox ITEM."
  (let ((session-id (alist-get 'session-id item)))
    (claude-gravity--inbox-remove (alist-get 'id item))
    (claude-gravity-open-session session-id)))

;;; ============================================================================
;;; Tmux-based Interactive Claude Sessions
;;; ============================================================================
;;; All data (tools, text, agents, tasks, permissions, questions) comes from
;;; hooks via the bridge.  Prompts are sent via `tmux send-keys`.  Interactive
;;; Claude (no -p flag) runs inside a tmux session with TERM=dumb.

(defvar claude-gravity--tmux-sessions (make-hash-table :test 'equal)
  "Map of session-id → tmux session name for Emacs-managed Claude sessions.")

(defvar claude-gravity--tmux-pending (make-hash-table :test 'equal)
  "Map of cwd → temp-id for tmux sessions awaiting SessionStart re-keying.")

(defvar claude-gravity--tmux-heartbeat-timer nil
  "Timer for periodic tmux session liveness checks.")

(defun claude-gravity--claude-command ()
  "Return the claude command path."
  (or (executable-find "claude")
      (error "Claude CLI not found in PATH")))

(defun claude-gravity--tmux-check ()
  "Check that tmux is available; signal error if not."
  (unless (executable-find "tmux")
    (error "tmux is required for managed sessions but not found in PATH")))

(defun claude-gravity--tmux-alive-p (tmux-name)
  "Return non-nil if tmux session TMUX-NAME is alive."
  (= 0 (call-process "tmux" nil nil nil "has-session" "-t" tmux-name)))

(defun claude-gravity--tmux-send-keys (tmux-name text)
  "Send TEXT to tmux session TMUX-NAME, then press Enter.
For multi-line text, uses tmux load-buffer/paste-buffer to avoid
send-keys -l interpreting newlines as Enter."
  (if (string-match-p "\n" text)
      ;; Multi-line: pipe through load-buffer then paste
      (let ((tmpfile (make-temp-file "claude-tmux-")))
        (unwind-protect
            (progn
              (with-temp-file tmpfile (insert text))
              (call-process "tmux" nil nil nil "load-buffer" tmpfile)
              (call-process "tmux" nil nil nil "paste-buffer" "-t" tmux-name)
              (call-process "tmux" nil nil nil "send-keys" "-t" tmux-name "Enter"))
          (delete-file tmpfile)))
    ;; Single line: send-keys -l for literal text
    (call-process "tmux" nil nil nil "send-keys" "-t" tmux-name "-l" text)
    (call-process "tmux" nil nil nil "send-keys" "-t" tmux-name "Enter")))

(defun claude-gravity--current-session-tmux-p ()
  "Return non-nil if the current session has a tmux session."
  (let ((sid (or claude-gravity--buffer-session-id
                 (let ((section (magit-current-section)))
                   (when (and section (eq (oref section type) 'session-entry))
                     (oref section value))))))
    (and sid (gethash sid claude-gravity--tmux-sessions))))

(defun claude-gravity-start-session (cwd &optional model permission-mode)
  "Start a new Claude session in CWD via tmux.
Optional MODEL overrides the default.  PERMISSION-MODE sets the mode.
Returns the temp session-id (re-keyed when SessionStart hook arrives)."
  (interactive
   (list (read-directory-name "Project directory: " default-directory)))
  (claude-gravity--tmux-check)
  (claude-gravity--ensure-server)
  (setq cwd (claude-gravity--normalize-cwd cwd))
  ;; Prevent concurrent same-cwd sessions
  (when (gethash cwd claude-gravity--tmux-pending)
    (error "A tmux session is already pending for %s" cwd))
  (let* ((temp-id (format "tmux-%s" (format-time-string "%s%3N")))
         (tmux-name (format "claude-%s" temp-id))
         (plugin-dir (expand-file-name
                      "emacs-bridge"
                      (file-name-directory
                       (or load-file-name
                           (locate-library "claude-gravity")
                           (error "Cannot locate claude-gravity.el for --plugin-dir")))))
         (cmd-parts (list "env" "TERM=dumb" "claude" "--plugin-dir" plugin-dir)))
    (when model
      (setq cmd-parts (append cmd-parts (list "--model" model))))
    (when permission-mode
      (setq cmd-parts (append cmd-parts (list "--permission-mode" permission-mode))))
    (let ((result (apply #'call-process "tmux" nil nil nil
                         "new-session" "-d" "-s" tmux-name
                         "-c" cwd
                         cmd-parts)))
      (unless (= result 0)
        (error "Failed to create tmux session %s" tmux-name))
      ;; Register pending re-key by cwd
      (puthash cwd temp-id claude-gravity--tmux-pending)
      (puthash temp-id tmux-name claude-gravity--tmux-sessions)
      ;; Create session with temp ID
      (let ((session (claude-gravity--ensure-session temp-id cwd)))
        (plist-put session :tmux-session tmux-name)
        (claude-gravity-model-set-claude-status session 'idle))
      (claude-gravity--tmux-ensure-heartbeat)
      (claude-gravity--schedule-refresh)
      (message "Claude tmux session starting in %s" cwd)
      temp-id)))

(defun claude-gravity-resume-session (session-id &optional cwd model)
  "Resume an existing Claude session by SESSION-ID via tmux.
CWD defaults to the session's stored cwd.  MODEL overrides the default."
  (interactive
   (let* ((candidates nil))
     (maphash (lambda (id session)
                (when (eq (plist-get session :status) 'ended)
                  (push (cons (format "%s [%s]"
                                      (or (plist-get session :slug) id)
                                      (plist-get session :project))
                              id)
                        candidates)))
              claude-gravity--sessions)
     (if candidates
         (let* ((choice (completing-read "Resume session: " candidates nil t))
                (sid (cdr (assoc choice candidates))))
           (list sid))
       (list (read-string "Session ID to resume: ")))))
  (claude-gravity--tmux-check)
  (claude-gravity--ensure-server)
  (let* ((existing (claude-gravity--get-session session-id))
         (cwd (claude-gravity--normalize-cwd
               (or cwd
                   (and existing (plist-get existing :cwd))
                   (read-directory-name "Project directory: "))))
         (tmux-name (format "claude-resume-%s"
                            (substring session-id 0 (min 8 (length session-id)))))
         (plugin-dir (expand-file-name
                      "emacs-bridge"
                      (file-name-directory
                       (or load-file-name
                           (locate-library "claude-gravity")
                           (error "Cannot locate claude-gravity.el for --plugin-dir")))))
         (cmd-parts (list "env" "TERM=dumb" "claude"
                          "--resume" session-id
                          "--plugin-dir" plugin-dir)))
    (when model
      (setq cmd-parts (append cmd-parts (list "--model" model))))
    ;; Register pending re-key by cwd (SessionStart will arrive with real ID)
    (puthash cwd session-id claude-gravity--tmux-pending)
    (let ((result (apply #'call-process "tmux" nil nil nil
                         "new-session" "-d" "-s" tmux-name
                         "-c" cwd
                         cmd-parts)))
      (unless (= result 0)
        (remhash cwd claude-gravity--tmux-pending)
        (error "Failed to create tmux session %s" tmux-name))
      (puthash session-id tmux-name claude-gravity--tmux-sessions)
      ;; Update or create session
      (let ((session (claude-gravity--ensure-session session-id cwd)))
        (when (eq (plist-get session :status) 'ended)
          (plist-put session :status 'active))
        (plist-put session :tmux-session tmux-name)
        (claude-gravity-model-set-claude-status session 'idle))
      (claude-gravity--tmux-ensure-heartbeat)
      (claude-gravity--schedule-refresh)
      (message "Claude tmux session resuming %s" session-id)
      session-id)))

(defun claude-gravity-send-prompt (prompt &optional session-id)
  "Send PROMPT to the tmux Claude session for SESSION-ID.
If SESSION-ID is nil, uses the current buffer's session."
  (interactive
   (list (read-string "Prompt: ")))
  (let* ((sid (or session-id
                  claude-gravity--buffer-session-id
                  ;; Find any active tmux session
                  (let ((found nil))
                    (maphash (lambda (id tmux-name)
                               (when (and (not found)
                                          (claude-gravity--tmux-alive-p tmux-name))
                                 (setq found id)))
                             claude-gravity--tmux-sessions)
                    found)))
         (tmux-name (and sid (gethash sid claude-gravity--tmux-sessions))))
    (unless tmux-name
      (error "No tmux Claude session found for %s" (or sid "any")))
    (unless (claude-gravity--tmux-alive-p tmux-name)
      (error "Tmux session %s is not running" tmux-name))
    (claude-gravity--tmux-send-keys tmux-name prompt)
    ;; Set flag so UserPromptSubmit hook dedup works
    (let ((session (claude-gravity--get-session sid)))
      (when session
        (plist-put session :tmux-prompt-sent t)
        (claude-gravity-model-add-prompt
         session (list (cons 'text prompt)
                       (cons 'submitted (current-time))
                       (cons 'elapsed nil)))
        (claude-gravity-model-set-claude-status session 'responding)
        (claude-gravity--schedule-session-refresh sid)))
    (message "Sent prompt to Claude [%s]" sid)))

(defun claude-gravity-slash-command (command &optional session-id)
  "Send slash COMMAND to the tmux Claude session and display output.
COMMAND should include the leading slash, e.g. \"/context\".
Captures the tmux pane content after a short delay and shows it
in a read-only buffer."
  (interactive
   (list (read-string "Slash command: " "/")))
  (let* ((sid (or session-id
                  claude-gravity--buffer-session-id
                  (let ((found nil))
                    (maphash (lambda (id tmux-name)
                               (when (and (not found)
                                          (claude-gravity--tmux-alive-p tmux-name))
                                 (setq found id)))
                             claude-gravity--tmux-sessions)
                    found)))
         (tmux-name (and sid (gethash sid claude-gravity--tmux-sessions))))
    (unless tmux-name
      (error "No tmux Claude session found for %s" (or sid "any")))
    (unless (claude-gravity--tmux-alive-p tmux-name)
      (error "Tmux session %s is not running" tmux-name))
    (claude-gravity--tmux-send-keys tmux-name command)
    (let ((cmd command))
      (run-at-time 1.0 nil
        (lambda ()
          (let* ((raw (shell-command-to-string
                        (format "tmux capture-pane -t %s -p -S -300"
                                (shell-quote-argument tmux-name))))
                 (pos (string-search cmd raw))
                 (output (if pos
                             (substring raw pos)
                           raw)))
            (with-current-buffer (get-buffer-create "*Claude Slash Output*")
              (let ((inhibit-read-only t))
                (erase-buffer)
                (insert output)
                (goto-char (point-min)))
              (special-mode)
              (display-buffer (current-buffer)))))))))

(defun claude-gravity-stop-session (&optional session-id)
  "Stop the tmux Claude session for SESSION-ID."
  (interactive)
  (let* ((sid (or session-id
                  claude-gravity--buffer-session-id
                  (let ((found nil))
                    (maphash (lambda (id tmux-name)
                               (when (and (not found)
                                          (claude-gravity--tmux-alive-p tmux-name))
                                 (setq found id)))
                             claude-gravity--tmux-sessions)
                    found)))
         (tmux-name (and sid (gethash sid claude-gravity--tmux-sessions))))
    (when tmux-name
      (when (claude-gravity--tmux-alive-p tmux-name)
        (call-process "tmux" nil nil nil "kill-session" "-t" tmux-name))
      (remhash sid claude-gravity--tmux-sessions)
      (let ((session (claude-gravity--get-session sid)))
        (when session
          (claude-gravity-model-session-end session)))
      (claude-gravity--schedule-refresh)
      (message "Stopped tmux Claude session [%s]" sid))))

;;; Tmux heartbeat — detect dead sessions

(defun claude-gravity--tmux-heartbeat ()
  "Check all tmux sessions for liveness; mark dead ones as ended."
  (let ((dead nil))
    (maphash (lambda (sid tmux-name)
               (unless (claude-gravity--tmux-alive-p tmux-name)
                 (push sid dead)))
             claude-gravity--tmux-sessions)
    (dolist (sid dead)
      (remhash sid claude-gravity--tmux-sessions)
      (let ((session (claude-gravity--get-session sid)))
        (when (and session (eq (plist-get session :status) 'active))
          (claude-gravity-model-session-end session)
          (claude-gravity--schedule-refresh)
          (claude-gravity--schedule-session-refresh sid))))))

(defun claude-gravity--tmux-ensure-heartbeat ()
  "Start the tmux heartbeat timer if not already running."
  (unless claude-gravity--tmux-heartbeat-timer
    (setq claude-gravity--tmux-heartbeat-timer
          (run-with-timer 5 5 #'claude-gravity--tmux-heartbeat))))

(defun claude-gravity--tmux-cleanup-all ()
  "Kill all tmux sessions managed by gravity.  For `kill-emacs-hook'."
  (when claude-gravity--tmux-heartbeat-timer
    (cancel-timer claude-gravity--tmux-heartbeat-timer)
    (setq claude-gravity--tmux-heartbeat-timer nil))
  (maphash (lambda (_sid tmux-name)
             (ignore-errors
               (call-process "tmux" nil nil nil "kill-session" "-t" tmux-name)))
           claude-gravity--tmux-sessions)
  (clrhash claude-gravity--tmux-sessions)
  (clrhash claude-gravity--tmux-pending))

(add-hook 'kill-emacs-hook #'claude-gravity--tmux-cleanup-all)

(defun claude-gravity-terminal-session ()
  "Open a terminal buffer attached to the current session's tmux."
  (interactive)
  (let* ((sid (or claude-gravity--buffer-session-id
                  (let ((section (magit-current-section)))
                    (when (and section (eq (oref section type) 'session-entry))
                      (oref section value)))))
         (tmux-name (and sid (gethash sid claude-gravity--tmux-sessions)))
         (session (and sid (gethash sid claude-gravity--sessions)))
         (project (and session (plist-get session :project))))
    (unless tmux-name
      (user-error "No tmux session at point"))
    (let* ((buf-name (format "*Claude Terminal: %s*" (or project sid)))
           (existing (get-buffer buf-name)))
      (if (and existing (buffer-live-p existing)
                (get-buffer-process existing)
                (process-live-p (get-buffer-process existing)))
          (pop-to-buffer existing)
        (when existing (kill-buffer existing))
        (let ((buf (make-term (substring buf-name 1 -1)
                              "tmux" nil "attach-session" "-t" tmux-name)))
          (with-current-buffer buf
            (term-char-mode))
          (pop-to-buffer buf))))))

;; Keybindings for session commands
(define-key claude-gravity-mode-map (kbd "s") 'claude-gravity-send-prompt)
(define-key claude-gravity-mode-map (kbd "/") 'claude-gravity-slash-command)
(define-key claude-gravity-mode-map (kbd "S") 'claude-gravity-start-session)
(define-key claude-gravity-mode-map (kbd "r") 'claude-gravity-resume-session)
(define-key claude-gravity-mode-map (kbd "$") 'claude-gravity-terminal-session)

(provide 'claude-gravity)
;;; claude-gravity.el ends here
