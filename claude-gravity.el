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
                           :cwd (or cwd "")
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
      (claude-gravity--render-session-buffer session))))

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
;;   >  normal prompt
;;   ?  question (AskUserQuestion)
;;   →  phase boundary (ExitPlanMode)
;;
;; permission_mode (from Claude Code hook payloads) is stored on tool
;; entries and the session for display purposes but is NOT used as a
;; turn boundary signal.

(defun claude-gravity-handle-event (event session-id cwd data &optional pid)
  "Handle EVENT for SESSION-ID (with CWD) carrying DATA.
Optional PID is the Claude Code process ID."
  (unless session-id
    (setq session-id "legacy")
    (setq cwd (or cwd "")))
  (message "Handling event: %s for session: %s" event session-id)
  ;; Update PID, last-event-time, and slug on every event
  (let ((existing (claude-gravity--get-session session-id)))
    (when existing
      (plist-put existing :last-event-time (current-time))
      (when (and pid (numberp pid) (> pid 0))
        (plist-put existing :pid pid))
      ;; Store slug from transcript (arrives via bridge enrichment)
      (let ((slug (alist-get 'slug data)))
        (when (and slug (stringp slug) (not (plist-get existing :slug)))
          (let ((old-buf (plist-get existing :buffer)))
            (plist-put existing :slug slug)
            ;; Rename session buffer if it exists
            (when (and old-buf (buffer-live-p old-buf))
              (with-current-buffer old-buf
                (rename-buffer (claude-gravity--session-buffer-name existing) t))))))))
  (pcase event
    ("SessionStart"
     (let ((existing (claude-gravity--get-session session-id)))
       (when existing
         (claude-gravity--reset-session existing)))
     (claude-gravity--ensure-session session-id cwd))

    ("SessionEnd"
     (let ((session (claude-gravity--get-session session-id)))
       (when session
         (plist-put session :status 'ended))))

    ("UserPromptSubmit"
     (let* ((session (claude-gravity--ensure-session session-id cwd))
            (prompt-text (alist-get 'prompt data)))
       (when prompt-text
         (let ((entry (list (cons 'text prompt-text)
                            (cons 'submitted (current-time))
                            (cons 'elapsed nil))))
           (plist-put session :prompts
                      (vconcat (plist-get session :prompts) (vector entry))))
         (plist-put session :current-turn
                    (1+ (or (plist-get session :current-turn) 0))))
       (plist-put session :claude-status 'responding)))

    ("Stop"
     (let ((session (claude-gravity--get-session session-id)))
       (when session
         (plist-put session :claude-status 'idle)
         ;; Compute elapsed time for the last prompt
         (let* ((prompts (plist-get session :prompts))
                (len (length prompts)))
           (when (> len 0)
             (let ((last-prompt (aref prompts (1- len))))
               (when (and (listp last-prompt)
                          (not (alist-get 'elapsed last-prompt))
                          (alist-get 'submitted last-prompt))
                 (setf (alist-get 'elapsed last-prompt)
                       (float-time (time-subtract (current-time)
                                                  (alist-get 'submitted last-prompt)))))
               ;; Store trailing assistant text (conclusion after last tool)
               ;; Note: setf alist-get on new keys conses a new head, so we
               ;; must write the updated alist back into the vector slot.
               (let ((stop-text (alist-get 'stop_text data))
                     (stop-thinking (alist-get 'stop_thinking data)))
                 (when stop-text
                   (setf (alist-get 'stop_text last-prompt) stop-text))
                 (when stop-thinking
                   (setf (alist-get 'stop_thinking last-prompt) stop-thinking))
                 (when (or stop-text stop-thinking)
                   (aset prompts (1- len) last-prompt)))))))))

    ("SubagentStart"
     (let* ((session (claude-gravity--ensure-session session-id cwd))
            (agents (plist-get session :agents))
            (new-agent (list (cons 'agent_id (alist-get 'agent_id data))
                             (cons 'type (alist-get 'agent_type data))
                             (cons 'status "running")
                             (cons 'timestamp (current-time))
                             (cons 'turn (or (plist-get session :current-turn) 0)))))
       (plist-put session :agents (vconcat agents (vector new-agent)))))

    ("SubagentStop"
     (let* ((session (claude-gravity--ensure-session session-id cwd))
            (agents (plist-get session :agents))
            (agent-id (alist-get 'agent_id data))
            (idx (claude-gravity--find-agent-by-id agents agent-id)))
       (when idx
         (let ((agent (aref agents idx)))
           (setf (alist-get 'status agent) "done")
           (let ((ts (alist-get 'timestamp agent)))
             (when ts
               (setf (alist-get 'duration agent)
                     (float-time (time-subtract (current-time) ts)))))
           (let ((tp (alist-get 'agent_transcript_path data)))
             (when tp
               (setf (alist-get 'transcript_path agent) tp)))
           (aset agents idx agent)))))

    ("PreToolUse"
     (let* ((session (claude-gravity--ensure-session session-id cwd))
            (state (plist-get session :state))
            (tools (alist-get 'tools state))
            (new-tool (list (cons 'tool_use_id (alist-get 'tool_use_id data))
                          (cons 'name (alist-get 'tool_name data))
                          (cons 'input (alist-get 'tool_input data))
                          (cons 'status "running")
                          (cons 'timestamp (current-time))
                          (cons 'turn (or (plist-get session :current-turn) 0))
                          (cons 'permission_mode (alist-get 'permission_mode data))
                          (cons 'assistant_text (alist-get 'assistant_text data))
                          (cons 'assistant_thinking (alist-get 'assistant_thinking data)))))
       (setf (alist-get 'tools state) (vconcat tools (vector new-tool)))
       (plist-put session :permission-mode (alist-get 'permission_mode data))
       (plist-put session :claude-status 'responding)
       (claude-gravity--track-file session (alist-get 'tool_name data) (alist-get 'tool_input data))
       (claude-gravity--track-task session "PreToolUse" (alist-get 'tool_name data)
                                   (alist-get 'tool_input data) (alist-get 'tool_use_id data))
       ;; Add AskUserQuestion as a prompt entry
       (when (equal (alist-get 'tool_name data) "AskUserQuestion")
         (let* ((input (alist-get 'tool_input data))
                (questions (alist-get 'questions input))
                (first-q (and (vectorp questions) (> (length questions) 0)
                              (aref questions 0)))
                (q-text (and first-q (alist-get 'question first-q))))
           (when q-text
             (let ((entry (list (cons 'text q-text)
                                (cons 'type 'question)
                                (cons 'tool_use_id (alist-get 'tool_use_id data))
                                (cons 'submitted (current-time))
                                (cons 'elapsed nil)
                                (cons 'answer nil))))
               (plist-put session :prompts
                          (vconcat (plist-get session :prompts) (vector entry)))
               ;; Advance turn — question is a phase boundary
               (plist-put session :current-turn
                          (1+ (or (plist-get session :current-turn) 0)))))))))

    ("PostToolUse"
     (let* ((session (claude-gravity--ensure-session session-id cwd))
            (state (plist-get session :state))
            (tools (alist-get 'tools state))
            (tool-use-id (alist-get 'tool_use_id data))
            (idx (claude-gravity--find-tool-by-id tools tool-use-id)))
       (when idx
         (let ((tool (aref tools idx)))
           (setf (alist-get 'status tool) "done")
           (setf (alist-get 'result tool) (alist-get 'tool_response data))
           (aset tools idx tool)))
       ;; Track file operations
       (claude-gravity--track-file session (alist-get 'tool_name data) (alist-get 'tool_input data))
       ;; Track task operations
       (claude-gravity--track-task session "PostToolUse" (alist-get 'tool_name data)
                                   (alist-get 'tool_input data) (alist-get 'tool_use_id data)
                                   (alist-get 'tool_response data))
       ;; Store AskUserQuestion answer in the prompt entry
       (when (equal (alist-get 'tool_name data) "AskUserQuestion")
         (let* ((prompts (plist-get session :prompts))
                (tid (alist-get 'tool_use_id data))
                (response (alist-get 'tool_response data)))
           (when (and prompts tid)
             (dotimes (i (length prompts))
               (let ((p (aref prompts i)))
                 (when (and (equal (alist-get 'type p) 'question)
                            (equal (alist-get 'tool_use_id p) tid))
                   (setf (alist-get 'answer p)
                         (claude-gravity--extract-ask-answer response))
                   (setf (alist-get 'elapsed p)
                         (float-time (time-subtract (current-time)
                                                    (alist-get 'submitted p))))
                   (aset prompts i p)))))))
       ;; Detect plan presentation
       (let ((tool-name (alist-get 'tool_name data)))
         (when (equal tool-name "ExitPlanMode")
           (let ((plan-content (alist-get 'plan (alist-get 'tool_input data)))
                 (file-path (alist-get 'filePath (alist-get 'tool_response data)))
                 (allowed-prompts (alist-get 'allowedPrompts (alist-get 'tool_input data))))
             (when plan-content
               (plist-put session :plan (list :content plan-content
                                              :file-path file-path
                                              :allowed-prompts (append allowed-prompts nil)))))
           ;; Advance turn — plan approval is a phase boundary
           (let ((entry (list (cons 'text "[Plan approved]")
                              (cons 'type 'phase-boundary)
                              (cons 'submitted (current-time))
                              (cons 'elapsed nil))))
             (plist-put session :prompts
                        (vconcat (plist-get session :prompts) (vector entry)))
             (plist-put session :current-turn
                        (1+ (or (plist-get session :current-turn) 0))))))))

    ("Notification"
     (let* ((session (claude-gravity--get-session session-id))
            (msg (or (alist-get 'message data) "")))
       (when (and session (string-match-p "\\(?:reset\\|clear\\)" msg))
         (claude-gravity--reset-session session)))))

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

(defface claude-gravity-header-title
  '((t :weight bold :foreground "white"))
  "Face for the main buffer header title."
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

(defun claude-gravity--insert-wrapped-with-margin (text indent-or-nil face)
  "Insert TEXT with word-wrap and a ┊ margin indicator.
INDENT-OR-NIL and FACE work like `claude-gravity--insert-wrapped'."
  (when (and text (not (string-empty-p text)))
    (let* ((indent (or indent-or-nil
                       (* (claude-gravity--section-depth) claude-gravity--indent-step)))
           (margin (propertize "┊ " 'face 'claude-gravity-margin-indicator))
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

;;; Tool display helpers

(defun claude-gravity--extract-ask-answer (response)
  "Extract the user's answer text from AskUserQuestion RESPONSE."
  (cond
   ;; MCP-style vector result
   ((vectorp response)
    (let* ((first (and (> (length response) 0) (aref response 0)))
           (text (and (listp first) (alist-get 'text first))))
      text))
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
    (_
     name)))

(defun claude-gravity--insert-tool-detail (name input result)
  "Insert expanded detail for tool NAME with INPUT and RESULT."
  (pcase name
    ("Bash"
     (let ((cmd (alist-get 'command input)))
       (when cmd
         (claude-gravity--insert-label "Command: ")
         (claude-gravity--insert-wrapped cmd nil))))
    ("Read"
     (let ((path (alist-get 'file_path input)))
       (when path
         (claude-gravity--insert-label "File: ")
         (claude-gravity--insert-wrapped path nil))))
    ((or "Edit" "Write")
     (let ((path (alist-get 'file_path input)))
       (when path
         (claude-gravity--insert-label "File: ")
         (claude-gravity--insert-wrapped path nil))))
    ((or "Grep" "Glob")
     (let ((pattern (alist-get 'pattern input))
           (path (alist-get 'path input)))
       (when pattern
         (claude-gravity--insert-label "Pattern: ")
         (claude-gravity--insert-wrapped pattern nil))
       (when path
         (claude-gravity--insert-label "Path: ")
         (claude-gravity--insert-wrapped path nil))))
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
        (display-buffer buf '(display-buffer-in-side-window
                              (side . right)
                              (window-width . 0.4)))))))

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

;;; Section renderers (used by per-session buffers)

(defun claude-gravity-insert-header (state)
  "Insert header section showing tool count from STATE."
  (let* ((tool-count (length (alist-get 'tools state)))
         (width (max 40 (- (or (window-width) 80) 2)))
         (top-line (make-string width ?━)))
    (magit-insert-section (header)
      (insert (propertize top-line 'face 'claude-gravity-divider) "\n")
      (magit-insert-heading
        (format "%s%s"
                (propertize "Structured Claude Session" 'face 'claude-gravity-header-title)
                (propertize (format "  ◆ %d tools" tool-count) 'face 'claude-gravity-detail-label)))
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
  "Format count string for TOOLS, AGENTS, TASKS lists."
  (let ((parts nil))
    (when (and tasks (> (length tasks) 0))
      (push (format "%dt" (length tasks)) parts))
    (when (and agents (> (length agents) 0))
      (push (format "%da" (length agents)) parts))
    (when (and tools (> (length tools) 0))
      (push (format "%d tools" (length tools)) parts))
    (if parts
        (propertize (format "[%s]" (string-join parts " "))
                    'face 'claude-gravity-detail-label)
      "")))

(defun claude-gravity--dedup-assistant-text (tools)
  "Remove duplicate assistant_text and assistant_thinking on consecutive TOOLS.
Parallel tool calls get the same preceding content; only the first should display it."
  (let ((prev-text nil)
        (prev-thinking nil))
    (dolist (item tools)
      (let ((atext (alist-get 'assistant_text item))
            (athink (alist-get 'assistant_thinking item)))
        (when (and atext (equal atext prev-text))
          (setf (alist-get 'assistant_text item) nil))
        (when (and athink (equal athink prev-thinking))
          (setf (alist-get 'assistant_thinking item) nil))
        (setq prev-text atext
              prev-thinking athink)))))

(defun claude-gravity--insert-tool-context (item)
  "Insert thinking and assistant text from ITEM as standalone interlaced lines.
Called before each tool item in the rendering loop."
  (let ((athink (alist-get 'assistant_thinking item))
        (atext (alist-get 'assistant_text item)))
    (when (and athink (not (string-empty-p athink)))
      (claude-gravity--insert-wrapped-with-margin
       athink nil 'claude-gravity-thinking))
    (when (and atext (not (string-empty-p atext)))
      (claude-gravity--insert-wrapped-with-margin atext nil 'claude-gravity-thinking))))

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
                 (heading-pair (claude-gravity--response-cycle-heading cycle))
                 (heading (car heading-pair))
                 (heading-face (cdr heading-pair)))
            ;; Render first tool's assistant context outside the collapsible
            ;; section so it stays visible when collapsed.
            (claude-gravity--insert-tool-context (car cycle))
            ;; Response cycle as a collapsible section (tools only)
            (magit-insert-section (response-cycle cycle-idx should-collapse)
              (magit-insert-heading
                (format "%s%s"
                        (claude-gravity--indent)
                        (propertize (concat "┊ " heading) 'face heading-face)))
              ;; First tool (context already rendered above)
              (claude-gravity--insert-tool-item (car cycle) agent-lookup)
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
    ;; Only render if there is any content
    (when (> max-turn 0)
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
                (propertize "Pre-prompt activity" 'face 'claude-gravity-detail-label))
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
                                   (propertize ">" 'face 'claude-gravity-prompt))))
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
                (let ((indent (claude-gravity--indent)))
                  (insert (format "%s%s  " indent indicator))
                  (claude-gravity--insert-wrapped prompt-text nil prompt-face)
                  (when (and is-question answer)
                    (insert indent "  ")
                    (claude-gravity--insert-label "Answer: ")
                    (claude-gravity--insert-wrapped answer nil 'claude-gravity-question))))
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
                (let ((stop-think (and (listp prompt-entry) (alist-get 'stop_thinking prompt-entry)))
                      (stop-text (and (listp prompt-entry) (alist-get 'stop_text prompt-entry))))
                  (when (and stop-think (not (string-empty-p stop-think)))
                    (claude-gravity--insert-wrapped-with-margin
                     stop-think nil 'claude-gravity-thinking))
                  (when (and stop-text (not (string-empty-p stop-text)))
                    (claude-gravity--insert-wrapped-with-margin stop-text nil 'claude-gravity-assistant-text))))))))
        (insert "\n")))))

(defun claude-gravity--insert-tool-item (item &optional agent-lookup)
  "Insert a single tool ITEM as a magit-section.
When AGENT-LOOKUP is provided and ITEM is a Task tool, annotate with agent info."
  (let* ((name (alist-get 'name item))
         (status (alist-get 'status item))
         (input (alist-get 'input item))
         (result (alist-get 'result item))
         (done-p (equal status "done"))
         (indicator (propertize (if done-p "[x]" "[/]")
                                'face (if done-p
                                           'claude-gravity-tool-done
                                         'claude-gravity-tool-running)))
         (tool-face (propertize (or name "?") 'face 'claude-gravity-tool-name))
         (summary (claude-gravity--tool-summary name input))
         (desc (claude-gravity--tool-description input))
         ;; Agent annotation for Task tools
         (agent (when (and agent-lookup (equal name "Task"))
                  (claude-gravity--pop-matching-agent
                   agent-lookup
                   (alist-get 'turn item)
                   (alist-get 'subagent_type input))))
         (agent-suffix
          (if agent
              (let* ((atype (alist-get 'type agent))
                     (aid (alist-get 'agent_id agent))
                     (short-id (if (and aid (> (length aid) 7))
                                   (substring aid 0 7)
                                 (or aid "?")))
                     (adur (alist-get 'duration agent))
                     (astatus (alist-get 'status agent))
                     (dur-str (if (and (equal astatus "done") adur)
                                  (format " %s" (claude-gravity--format-duration adur))
                                "")))
                (format "  %s %s (%s)%s"
                        (propertize "→" 'face 'claude-gravity-detail-label)
                        (propertize (or atype "?") 'face 'claude-gravity-tool-name)
                        (propertize short-id 'face 'claude-gravity-detail-label)
                        dur-str))
            "")))
    (let ((section-start (point)))
      (magit-insert-section (tool item t)
        (magit-insert-heading
          (if desc
              (format "%s%s %s%s\n%s%s %s"
                      (claude-gravity--indent)
                      indicator
                      (propertize desc 'face 'claude-gravity-tool-description)
                      agent-suffix
                      (claude-gravity--indent 2)
                      (propertize (or name "?") 'face 'claude-gravity-tool-signature)
                      (propertize summary 'face 'claude-gravity-tool-signature))
            (format "%s%s %s  %s%s"
                    (claude-gravity--indent)
                    indicator
                    tool-face
                    (propertize summary 'face 'claude-gravity-detail-label)
                    agent-suffix)))
        ;; Show permission-format signature in detail
        (let ((sig (claude-gravity--tool-signature name input)))
          (claude-gravity--insert-wrapped sig nil 'claude-gravity-tool-signature))
        (claude-gravity--insert-tool-detail name input result)
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
        (add-face-text-property section-start (point) 'claude-gravity-running-bg)))))


(defun claude-gravity--task-sort-key (status)
  "Return sort key for task STATUS.  Lower = higher priority."
  (pcase status
    ("in_progress" 0)
    ("pending" 1)
    ("completed" 2)
    (_ 3)))

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
          (format "%s%s %s  (%s)%s"
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
            (let* ((active-count 0)
                   (width (max 40 (- (or (window-width) 80) 2)))
                   (top-line (make-string width ?━)))
              (maphash (lambda (_k s)
                         (when (eq (plist-get s :status) 'active)
                           (cl-incf active-count)))
                       claude-gravity--sessions)
              (magit-insert-section (header)
                (insert (propertize top-line 'face 'claude-gravity-divider) "\n")
                (magit-insert-heading
                  (format "%s%s"
                          (propertize "Structured Claude Sessions" 'face 'claude-gravity-header-title)
                          (propertize (format "  ◆ %d sessions" active-count) 'face 'claude-gravity-detail-label)))
                (insert (propertize top-line 'face 'claude-gravity-divider) "\n\n")))
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
                       (magit-insert-section (session-entry sid)
                         (magit-insert-heading
                           (format "%s%s %s  %s  [%d tools]"
                                   (claude-gravity--indent)
                                   indicator label
                                   (or status-label "")
                                   n-tools)))))))
               projects)))
          (goto-char (min pos (point-max)))
          (claude-gravity--apply-visibility))))))

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
            (claude-gravity-insert-header state)
            (claude-gravity-insert-plan session)
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

(define-derived-mode claude-gravity-mode magit-section-mode "StructuredSessions"
  "Major mode for Structured Claude Sessions overview.

\\{claude-gravity-mode-map}"
  (font-lock-mode -1)
  (visual-line-mode 1))

(define-key claude-gravity-mode-map (kbd "T") 'claude-gravity-view-agent-transcript)
(define-key claude-gravity-mode-map (kbd "V") 'claude-gravity-open-agent-transcript)

(define-derived-mode claude-gravity-session-mode claude-gravity-mode "StructuredSession"
  "Major mode for a single Structured Claude Session buffer.")

(defun claude-gravity-visit-or-toggle ()
  "If on a session entry, open it.  On an agent, parse transcript.  Otherwise toggle."
  (interactive)
  (let ((section (magit-current-section)))
    (cond
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
   ("P" "Show Plan" claude-gravity-show-plan)
   ("F" "Open plan file" claude-gravity-open-plan-file)
   ("T" "Parse agent transcript" claude-gravity-view-agent-transcript)
   ("V" "Open agent transcript file" claude-gravity-open-agent-transcript)]
  ["Permissions"
   ("A" "Copy allow pattern" claude-gravity-add-allow-pattern)
   ("a" "Add to settings" claude-gravity-add-allow-pattern-to-settings)]
  ["Sessions"
   ("D" "Remove ended sessions" claude-gravity-cleanup-sessions)
   ("R" "Reset all status to idle" claude-gravity-reset-status)
   ("X" "Detect dead sessions" claude-gravity-detect-dead-sessions)
   ("d" "Delete session at point" claude-gravity-delete-session)]
  ["Manage"
   ("q" "Quit" bury-buffer)])

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
  (let ((count 0))
    (maphash
     (lambda (_id session)
       (when (eq (plist-get session :status) 'active)
         (let ((pid (plist-get session :pid))
               (last-event (plist-get session :last-event-time)))
           (cond
            ;; PID known: check if process is alive
            ((and pid (numberp pid) (> pid 0))
             (unless (claude-gravity--process-alive-p pid)
               (plist-put session :status 'ended)
               (cl-incf count)))
            ;; No PID, has last-event: use staleness (>5 min since last event)
            ((and last-event
                  (> (float-time (time-subtract (current-time) last-event)) 300))
             (plist-put session :status 'ended)
             (cl-incf count))
            ;; No PID, no last-event: legacy session with no way to verify
            ((null last-event)
             (plist-put session :status 'ended)
             (cl-incf count))))))
     claude-gravity--sessions)
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
                      (payload (alist-get 'data data)))
                  (when event
                    (claude-gravity-handle-event event session-id cwd payload pid))))
            (error
             (message "Claude Gravity JSON error: %s" err))))))
    ;; Store any remaining partial data
    (process-put proc 'claude-gravity--buffer buf)))

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

(provide 'claude-gravity)
;;; claude-gravity.el ends here
