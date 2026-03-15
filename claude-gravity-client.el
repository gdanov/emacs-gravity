;;; claude-gravity-client.el --- Client connection to gravity-server  -*- lexical-binding: t; -*-

;; Client that connects to gravity-server's terminal socket.
;; Receives session snapshots and semantic patches, maintains
;; local read-replica of session state for rendering.
;; Sends user actions (permission responses, plan review) back to server.

;;; Code:

(require 'claude-gravity-core)
(require 'claude-gravity-session)
(require 'claude-gravity-state)

;; Forward declarations
(declare-function claude-gravity--render-overview "claude-gravity-ui")
(declare-function claude-gravity--session-buffer-name "claude-gravity-ui")
(declare-function claude-gravity--tool-signature "claude-gravity-diff")
(declare-function claude-gravity-tail "claude-gravity-ui")
(declare-function claude-gravity--inbox-act-permission "claude-gravity-actions")
(declare-function claude-gravity--inbox-act-question "claude-gravity-actions")
(declare-function claude-gravity--inbox-act-plan-review "claude-gravity-actions")
(declare-function claude-gravity--inbox-act-idle "claude-gravity-actions")
(declare-function claude-gravity--wrap-cache-clear "claude-gravity-text")

;;; Server process management

(defvar claude-gravity--backend-process nil
  "The gravity-server subprocess, or nil if not managed by Emacs.")

(defvar claude-gravity--client-process nil
  "Network connection to gravity-server's terminal socket.")

(defvar claude-gravity--client-buffer ""
  "Accumulation buffer for partial JSON from server.")

(defvar claude-gravity--client-reconnect-timer nil
  "Timer for reconnection attempts.")

(defvar claude-gravity--client-subscribed-sessions (make-hash-table :test 'equal)
  "Set of session IDs we've requested detail for.")

(defvar claude-gravity-server-terminal-sock
  (expand-file-name
   (or (getenv "GRAVITY_TERMINAL_SOCK")
       "gravity-terminal.sock")
   (or (getenv "GRAVITY_SOCK_DIR")
       (expand-file-name ".local/state" (getenv "HOME"))))
  "Path to gravity-server's terminal socket.")

(defvar claude-gravity-server-hook-sock
  (expand-file-name
   (or (getenv "GRAVITY_HOOK_SOCK")
       "gravity-hooks.sock")
   (or (getenv "GRAVITY_SOCK_DIR")
       (expand-file-name ".local/state" (getenv "HOME"))))
  "Path to gravity-server's hook socket (for bridge reference).")

(defvar claude-gravity--server-bin nil
  "Path to the gravity-server entry point.
Set automatically from package directory.")

;;; Notification mode-line indicator (shared with socket.el)

(defvar claude-gravity--notification-indicator ""
  "Mode-line string showing Claude notification status.")

(put 'claude-gravity--notification-indicator 'risky-local-variable t)


;;; ── Server lifecycle ────────────────────────────────────────────────

(defun claude-gravity-server-start ()
  "Start gravity-server and connect to its terminal socket.
If server is already running (socket exists), just connect."
  (interactive)
  ;; Determine server binary path
  (unless claude-gravity--server-bin
    (setq claude-gravity--server-bin
          (expand-file-name "packages/gravity-server/src/server.ts"
                            (file-name-directory
                             (or load-file-name buffer-file-name
                                 default-directory)))))
  ;; Start server if socket doesn't exist
  (unless (file-exists-p claude-gravity-server-terminal-sock)
    (claude-gravity--start-backend))
  ;; Connect to terminal socket
  (claude-gravity--client-connect)
  ;; Register mode-line indicator
  (unless (memq 'claude-gravity--notification-indicator global-mode-string)
    (if (or (null global-mode-string) (equal global-mode-string '("")))
        (setq global-mode-string '("" claude-gravity--notification-indicator))
      (push 'claude-gravity--notification-indicator (cdr (last global-mode-string)))))
  ;; Window resize hook
  (add-hook 'window-size-change-functions
            (lambda (_frame) (claude-gravity--wrap-cache-clear)))
  (claude-gravity--log 'info "Gravity client started"))

(defun claude-gravity-server-stop ()
  "Disconnect from gravity-server and optionally stop the backend."
  (interactive)
  (when claude-gravity--client-reconnect-timer
    (cancel-timer claude-gravity--client-reconnect-timer)
    (setq claude-gravity--client-reconnect-timer nil))
  (when (and claude-gravity--client-process
             (process-live-p claude-gravity--client-process))
    (delete-process claude-gravity--client-process))
  (setq claude-gravity--client-process nil)
  (setq claude-gravity--client-buffer "")
  ;; Stop backend if we started it
  (when (and claude-gravity--backend-process
             (process-live-p claude-gravity--backend-process))
    (interrupt-process claude-gravity--backend-process)
    (setq claude-gravity--backend-process nil))
  (claude-gravity--log 'info "Gravity client stopped"))

(defun claude-gravity-server-alive-p ()
  "Return non-nil if the client is connected to gravity-server."
  (and claude-gravity--client-process
       (process-live-p claude-gravity--client-process)))

(defun claude-gravity--start-backend ()
  "Start gravity-server as a subprocess."
  (when (and claude-gravity--backend-process
             (process-live-p claude-gravity--backend-process))
    (interrupt-process claude-gravity--backend-process))
  (let ((tsx (executable-find "tsx")))
    (unless tsx
      (error "tsx not found. Install with: npm install -g tsx"))
    (setq claude-gravity--backend-process
          (make-process
           :name "gravity-server"
           :buffer "*gravity-server*"
           :command (list tsx claude-gravity--server-bin)
           :noquery t
           :sentinel #'claude-gravity--backend-sentinel))
    (claude-gravity--log 'info "Started gravity-server: %s" claude-gravity--server-bin)
    ;; Wait briefly for socket to appear
    (let ((attempts 0))
      (while (and (< attempts 20)
                  (not (file-exists-p claude-gravity-server-terminal-sock)))
        (sleep-for 0.1)
        (cl-incf attempts))
      (unless (file-exists-p claude-gravity-server-terminal-sock)
        (claude-gravity--log 'warn "Server socket not ready after 2s")))))

(defun claude-gravity--backend-sentinel (proc event)
  "Handle gravity-server process state changes.
PROC is the process, EVENT is the status change."
  (let ((trimmed (string-trim event)))
    (claude-gravity--log 'warn "gravity-server: %s" trimmed)
    (when (and (not (string-prefix-p "open" trimmed))
               (not (process-live-p proc)))
      ;; Server died — schedule reconnect
      (claude-gravity--schedule-reconnect))))


;;; ── Client connection ───────────────────────────────────────────────

(defun claude-gravity--client-connect ()
  "Connect to gravity-server's terminal socket."
  (when (and claude-gravity--client-process
             (process-live-p claude-gravity--client-process))
    (delete-process claude-gravity--client-process))
  (condition-case err
      (progn
        (setq claude-gravity--client-buffer "")
        (setq claude-gravity--client-process
              (make-network-process
               :name "gravity-client"
               :family 'local
               :service claude-gravity-server-terminal-sock
               :filter #'claude-gravity--client-filter
               :sentinel #'claude-gravity--client-sentinel
               :noquery t))
        (claude-gravity--log 'info "Connected to gravity-server at %s"
                             claude-gravity-server-terminal-sock)
        ;; Request overview on connect
        (claude-gravity--send-to-server
         '((type . "request.overview"))))
    (error
     (claude-gravity--log 'error "Failed to connect to gravity-server: %s" err)
     (claude-gravity--schedule-reconnect))))

(defun claude-gravity--client-sentinel (_proc event)
  "Handle client connection state changes."
  (let ((trimmed (string-trim event)))
    (cond
     ((string= "connection broken by remote peer" trimmed)
      (claude-gravity--log 'warn "Lost connection to gravity-server")
      (claude-gravity--schedule-reconnect))
     ((string-prefix-p "deleted" trimmed)
      nil) ; Intentional disconnect
     (t
      (claude-gravity--log 'warn "Client connection event: %s" trimmed)))))

(defun claude-gravity--schedule-reconnect ()
  "Schedule a reconnection attempt in 2 seconds."
  (when claude-gravity--client-reconnect-timer
    (cancel-timer claude-gravity--client-reconnect-timer))
  (setq claude-gravity--client-reconnect-timer
        (run-with-timer 2 nil
                        (lambda ()
                          (setq claude-gravity--client-reconnect-timer nil)
                          (when (file-exists-p claude-gravity-server-terminal-sock)
                            (claude-gravity--client-connect))))))


;;; ── Message sending ─────────────────────────────────────────────────

(defun claude-gravity--send-to-server (msg)
  "Send MSG alist as JSON to gravity-server."
  (when (and claude-gravity--client-process
             (process-live-p claude-gravity--client-process))
    (let ((json (concat (json-serialize msg) "\n")))
      (process-send-string claude-gravity--client-process json))))

(defun claude-gravity--request-session (session-id)
  "Subscribe to session detail updates for SESSION-ID."
  (unless (gethash session-id claude-gravity--client-subscribed-sessions)
    (puthash session-id t claude-gravity--client-subscribed-sessions)
    (claude-gravity--send-to-server
     `((type . "request.session")
       (sessionId . ,session-id)))))

(defun claude-gravity--send-permission-response (item-id decision &optional message)
  "Send permission response for inbox ITEM-ID with DECISION and optional MESSAGE."
  (claude-gravity--send-to-server
   `((type . "action.permission")
     (itemId . ,item-id)
     (decision . ,decision)
     ,@(when message `((message . ,message))))))

(defun claude-gravity--send-question-response (item-id answers)
  "Send question response for inbox ITEM-ID with ANSWERS list."
  (claude-gravity--send-to-server
   `((type . "action.question")
     (itemId . ,item-id)
     (answers . ,(vconcat answers)))))

(defun claude-gravity--send-plan-review-response (item-id decision &optional feedback)
  "Send plan review response for inbox ITEM-ID with DECISION and optional FEEDBACK."
  (claude-gravity--send-to-server
   `((type . "action.plan-review")
     (itemId . ,item-id)
     (decision . ,decision)
     ,@(when feedback `((feedback . ,feedback))))))

(defun claude-gravity--send-turn-auto-approve (session-id)
  "Enable turn-scoped auto-approve for SESSION-ID."
  (claude-gravity--send-to-server
   `((type . "action.turn-auto-approve")
     (sessionId . ,session-id))))


;;; ── Message receiving ───────────────────────────────────────────────

(defun claude-gravity--client-filter (_proc string)
  "Process incoming messages from gravity-server.
Accumulates partial data and processes complete newline-delimited JSON."
  (setq claude-gravity--client-buffer
        (concat claude-gravity--client-buffer string))
  (while (string-match "\n" claude-gravity--client-buffer)
    (let ((line (substring claude-gravity--client-buffer 0 (match-beginning 0))))
      (setq claude-gravity--client-buffer
            (substring claude-gravity--client-buffer (match-end 0)))
      (when (> (length line) 0)
        (condition-case err
            (let ((msg (json-parse-string line :object-type 'alist :array-type 'list)))
              (claude-gravity--handle-server-message msg))
          (error
           (claude-gravity--log 'error "Client JSON parse error: %s" err)))))))

(defun claude-gravity--handle-server-message (msg)
  "Dispatch a server message MSG to the appropriate handler."
  (let ((type (alist-get 'type msg)))
    (pcase type
      ("session.snapshot"
       (claude-gravity--handle-session-snapshot msg))
      ("session.update"
       (claude-gravity--handle-session-update msg))
      ("session.removed"
       (claude-gravity--handle-session-removed msg))
      ("inbox.added"
       (claude-gravity--handle-inbox-added msg))
      ("inbox.removed"
       (claude-gravity--handle-inbox-removed msg))
      ("overview.snapshot"
       (claude-gravity--handle-overview-snapshot msg))
      (_
       (claude-gravity--log 'warn "Unknown server message type: %s" type)))))


;;; ── JSON null handling ──────────────────────────────────────────────
;;
;; json-parse-string converts JSON null → :null and false → :false.
;; We need Emacs nil for both.

(defsubst claude-gravity--jnil (val)
  "Convert JSON null/false to Emacs nil.  Pass through other values."
  (if (memq val '(:null :false nil)) nil val))

;;; ── Session snapshot → plist conversion ─────────────────────────────
;;
;; The server sends Session objects as JSON. We convert to the exact
;; plist/hash-table structure that claude-gravity-render.el already reads.
;; This is the key bridge: server state → Emacs view model.

(defun claude-gravity--json-session-to-plist (session-json)
  "Convert a JSON Session object to the Emacs session plist format.
SESSION-JSON is an alist from json-parse-string."
  (let* ((jnil #'claude-gravity--jnil)
         (session-id (alist-get 'sessionId session-json))
         (cwd (or (funcall jnil (alist-get 'cwd session-json)) ""))
         (project (or (funcall jnil (alist-get 'project session-json))
                      (file-name-nondirectory (directory-file-name cwd))))
         (status (intern (or (funcall jnil (alist-get 'status session-json)) "active")))
         (claude-status (intern (or (funcall jnil (alist-get 'claudeStatus session-json)) "idle")))
         ;; Build turn tree
         (turns-json (or (funcall jnil (alist-get 'turns session-json)) nil))
         (turns-tl (claude-gravity--tlist-new))
         ;; Build indexes
         (tool-index (make-hash-table :test 'equal))
         (agent-index (make-hash-table :test 'equal))
         (tasks-ht (make-hash-table :test 'equal))
         (files-ht (make-hash-table :test 'equal)))
    ;; Convert turns
    (dolist (turn-json turns-json)
      (let ((turn-node (claude-gravity--json-turn-to-alist turn-json)))
        (claude-gravity--tlist-append turns-tl turn-node)
        ;; Index tools in this turn
        (claude-gravity--index-turn-tools turn-node tool-index agent-index)))
    ;; Convert tasks
    (let ((tasks-json (funcall jnil (alist-get 'tasks session-json))))
      (when tasks-json
        (dolist (pair tasks-json)
          (let ((task-id (symbol-name (car pair)))
                (task-val (cdr pair)))
            (puthash task-id (claude-gravity--json-task-to-alist task-val) tasks-ht)))))
    ;; Convert files
    (let ((files-json (funcall jnil (alist-get 'files session-json))))
      (when files-json
        (dolist (pair files-json)
          (let ((path (symbol-name (car pair)))
                (entry (cdr pair)))
            (puthash path
                     (list (cons 'ops (or (funcall jnil (alist-get 'ops entry)) nil))
                           (cons 'last-touched (alist-get 'lastTouched entry)))
                     files-ht)))))
    ;; Build plist
    (list :session-id session-id
          :source "gravity-server"
          :cwd cwd
          :project project
          :status status
          :claude-status claude-status
          :slug (funcall jnil (alist-get 'slug session-json))
          :branch (funcall jnil (alist-get 'branch session-json))
          :pid (funcall jnil (alist-get 'pid session-json))
          :start-time (claude-gravity--epoch-to-time
                       (alist-get 'startTime session-json))
          :last-event-time (claude-gravity--epoch-to-time
                            (alist-get 'lastEventTime session-json))
          :token-usage (claude-gravity--json-token-usage
                        (funcall jnil (alist-get 'tokenUsage session-json)))
          :plan (claude-gravity--json-plan
                 (funcall jnil (alist-get 'plan session-json)))
          :streaming-text (funcall jnil (alist-get 'streamingText session-json))
          :permission-mode (funcall jnil (alist-get 'permissionMode session-json))
          :model-name (funcall jnil (alist-get 'modelName session-json))
          :turns turns-tl
          :current-turn (or (alist-get 'currentTurn session-json) 0)
          :tool-index tool-index
          :agent-index agent-index
          :tasks tasks-ht
          :files files-ht
          :total-tool-count (or (alist-get 'totalToolCount session-json) 0)
          :header-line-cache nil
          :buffer nil
          :display-name nil
          :ignored nil
          :allow-patterns nil)))

(defun claude-gravity--json-turn-to-alist (turn-json)
  "Convert a JSON TurnNode to turn alist."
  (let* ((jnil #'claude-gravity--jnil)
         (prompt-json (funcall jnil (alist-get 'prompt turn-json)))
         (steps-json (or (funcall jnil (alist-get 'steps turn-json)) nil))
         (agents-json (or (funcall jnil (alist-get 'agents turn-json)) nil))
         (tasks-json (or (funcall jnil (alist-get 'tasks turn-json)) nil))
         (steps-tl (claude-gravity--tlist-new))
         (agents-tl (claude-gravity--tlist-new)))
    ;; Convert steps
    (dolist (step-json steps-json)
      (claude-gravity--tlist-append steps-tl
                                    (claude-gravity--json-step-to-alist step-json)))
    ;; Convert agents
    (dolist (agent-json agents-json)
      (claude-gravity--tlist-append agents-tl
                                    (claude-gravity--json-agent-to-alist agent-json)))
    ;; Build turn alist
    (list (cons 'turn-number (or (alist-get 'turnNumber turn-json) 0))
          (cons 'prompt (when prompt-json
                          (claude-gravity--json-prompt-to-alist prompt-json)))
          (cons 'steps steps-tl)
          (cons 'agents agents-tl)
          (cons 'tasks (mapcar #'claude-gravity--json-task-to-alist tasks-json))
          (cons 'tool-count (or (alist-get 'toolCount turn-json) 0))
          (cons 'agent-count (or (alist-get 'agentCount turn-json) 0))
          (cons 'frozen (eq (alist-get 'frozen turn-json) t))
          (cons 'stop_text (funcall jnil (alist-get 'stopText turn-json)))
          (cons 'stop_thinking (funcall jnil (alist-get 'stopThinking turn-json)))
          (cons 'token-in (funcall jnil (alist-get 'tokenIn turn-json)))
          (cons 'token-out (funcall jnil (alist-get 'tokenOut turn-json))))))

(defun claude-gravity--json-step-to-alist (step-json)
  "Convert a JSON StepNode to step alist."
  (let* ((jnil #'claude-gravity--jnil)
         (tools-json (or (funcall jnil (alist-get 'tools step-json)) nil))
         (tools-tl (claude-gravity--tlist-new)))
    (dolist (tool-json tools-json)
      (claude-gravity--tlist-append tools-tl
                                    (claude-gravity--json-tool-to-alist tool-json)))
    (list (cons 'thinking (funcall jnil (alist-get 'thinking step-json)))
          (cons 'text (funcall jnil (alist-get 'text step-json)))
          (cons 'tools tools-tl))))

(defun claude-gravity--json-tool-to-alist (tool-json)
  "Convert a JSON Tool to tool alist."
  (let ((jnil #'claude-gravity--jnil))
    (list (cons 'tool_use_id (alist-get 'toolUseId tool-json))
          (cons 'name (alist-get 'name tool-json))
          (cons 'input (or (funcall jnil (alist-get 'input tool-json)) nil))
          (cons 'status (or (funcall jnil (alist-get 'status tool-json)) "running"))
          (cons 'result (funcall jnil (alist-get 'result tool-json)))
          (cons 'timestamp (claude-gravity--epoch-to-time
                            (alist-get 'timestamp tool-json)))
          (cons 'duration (funcall jnil (alist-get 'duration tool-json)))
          (cons 'turn (or (alist-get 'turn tool-json) 0))
          (cons 'assistant_text (funcall jnil (alist-get 'assistantText tool-json)))
          (cons 'assistant_thinking (funcall jnil (alist-get 'assistantThinking tool-json)))
          (cons 'post_text (funcall jnil (alist-get 'postText tool-json)))
          (cons 'post_thinking (funcall jnil (alist-get 'postThinking tool-json)))
          (cons 'parent_agent_id (funcall jnil (alist-get 'parentAgentId tool-json)))
          (cons 'ambiguous (eq (alist-get 'ambiguous tool-json) t))
          (cons 'candidate-agents (funcall jnil (alist-get 'candidateAgentIds tool-json)))
          (cons 'agent nil))))  ; agent link populated during indexing

(defun claude-gravity--json-agent-to-alist (agent-json)
  "Convert a JSON Agent to agent alist."
  (let* ((jnil #'claude-gravity--jnil)
         (steps-json (or (funcall jnil (alist-get 'steps agent-json)) nil))
         (steps-tl (claude-gravity--tlist-new)))
    (dolist (step-json steps-json)
      (claude-gravity--tlist-append steps-tl
                                    (claude-gravity--json-step-to-alist step-json)))
    (list (cons 'agent_id (alist-get 'agentId agent-json))
          (cons 'type (alist-get 'type agent-json))
          (cons 'status (or (funcall jnil (alist-get 'status agent-json)) "running"))
          (cons 'steps steps-tl)
          (cons 'tool-count (or (alist-get 'toolCount agent-json) 0))
          (cons 'stop_text (funcall jnil (alist-get 'stopText agent-json)))
          (cons 'stop_thinking (funcall jnil (alist-get 'stopThinking agent-json)))
          (cons 'duration (funcall jnil (alist-get 'duration agent-json)))
          (cons 'timestamp (claude-gravity--epoch-to-time
                            (alist-get 'timestamp agent-json)))
          (cons 'transcript_path (funcall jnil (alist-get 'transcriptPath agent-json)))
          (cons 'task-tool nil))))  ; linked during indexing

(defun claude-gravity--json-prompt-to-alist (prompt-json)
  "Convert a JSON PromptEntry to prompt alist."
  (let ((jnil #'claude-gravity--jnil))
    (list (cons 'type (intern (or (funcall jnil (alist-get 'type prompt-json)) "user")))
          (cons 'text (funcall jnil (alist-get 'text prompt-json)))
          (cons 'submitted (claude-gravity--epoch-to-time
                            (alist-get 'submitted prompt-json)))
          (cons 'elapsed (funcall jnil (alist-get 'elapsed prompt-json)))
          (cons 'tool_use_id (funcall jnil (alist-get 'toolUseId prompt-json)))
          (cons 'answer (funcall jnil (alist-get 'answer prompt-json))))))

(defun claude-gravity--json-task-to-alist (task-json)
  "Convert a JSON Task to task alist."
  (let ((jnil #'claude-gravity--jnil))
    (list (cons 'taskId (funcall jnil (alist-get 'taskId task-json)))
          (cons 'subject (funcall jnil (alist-get 'subject task-json)))
          (cons 'description (funcall jnil (alist-get 'description task-json)))
          (cons 'activeForm (funcall jnil (alist-get 'activeForm task-json)))
          (cons 'status (or (funcall jnil (alist-get 'status task-json)) "pending"))
          (cons 'turn (or (alist-get 'turn task-json) 0)))))

(defun claude-gravity--json-token-usage (usage-json)
  "Convert JSON TokenUsage to alist, or nil."
  (when usage-json
    (list (cons 'input_tokens (or (alist-get 'input_tokens usage-json) 0))
          (cons 'output_tokens (or (alist-get 'output_tokens usage-json) 0))
          (cons 'cache_read_input_tokens
                (or (alist-get 'cache_read_input_tokens usage-json) 0))
          (cons 'cache_creation_input_tokens
                (or (alist-get 'cache_creation_input_tokens usage-json) 0)))))

(defun claude-gravity--json-plan (plan-json)
  "Convert JSON Plan to plist, or nil."
  (when plan-json
    (list :content (alist-get 'content plan-json)
          :file-path (alist-get 'filePath plan-json)
          :allowed-prompts (alist-get 'allowedPrompts plan-json))))

(defun claude-gravity--epoch-to-time (epoch-ms)
  "Convert EPOCH-MS (milliseconds since epoch) to Emacs time value."
  (when (and epoch-ms (numberp epoch-ms) (> epoch-ms 0))
    (let ((secs (/ epoch-ms 1000))
          (usecs (* (mod epoch-ms 1000) 1000)))
      (time-add (seconds-to-time 0)
                (seconds-to-time (+ secs (/ usecs 1000000.0)))))))

(defun claude-gravity--index-turn-tools (turn-node tool-index agent-index)
  "Index all tools in TURN-NODE into TOOL-INDEX and AGENT-INDEX.
Also links agents to their Task tools."
  ;; Index root-level tools
  (dolist (step (claude-gravity--tlist-items (alist-get 'steps turn-node)))
    (dolist (tool (claude-gravity--tlist-items (alist-get 'tools step)))
      (let ((tid (alist-get 'tool_use_id tool)))
        (when tid (puthash tid tool tool-index)))))
  ;; Index agents and their tools
  (dolist (agent (claude-gravity--tlist-items (alist-get 'agents turn-node)))
    (let ((aid (alist-get 'agent_id agent)))
      (when aid (puthash aid agent agent-index))
      ;; Index agent's tools
      (dolist (step (claude-gravity--tlist-items (alist-get 'steps agent)))
        (dolist (tool (claude-gravity--tlist-items (alist-get 'tools step)))
          (let ((tid (alist-get 'tool_use_id tool)))
            (when tid (puthash tid tool tool-index)))))
      ;; Link agent to Task tool
      (let ((task-tid (alist-get 'taskToolUseId agent)))
        (when task-tid
          (let ((task-tool (gethash task-tid tool-index)))
            (when task-tool
              (nconc task-tool (list (cons 'agent agent)))
              (setf (alist-get 'task-tool agent) task-tool))))))))


;;; ── Snapshot handlers ───────────────────────────────────────────────

(defun claude-gravity--handle-session-snapshot (msg)
  "Handle a session.snapshot message.
MSG contains sessionId and full session state."
  (let* ((session-id (alist-get 'sessionId msg))
         (session-json (alist-get 'session msg))
         (session (claude-gravity--json-session-to-plist session-json))
         (existing (gethash session-id claude-gravity--sessions)))
    ;; Preserve buffer reference from existing session
    (when existing
      (let ((buf (plist-get existing :buffer)))
        (when (and buf (buffer-live-p buf))
          (plist-put session :buffer buf))))
    ;; Store session
    (puthash session-id session claude-gravity--sessions)
    ;; Refresh UI
    (claude-gravity--schedule-refresh)
    (claude-gravity--schedule-session-refresh session-id)
    (claude-gravity--log 'debug "Session snapshot received: %s" session-id)))


;;; ── Patch application ───────────────────────────────────────────────
;;
;; Each patch maps to a mutation on the local session plist.
;; After applying all patches, we schedule a render.

(defun claude-gravity--handle-session-update (msg)
  "Handle a session.update message containing semantic patches.
MSG contains sessionId and patches array."
  (let* ((session-id (alist-get 'sessionId msg))
         (patches (alist-get 'patches msg))
         (session (gethash session-id claude-gravity--sessions)))
    (unless session
      ;; Session not known locally — request snapshot
      (claude-gravity--request-session session-id)
      (cl-return-from claude-gravity--handle-session-update nil))
    ;; Apply each patch
    (dolist (patch patches)
      (claude-gravity--apply-patch session patch))
    ;; Schedule refresh
    (claude-gravity--schedule-refresh)
    (claude-gravity--schedule-session-refresh session-id)))

(defun claude-gravity--apply-patch (session patch)
  "Apply a single semantic PATCH to SESSION."
  (let ((op (alist-get 'op patch)))
    (pcase op
      ("set_status"
       (plist-put session :status
                  (intern (alist-get 'status patch))))

      ("set_claude_status"
       (plist-put session :claude-status
                  (intern (alist-get 'claudeStatus patch))))

      ("set_token_usage"
       (plist-put session :token-usage
                  (claude-gravity--json-token-usage (alist-get 'usage patch))))

      ("set_plan"
       (plist-put session :plan
                  (claude-gravity--json-plan (alist-get 'plan patch))))

      ("set_streaming_text"
       (plist-put session :streaming-text (alist-get 'text patch)))

      ("set_permission_mode"
       (plist-put session :permission-mode (alist-get 'mode patch)))

      ("set_meta"
       (let ((slug (alist-get 'slug patch))
             (branch (alist-get 'branch patch))
             (pid (alist-get 'pid patch))
             (model-name (alist-get 'modelName patch)))
         (when slug (plist-put session :slug slug))
         (when branch (plist-put session :branch branch))
         (when (and pid (numberp pid) (> pid 0))
           (plist-put session :pid pid))
         (when model-name (plist-put session :model-name model-name))
         (plist-put session :last-event-time (current-time))))

      ("add_turn"
       (let* ((turn-json (alist-get 'turn patch))
              (turn-node (claude-gravity--json-turn-to-alist turn-json)))
         (claude-gravity--tlist-append (plist-get session :turns) turn-node)
         (plist-put session :current-turn
                    (alist-get 'turn-number turn-node))))

      ("freeze_turn"
       (let* ((turn-num (alist-get 'turnNumber patch))
              (turn-node (claude-gravity--get-turn-node session turn-num)))
         (when turn-node
           (setf (alist-get 'frozen turn-node) t))))

      ("set_turn_stop"
       (let* ((turn-num (alist-get 'turnNumber patch))
              (turn-node (claude-gravity--get-turn-node session turn-num)))
         (when turn-node
           (let ((st (alist-get 'stopText patch))
                 (sth (alist-get 'stopThinking patch)))
             (when st (setf (alist-get 'stop_text turn-node) st))
             (when sth (setf (alist-get 'stop_thinking turn-node) sth))))))

      ("set_turn_tokens"
       (let* ((turn-num (alist-get 'turnNumber patch))
              (turn-node (claude-gravity--get-turn-node session turn-num)))
         (when turn-node
           (setf (alist-get 'token-in turn-node) (alist-get 'tokenIn patch))
           (setf (alist-get 'token-out turn-node) (alist-get 'tokenOut patch)))))

      ("add_step"
       (let* ((turn-num (alist-get 'turnNumber patch))
              (agent-id (alist-get 'agentId patch))
              (step-json (alist-get 'step patch))
              (step-node (claude-gravity--json-step-to-alist step-json)))
         (if agent-id
             ;; Add to agent's steps
             (let ((agent (gethash agent-id (plist-get session :agent-index))))
               (when agent
                 (claude-gravity--tlist-append (alist-get 'steps agent) step-node)))
           ;; Add to turn's steps
           (let ((turn-node (claude-gravity--get-turn-node session turn-num)))
             (when turn-node
               (claude-gravity--tlist-append (alist-get 'steps turn-node) step-node))))))

      ("add_tool"
       (let* ((turn-num (alist-get 'turnNumber patch))
              (step-idx (alist-get 'stepIndex patch))
              (agent-id (alist-get 'agentId patch))
              (tool-json (alist-get 'tool patch))
              (tool (claude-gravity--json-tool-to-alist tool-json))
              (tid (alist-get 'tool_use_id tool)))
         ;; Dedup
         (unless (and tid (gethash tid (plist-get session :tool-index)))
           ;; Index the tool
           (when tid
             (puthash tid tool (plist-get session :tool-index)))
           ;; Route to correct step
           (let ((target-steps
                  (if agent-id
                      (let ((agent (gethash agent-id (plist-get session :agent-index))))
                        (when agent (alist-get 'steps agent)))
                    (let ((turn-node (claude-gravity--get-turn-node session turn-num)))
                      (when turn-node (alist-get 'steps turn-node))))))
             (when target-steps
               (let* ((items (claude-gravity--tlist-items target-steps))
                      (step (if (and step-idx (< step-idx (length items)))
                                (nth step-idx items)
                              ;; Fallback: ensure a step exists
                              (or (claude-gravity--tlist-last-item target-steps)
                                  (let ((s (claude-gravity--make-step-node)))
                                    (claude-gravity--tlist-append target-steps s)
                                    s)))))
                 (claude-gravity--tlist-append (alist-get 'tools step) tool))))
           ;; Update counters
           (plist-put session :total-tool-count
                      (1+ (or (plist-get session :total-tool-count) 0)))
           (unless agent-id
             (let ((turn-node (claude-gravity--get-turn-node session turn-num)))
               (when turn-node
                 (cl-incf (alist-get 'tool-count turn-node))))))))

      ("complete_tool"
       (let* ((tid (alist-get 'toolUseId patch))
              (tool (gethash tid (plist-get session :tool-index))))
         (when tool
           (setf (alist-get 'status tool) (alist-get 'status patch))
           (setf (alist-get 'result tool) (alist-get 'result patch))
           (let ((dur (alist-get 'duration patch)))
             (when dur (setf (alist-get 'duration tool) dur)))
           (let ((pt (alist-get 'postText patch)))
             (when pt (setf (alist-get 'post_text tool) pt)))
           (let ((pth (alist-get 'postThinking patch)))
             (when pth (setf (alist-get 'post_thinking tool) pth))))))

      ("add_agent"
       (let* ((agent-json (alist-get 'agent patch))
              (agent (claude-gravity--json-agent-to-alist agent-json))
              (aid (alist-get 'agent_id agent))
              (turn-node (claude-gravity--current-turn-node session)))
         (unless (gethash aid (plist-get session :agent-index))
           ;; Index
           (puthash aid agent (plist-get session :agent-index))
           ;; Add to current turn
           (when turn-node
             (claude-gravity--tlist-append (alist-get 'agents turn-node) agent)
             (cl-incf (alist-get 'agent-count turn-node))
             ;; Link to Task tool
             (claude-gravity--link-agent-to-task-tool session turn-node agent)))))

      ("complete_agent"
       (let* ((aid (alist-get 'agentId patch))
              (agent (gethash aid (plist-get session :agent-index))))
         (when agent
           (setf (alist-get 'status agent) "done")
           (let ((st (alist-get 'stopText patch)))
             (when st (setf (alist-get 'stop_text agent) st)))
           (let ((sth (alist-get 'stopThinking patch)))
             (when sth (setf (alist-get 'stop_thinking agent) sth)))
           (let ((dur (alist-get 'duration patch)))
             (when dur (setf (alist-get 'duration agent) dur))))))

      ("update_task"
       (let* ((task-id (alist-get 'taskId patch))
              (task-json (alist-get 'task patch))
              (task (claude-gravity--json-task-to-alist task-json)))
         (puthash task-id task (plist-get session :tasks))))

      ("track_file"
       (let* ((path (alist-get 'path patch))
              (file-op (alist-get 'fileOp patch))
              (files (plist-get session :files))
              (entry (gethash path files)))
         (if entry
             (progn
               (unless (member file-op (alist-get 'ops entry))
                 (setf (alist-get 'ops entry)
                       (cons file-op (alist-get 'ops entry))))
               (setf (alist-get 'last-touched entry) (current-time)))
           (puthash path
                    (list (cons 'ops (list file-op))
                          (cons 'last-touched (current-time)))
                    files))))

      ("add_prompt"
       (let* ((turn-num (alist-get 'turnNumber patch))
              (prompt-json (alist-get 'prompt patch))
              (turn-node (claude-gravity--get-turn-node session turn-num)))
         (when turn-node
           (setf (alist-get 'prompt turn-node)
                 (claude-gravity--json-prompt-to-alist prompt-json)))))

      ("set_prompt_answer"
       (let* ((turn-num (alist-get 'turnNumber patch))
              (tid (alist-get 'toolUseId patch))
              (answer (alist-get 'answer patch))
              (turn-node (claude-gravity--get-turn-node session turn-num)))
         (when turn-node
           (let ((p (alist-get 'prompt turn-node)))
             (when (and p (equal (alist-get 'tool_use_id p) tid))
               (setf (alist-get 'answer p) answer)
               (setf (alist-get 'elapsed p)
                     (float-time
                      (time-subtract (current-time)
                                     (alist-get 'submitted p)))))))))
      (_
       ;; Unknown patch op — request full snapshot to recover
       (claude-gravity--log 'warn "Unknown patch op: %s — requesting snapshot" op)
       (let ((sid (plist-get session :session-id)))
         (when sid
           (remhash sid claude-gravity--client-subscribed-sessions)
           (claude-gravity--request-session sid)))))))


;;; ── Overview handler ────────────────────────────────────────────────

(defun claude-gravity--handle-overview-snapshot (msg)
  "Handle overview.snapshot — update session list from server.
MSG contains projects array with session summaries."
  (let ((projects (alist-get 'projects msg)))
    ;; Auto-subscribe to all sessions we don't know about yet
    (dolist (proj projects)
      (let ((sessions (alist-get 'sessions proj)))
        (dolist (s sessions)
          (let ((sid (alist-get 'sessionId s)))
            (when (and sid (not (gethash sid claude-gravity--sessions)))
              (claude-gravity--request-session sid))))))
    ;; Render overview
    (claude-gravity--schedule-refresh)))


;;; ── Session removed ─────────────────────────────────────────────────

(defun claude-gravity--handle-session-removed (msg)
  "Handle session.removed — remove session from local state."
  (let* ((session-id (alist-get 'sessionId msg))
         (session (gethash session-id claude-gravity--sessions)))
    (when session
      (let ((buf (plist-get session :buffer)))
        (when (and buf (buffer-live-p buf))
          (kill-buffer buf))))
    (remhash session-id claude-gravity--sessions)
    (remhash session-id claude-gravity--client-subscribed-sessions)
    (claude-gravity--schedule-refresh)))


;;; ── Inbox handlers ──────────────────────────────────────────────────

(defun claude-gravity--handle-inbox-added (msg)
  "Handle inbox.added — add item to local inbox and trigger UI."
  (let* ((item-json (alist-get 'item msg))
         (item (claude-gravity--json-inbox-item-to-alist item-json)))
    ;; Add to local inbox
    (push item claude-gravity--inbox)
    (claude-gravity--inbox-notify item)
    (claude-gravity--schedule-refresh)
    ;; Auto-act based on type
    (let ((type (alist-get 'type item))
          (session-id (alist-get 'session-id item)))
      ;; Check turn auto-approve for permissions
      (when (eq type 'permission)
        (let ((auto (assoc session-id claude-gravity--turn-auto-approve)))
          (when (and auto
                     (let ((session (claude-gravity--get-session session-id)))
                       (and session
                            (eql (cdr auto) (plist-get session :current-turn)))))
            (claude-gravity--send-permission-response
             (alist-get 'id item) "allow")
            (claude-gravity--inbox-remove (alist-get 'id item)))))
      ;; Schedule session refresh
      (claude-gravity--schedule-session-refresh session-id))))

(defun claude-gravity--handle-inbox-removed (msg)
  "Handle inbox.removed — remove item from local inbox."
  (let ((item-id (alist-get 'itemId msg)))
    (claude-gravity--inbox-remove item-id)))

(defun claude-gravity--json-inbox-item-to-alist (item-json)
  "Convert a JSON InboxItem to the alist format used by Emacs."
  (list (cons 'id (alist-get 'id item-json))
        (cons 'type (intern (or (alist-get 'type item-json) "permission")))
        (cons 'session-id (alist-get 'sessionId item-json))
        (cons 'project (alist-get 'project item-json))
        (cons 'label (alist-get 'label item-json))
        (cons 'timestamp (claude-gravity--epoch-to-time
                          (alist-get 'timestamp item-json)))
        (cons 'summary (alist-get 'summary item-json))
        (cons 'data (alist-get 'data item-json))
        ;; No socket-proc in client mode — server handles bidirectional
        (cons 'socket-proc nil)))


;;; ── Inbox notification (mode-line) ──────────────────────────────────

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
    (claude-gravity--log 'debug "Claude: %s from %s" type-label project)))

(defun claude-gravity--clear-notification-indicator ()
  "Clear the mode-line notification indicator."
  (let ((attention (cl-count-if
                    (lambda (item) (not (eq (alist-get 'type item) 'idle)))
                    claude-gravity--inbox)))
    (if (> attention 0)
        (claude-gravity--update-inbox-indicator)
      (when (not (string-empty-p claude-gravity--notification-indicator))
        (setq claude-gravity--notification-indicator "")
        (force-mode-line-update t)))))

(provide 'claude-gravity-client)
;;; claude-gravity-client.el ends here
