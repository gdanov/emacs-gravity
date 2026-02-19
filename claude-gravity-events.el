;;; claude-gravity-events.el --- Event handling for Claude Gravity  -*- lexical-binding: t; -*-

;;; Code:

(require 'claude-gravity-core)
(require 'claude-gravity-session)
(require 'claude-gravity-state)

; Forward declarations for functions/vars in modules loaded later
(defvar claude-gravity--tmux-sessions)
(defvar claude-gravity--tmux-pending)
(defvar claude-gravity--daemon-pending)
(defvar claude-gravity--daemon-sessions)
(defvar claude-gravity--resume-picker-buffer)
(defvar claude-gravity--resume-picker-tmux)
(defvar claude-gravity--resume-picker-init-seen nil
  "Non-nil after the init SessionStart from the resume picker has been seen.")
(declare-function claude-gravity--bury-resume-picker "claude-gravity-tmux")
(declare-function claude-gravity--handle-tool-permission "claude-gravity-socket")
(declare-function claude-gravity--handle-ask-user-question "claude-gravity-socket")
(declare-function claude-gravity--handle-plan-review "claude-gravity-socket")
(declare-function claude-gravity--clear-notification-indicator "claude-gravity-socket")
(declare-function claude-gravity--tmux-ensure-heartbeat "claude-gravity-tmux")
(declare-function claude-gravity--tmux-alive-p "claude-gravity-tmux")
(declare-function claude-gravity--plan-revision-diff "claude-gravity-diff")
(declare-function claude-gravity--plan-review-apply-margin-indicators "claude-gravity-diff")
(declare-function claude-gravity--daemon-rekey-session "claude-gravity-daemon")
(declare-function claude-gravity--send-prompt-core "claude-gravity-tmux")
(declare-function claude-gravity--soft-rekey-session "claude-gravity-session")


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

(defun claude-gravity--extract-model-from-transcript (session transcript-path)
  "Extract model name from TRANSCRIPT-PATH and set on SESSION.
Reads the first few lines of the JSONL transcript to find the model field."
  (condition-case nil
      (with-temp-buffer
        (insert-file-contents transcript-path nil 0 8192)
        (goto-char (point-min))
        (catch 'found
          (while (not (eobp))
            (let ((line (buffer-substring-no-properties
                         (line-beginning-position) (line-end-position))))
              (when (> (length line) 0)
                (condition-case nil
                    (let* ((obj (json-parse-string line :object-type 'alist))
                           (msg (alist-get 'message obj))
                           (model (when msg (alist-get 'model msg))))
                      (when (and model (stringp model) (not (string-empty-p model)))
                        (plist-put session :model-name
                                   (or (claude-gravity--short-model-name model) model))
                        (throw 'found t)))
                  (error nil))))
            (forward-line 1))))
    (error nil)))

(defun claude-gravity--update-turn-tokens (session token-usage)
  "Update per-turn token delta on SESSION's current turn from TOKEN-USAGE.
Used during active turns (PostToolUse) and at turn end (Stop).
On Stop, also updates :prev-token-usage so the next turn starts fresh."
  (when token-usage
    (let* ((prev-usage (plist-get session :prev-token-usage))
           (new-in (+ (or (alist-get 'input_tokens token-usage) 0)
                      (or (alist-get 'cache_read_input_tokens token-usage) 0)
                      (or (alist-get 'cache_creation_input_tokens token-usage) 0)))
           (new-out (or (alist-get 'output_tokens token-usage) 0))
           (delta-in (max 0 (- new-in (or (alist-get 'total-in prev-usage) 0))))
           (delta-out (max 0 (- new-out (or (alist-get 'total-out prev-usage) 0)))))
      (claude-gravity-model-set-turn-tokens session delta-in delta-out))))

(defun claude-gravity--finalize-turn-tokens (session token-usage)
  "Finalize per-turn tokens and advance :prev-token-usage baseline.
Called at Stop to lock in delta and set baseline for next turn."
  (when token-usage
    (claude-gravity--update-turn-tokens session token-usage)
    (let* ((new-in (+ (or (alist-get 'input_tokens token-usage) 0)
                      (or (alist-get 'cache_read_input_tokens token-usage) 0)
                      (or (alist-get 'cache_creation_input_tokens token-usage) 0)))
           (new-out (or (alist-get 'output_tokens token-usage) 0)))
      (if (plist-member session :prev-token-usage)
          (plist-put session :prev-token-usage
                     `((total-in . ,new-in) (total-out . ,new-out)))
        (nconc session (list :prev-token-usage
                             `((total-in . ,new-in) (total-out . ,new-out))))))))

(defun claude-gravity-handle-event (event session-id cwd data &optional pid source)
  "Handle EVENT for SESSION-ID (with CWD) carrying DATA.
Optional PID is the Claude Code process ID.
Optional SOURCE is the event source: \"claude-code\" or \"opencode\".
This is the hooks adapter — it parses hook event payloads and calls
the model mutation API to update session state."
  (unless session-id
    (setq session-id "legacy")
    (setq cwd (or cwd "")))
  (claude-gravity--log 'debug "Handling event: %s for session: %s" event session-id)
  ;; Auto-register tmux association from bridge-reported tmux_session
  (let ((tmux-name (alist-get 'tmux_session data)))
    (when (and tmux-name session-id
               (not (gethash session-id claude-gravity--tmux-sessions)))
      (puthash session-id tmux-name claude-gravity--tmux-sessions)
      (claude-gravity--log 'info "Auto-registered tmux mapping: %s → %s" session-id tmux-name)))
  ;; Update PID, last-event-time, and slug on every event
  (let ((existing (claude-gravity--get-session session-id)))
    (when existing
      ;; Self-heal: if session is ended but we're receiving events, it's alive
      (when (and (eq (plist-get existing :status) 'ended)
                 (not (equal event "SessionEnd")))
        (plist-put existing :status 'active)
        (claude-gravity--log 'info "Session %s self-healed to active on %s" session-id event))
      (claude-gravity-model-update-session-meta
       existing :pid pid :slug (alist-get 'slug data)
       :branch (alist-get 'branch data))
      ;; Set source for OpenCode sessions
      (when (and source (equal source "opencode"))
        (claude-gravity--log 'debug "Setting source for session %s" session-id)
        (claude-gravity--session-set-source existing "opencode"
          (alist-get 'instance_port data)
          (alist-get 'instance_dir data)))))
  ;; Auto-dismiss stale bidirectional inbox items.
  ;; If a turn boundary event fires while bidirectional items are pending,
  ;; the user handled them outside Emacs (e.g., in Claude's tmux terminal).
  ;; Only dismiss on turn/session boundaries — mid-turn events like
  ;; PostToolUse or SubagentStop don't mean other tools' permissions are stale.
  (when (member event '("UserPromptSubmit" "Stop" "SessionEnd"))
    (claude-gravity--dismiss-stale-inbox-items session-id)
    (setq claude-gravity--turn-auto-approve
          (assoc-delete-all session-id claude-gravity--turn-auto-approve)))
  (pcase event
    ("SessionStart"
     (let ((existing (claude-gravity--get-session session-id)))
       (when existing
         (claude-gravity--reset-session existing)))
     ;; Re-key tmux pending session: match temp-id from bridge payload
     (let* ((temp-id (alist-get 'temp_id data))
            (tmux-name (and temp-id (gethash temp-id claude-gravity--tmux-pending))))
       (when tmux-name
         (remhash temp-id claude-gravity--tmux-pending)
         ;; Resume picker: the init SessionStart (source=startup) consumes
         ;; the pending entry, but the real SessionStart (source=resume)
         ;; needs it too.  Re-register so the second re-key works.
         (when (and claude-gravity--resume-picker-buffer
                    (equal (alist-get 'source data) "startup"))
           (puthash temp-id tmux-name claude-gravity--tmux-pending))
         (let ((temp-session (gethash temp-id claude-gravity--sessions)))
           (if temp-session
               (progn
                 ;; Re-key session from temp-id to real session-id
                 (remhash temp-id claude-gravity--sessions)
                 (plist-put temp-session :session-id session-id)
                 (plist-put temp-session :temp-id temp-id)
                 (puthash session-id temp-session claude-gravity--sessions)
                 ;; Register tmux mapping under real session-id
                 (puthash session-id tmux-name claude-gravity--tmux-sessions)
                 ;; Rename buffer
                 (let ((old-buf (plist-get temp-session :buffer)))
                   (when (and old-buf (buffer-live-p old-buf))
                     (let ((new-name (claude-gravity--session-buffer-name temp-session)))
                       (with-current-buffer old-buf
                         (rename-buffer new-name t)
                         (setq claude-gravity--buffer-session-id session-id)))))
                 ;; Clear-and-proceed: preserve turns/tools/agents, just re-key identity.
                 ;; Normal /clear: full state reset.
                 (if (plist-get temp-session :awaiting-clear)
                     (let ((plan-path (plist-get temp-session :clear-plan-path)))
                       (claude-gravity--soft-rekey-session temp-session session-id)
                       ;; Auto-send implementation prompt via tmux
                       (when (and tmux-name plan-path)
                         (run-at-time 2 nil
                           (lambda ()
                             (when (claude-gravity--tmux-alive-p tmux-name)
                               (claude-gravity--send-prompt-core
                                (format "Implement the plan in @%s" plan-path)
                                session-id tmux-name))))))
                   (claude-gravity--reset-session temp-session))
                 ;; Apply slug now — the pre-pcase update missed it
                 ;; because the session was still keyed under temp-id.
                 (claude-gravity-model-update-session-meta
                  temp-session :pid pid :slug (alist-get 'slug data)))
             ;; resume-in-tmux fallback: tmux-pending entry found but no
             ;; session stored under temp-id.  Set :temp-id on the real
             ;; session so SessionEnd can re-key it on /clear.
             (let ((real-session (or (claude-gravity--get-session session-id)
                                     (claude-gravity--ensure-session session-id cwd))))
               (plist-put real-session :temp-id temp-id)
               (puthash session-id tmux-name claude-gravity--tmux-sessions))))))
     ;; Re-key daemon pending session (same pattern as tmux above)
     (let ((temp-id (alist-get 'temp_id data)))
       (when (and temp-id
                  (boundp 'claude-gravity--daemon-pending)
                  (gethash temp-id claude-gravity--daemon-pending))
         (claude-gravity--daemon-rekey-session temp-id session-id data pid)))
     (unless (equal (alist-get 'source data) "startup")
       (claude-gravity--ensure-session session-id cwd))
     ;; Apply slug/branch/source from data AFTER session exists.
     ;; The pre-pcase meta update at top misses new sessions because
     ;; the session doesn't exist yet when that code runs.
     (let ((session (claude-gravity--get-session session-id)))
       (when session
         (claude-gravity-model-update-session-meta
          session :pid pid :slug (alist-get 'slug data)
          :branch (alist-get 'branch data))
         ;; Capture model from SessionStart payload (available since Claude Code 2.x)
         (let ((model-id (alist-get 'model data)))
           (when (and model-id (stringp model-id) (not (string-empty-p model-id)))
             (plist-put session :model-name
                        (or (claude-gravity--short-model-name model-id) model-id))))
         (when (and source (equal source "opencode"))
           (claude-gravity--session-set-source session "opencode"
             (alist-get 'instance_port data)
             (alist-get 'instance_dir data)))
         ;; Auto-focus the new session buffer (deferred out of process filter).
         ;; Resume picker flow fires two SessionStarts:
         ;;   1. source="startup" — init before picker UI, suppress
         ;;   2. source="resume"  — user picked a session, bury picker + open
         (cond
          ((and claude-gravity--resume-picker-buffer
                (equal (alist-get 'source data) "startup"))
           ;; Init SessionStart — suppress auto-focus, picker is showing
           nil)
          (claude-gravity--resume-picker-buffer
           ;; Real resumed session — bury picker and open gravity buffer
           (run-at-time 0 nil #'claude-gravity--bury-resume-picker)
           (run-at-time 0.1 nil #'claude-gravity-open-session session-id))
          (t
           ;; Normal SessionStart — open session buffer
           (run-at-time 0 nil #'claude-gravity-open-session session-id))))))

    ("SessionEnd"
     (let ((session (claude-gravity--get-session session-id)))
       (when session
         ;; Clear-and-proceed: suppress session-end marking so the session
         ;; stays active.  The /clear re-key path below still fires.
         (unless (plist-get session :awaiting-clear)
           (claude-gravity-model-session-end session))
         ;; If tmux process is still alive, this is a /clear — preserve mapping
         ;; for SessionStart re-keying.  Otherwise clean up.
         (let ((tmux-name (gethash session-id claude-gravity--tmux-sessions)))
           (if (and tmux-name (claude-gravity--tmux-alive-p tmux-name))
               ;; /clear — put temp-id back into pending for next SessionStart
               (let ((temp-id (plist-get session :temp-id)))
                 (when temp-id
                   (puthash temp-id tmux-name claude-gravity--tmux-pending)
                   ;; Move session back under temp-id so SessionStart re-keying finds it
                   (puthash temp-id session claude-gravity--sessions)
                   (remhash session-id claude-gravity--sessions)
                   (remhash session-id claude-gravity--tmux-sessions)))
             (remhash session-id claude-gravity--tmux-sessions)))))
     ;; Remove all inbox items for this session
     (claude-gravity--inbox-remove-for-session session-id)
     (claude-gravity--clear-notification-indicator))

    ("StatusLine"
     (let ((session (claude-gravity--get-session session-id)))
       (when session
         (let-alist data
           (when (numberp .cost\.total_cost_usd)
             (plist-put session :cost .cost\.total_cost_usd))
           (when (numberp .context_window\.used_percentage)
             (plist-put session :context-pct
                        (truncate .context_window\.used_percentage)))
           (when .model\.display_name
             (plist-put session :model-name .model\.display_name))
           (when .model\.id
             (plist-put session :model-id .model\.id))
           (when (numberp .context_window\.total_input_tokens)
             (plist-put session :sl-input-tokens
                        (truncate .context_window\.total_input_tokens)))
           (when (numberp .context_window\.total_output_tokens)
             (plist-put session :sl-output-tokens
                        (truncate .context_window\.total_output_tokens)))
           (when (numberp .cost\.total_duration_ms)
             (plist-put session :sl-duration-ms
                        (truncate .cost\.total_duration_ms)))
           (when (numberp .cost\.total_lines_added)
             (plist-put session :sl-lines-added
                        (truncate .cost\.total_lines_added)))
           (when (numberp .cost\.total_lines_removed)
             (plist-put session :sl-lines-removed
                        (truncate .cost\.total_lines_removed))))
         (claude-gravity--schedule-refresh))))

    ("UserPromptSubmit"
     (let* ((session (claude-gravity--ensure-session session-id cwd))
            (raw-prompt (alist-get 'prompt data))
            (prompt-text (claude-gravity--strip-system-xml raw-prompt))
            ;; Extract slash command name as fallback when XML stripping yields nil
            (cmd-name (when (and (not prompt-text) raw-prompt
                                 (string-match "<command-name>\\([^<]+\\)</command-name>" raw-prompt))
                        (match-string 1 raw-prompt)))
            (cmd-args (when (and cmd-name raw-prompt
                                 (string-match "<command-args>\\([^<]*\\)</command-args>" raw-prompt))
                        (match-string 1 raw-prompt)))
            (display-text (or prompt-text
                              (when cmd-name
                                (if (and cmd-args (not (string-empty-p cmd-args)))
                                    (format "/%s %s" cmd-name cmd-args)
                                  (format "/%s" cmd-name))))))
       ;; Dedup: if tmux/daemon-prompt-sent flag is set, we already created
       ;; the prompt entry in send-prompt — skip to avoid duplicate
       (if (or (plist-get session :tmux-prompt-sent)
               (plist-get session :daemon-prompt-sent))
           (progn
             (plist-put session :tmux-prompt-sent nil)
             (plist-put session :daemon-prompt-sent nil))
         ;; OC sends UserPromptSubmit without 'prompt — use placeholder
         (let ((text (or display-text
                         (when (equal source "opencode") "(user prompt)"))))
           (when text
             (claude-gravity-model-add-prompt
              session (list (cons 'text text)
                            (cons 'submitted (current-time))
                            (cons 'elapsed nil)
                            (cons 'stop_text nil)
                            (cons 'stop_thinking nil))))))
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
         ;; Finalize per-turn token delta and advance baseline for next turn
         (claude-gravity--finalize-turn-tokens session (alist-get 'token_usage data))
         ;; Replace any existing idle inbox item for this session
         (claude-gravity--inbox-remove-for-session session-id 'idle)
         (let* ((turn (plist-get session :current-turn))
                (stop-text (alist-get 'stop_text data))
                (snippet (when stop-text
                           (truncate-string-to-width
                            (replace-regexp-in-string "\n" " " stop-text) 80))))
           (claude-gravity--inbox-add 'idle session-id
                                      `((turn . ,turn) (snippet . ,snippet)) nil))
         ;; Clear-and-proceed: Claude finished its turn, now send /clear
         (when (plist-get session :awaiting-clear)
           (let ((tmux-name (gethash session-id claude-gravity--tmux-sessions)))
             (when (and tmux-name (claude-gravity--tmux-alive-p tmux-name))
               (claude-gravity--log 'info "Clear-and-proceed: sending /clear to %s" tmux-name)
               ;; Escape aborts any lingering autocomplete, then /clear
               (run-at-time 0.5 nil
                 (lambda ()
                   (call-process "tmux" nil nil nil
                                 "send-keys" "-t" tmux-name "Escape")
                   (run-at-time 1 nil
                     (lambda ()
                       (call-process "tmux" nil nil nil
                                     "send-keys" "-t" tmux-name "-l" "/clear")
                       (run-at-time 2 nil
                         (lambda ()
                           (call-process "tmux" nil nil nil
                                         "send-keys" "-t" tmux-name "Enter")))))))))))))

    ("SubagentStart"
     (let* ((session (claude-gravity--ensure-session session-id cwd))
            (new-agent (list (cons 'agent_id (alist-get 'agent_id data))
                             (cons 'type (alist-get 'agent_type data))
                             (cons 'status "running")
                             (cons 'timestamp (current-time))
                             (cons 'turn (or (plist-get session :current-turn) 0))
                             (cons 'cycles (claude-gravity--tlist-new))
                             (cons 'tool-count 0)
                             (cons 'task-tool nil)
                             (cons 'transcript_path (alist-get 'agent_transcript_path data))
                             (cons 'stop_text nil)
                             (cons 'stop_thinking nil)
                             (cons 'duration nil))))
       (claude-gravity-model-add-agent session new-agent)))

    ("SubagentStop"
     (let* ((session (claude-gravity--ensure-session session-id cwd))
            (agent-id (alist-get 'agent_id data))
            (stop-text (alist-get 'agent_stop_text data))
            (stop-thinking (alist-get 'agent_stop_thinking data)))
       (claude-gravity-model-complete-agent
        session agent-id
        :transcript-path (alist-get 'agent_transcript_path data)
        :stop-text stop-text
        :stop-thinking stop-thinking)))

    ("PreToolUse"
     (let* ((session (claude-gravity--ensure-session session-id cwd))
            (parent-agent-id (alist-get 'parent_agent_id data))
            (new-tool (list (cons 'tool_use_id (alist-get 'tool_use_id data))
                            (cons 'name (alist-get 'tool_name data))
                            (cons 'input (alist-get 'tool_input data))
                            (cons 'status "running")
                            (cons 'result nil)
                            (cons 'timestamp (current-time))
                            (cons 'turn (or (plist-get session :current-turn) 0))
                            (cons 'permission_mode (alist-get 'permission_mode data))
                            (cons 'assistant_text (alist-get 'assistant_text data))
                            (cons 'assistant_thinking (alist-get 'assistant_thinking data))
                            (cons 'parent_agent_id parent-agent-id)
                            (cons 'model (alist-get 'model data))
                            (cons 'requested_model (alist-get 'requested_model data))
                            (cons 'post_text nil)
                            (cons 'post_thinking nil))))
       (claude-gravity-model-add-tool
        session new-tool parent-agent-id
        (alist-get 'candidate_agent_ids data))
       (claude-gravity-model-set-permission-mode
        session (alist-get 'permission_mode data))
       (claude-gravity-model-set-claude-status session 'responding)
       ;; Set model name from bridge enrichment or transcript fallback
       (unless parent-agent-id
         (let ((model-id (alist-get 'model data)))
           (when (and model-id (stringp model-id) (not (string-empty-p model-id)))
             (plist-put session :model-name
                        (or (claude-gravity--short-model-name model-id)
                            model-id)))
           ;; Fallback: extract model from transcript if bridge didn't provide it
           (unless (plist-get session :model-name)
             (let ((tp (alist-get 'transcript_path data)))
               (when (and tp (file-readable-p tp))
                 (claude-gravity--extract-model-from-transcript session tp))))))
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
                            (cons 'answer nil)
                            (cons 'stop_text nil)
                            (cons 'stop_thinking nil))))))))

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
                            :allowed-prompts (append allowed-prompts nil))))
           ;; Clear-and-proceed: plan file path is only available now
           ;; (PostToolUse response), not at PermissionRequest time.
           (when (and file-path (plist-get session :awaiting-clear))
             (plist-put session :clear-plan-path file-path)))
         ;; Advance turn — plan approval is a phase boundary
         (claude-gravity-model-add-prompt
          session (list (cons 'text "[Plan approved]")
                        (cons 'type 'phase-boundary)
                        (cons 'submitted (current-time))
                        (cons 'elapsed nil)
                        (cons 'stop_text nil)
                        (cons 'stop_thinking nil))))
       ;; Update per-turn token delta (running estimate during active turn)
       (claude-gravity--update-turn-tokens session (alist-get 'token_usage data))))

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

    ;; OpenCode-specific events
    ("SessionStatus"
     (let* ((session (claude-gravity--ensure-session session-id cwd))
            (oc-status (alist-get 'status data))
            (title (alist-get 'title data))
            (slug (alist-get 'slug data))
            (branch (alist-get 'branch data)))
       (when session
         ;; Set source — SessionStatus may be first event for this session
         ;; (from polling), so pre-pcase source check missed it
         (when (equal source "opencode")
           (claude-gravity--session-set-source session "opencode"
(alist-get 'instance_port data)
              (alist-get 'instance_dir data)))
          ;; Map OC status to claude-gravity status
          ;; Only upgrade to responding; never downgrade to idle from polling
          ;; (real idle comes from SessionIdle/Stop events)
          (pcase oc-status
            ((or "busy" "running")
             (claude-gravity-model-set-claude-status session 'responding))
            ((or "idle" "completed")
             (unless (eq (plist-get session :claude-status) 'responding)
               (claude-gravity-model-set-claude-status session 'idle)))
            ("error"
             (unless (eq (plist-get session :claude-status) 'responding)
               (claude-gravity-model-set-claude-status session 'idle))))
         (when title
           (plist-put session :title title))
         (claude-gravity-model-update-session-meta
          session :slug slug :branch branch))))

    ("SessionIdle"
     (let ((session (claude-gravity--get-session session-id)))
       (when session
         (claude-gravity-model-set-claude-status session 'idle)
         (claude-gravity-model-finalize-last-prompt session))))

    ("VcsBranchUpdate"
     (let ((session (claude-gravity--get-session session-id)))
       (when session
         (let ((branch (alist-get 'branch data)))
           (when branch
             (plist-put session :branch branch))))))

    ("MessagePart"
     (let ((session (claude-gravity--ensure-session session-id cwd)))
       (when session
         ;; Set source — MessagePart may arrive before SessionStart
         (when (equal source "opencode")
           (claude-gravity--session-set-source session "opencode"
             (alist-get 'instance_port data)
             (alist-get 'instance_dir data)))
         (claude-gravity-model-set-claude-status session 'responding)
         (let ((part-type (alist-get 'part_type data)))
           (pcase part-type
             ("tool"
              (let* ((tool-data (alist-get 'tool data))
                     (call-id (alist-get 'call_id tool-data))
                     (tool-name (alist-get 'tool_name tool-data))
                     (state (alist-get 'state tool-data))
                     (existing (when call-id
                                 (claude-gravity-model-find-tool session call-id))))
                (pcase state
                  ((or "pending" "running")
                   ;; Create tool if not exists
                   (unless existing
                     (let ((new-tool
                            (list (cons 'tool_use_id call-id)
                                  (cons 'name tool-name)
                                  (cons 'input nil)
                                  (cons 'status "running")
                                  (cons 'result nil)
                                  (cons 'timestamp (current-time))
                                  (cons 'turn (or (plist-get session :current-turn) 0))
                                  (cons 'parent_agent_id nil)
                                  (cons 'post_text nil)
                                  (cons 'post_thinking nil))))
                       (claude-gravity-model-add-tool session new-tool nil nil)
                       (claude-gravity--track-file session tool-name nil))))
                  ("completed"
                   (if existing
                       (claude-gravity-model-complete-tool
                        session call-id nil
                        (alist-get 'text data))
                     ;; Arrived completed without a prior running event
                     (let ((new-tool
                            (list (cons 'tool_use_id call-id)
                                  (cons 'name tool-name)
                                  (cons 'input nil)
                                  (cons 'status "done")
                                  (cons 'result (alist-get 'text data))
                                  (cons 'timestamp (current-time))
                                  (cons 'turn (or (plist-get session :current-turn) 0))
                                  (cons 'parent_agent_id nil)
                                  (cons 'post_text nil)
                                  (cons 'post_thinking nil))))
                       (claude-gravity-model-add-tool session new-tool nil nil)
                       (claude-gravity--track-file session tool-name nil))))
                  ("error"
                   (if existing
                       (progn
                         (claude-gravity-model-complete-tool
                          session call-id nil
                          (format "[ERROR] %s" (or (alist-get 'text data) "Unknown")))
                         (let ((tool (claude-gravity-model-find-tool session call-id)))
                           (when tool
                             (setf (alist-get 'status tool) "error"))))
                     ;; Error without prior running
                     (let ((new-tool
                            (list (cons 'tool_use_id call-id)
                                  (cons 'name tool-name)
                                  (cons 'input nil)
                                  (cons 'status "error")
                                  (cons 'result (format "[ERROR] %s" (or (alist-get 'text data) "Unknown")))
                                  (cons 'timestamp (current-time))
                                  (cons 'turn (or (plist-get session :current-turn) 0))
                                  (cons 'parent_agent_id nil)
                                  (cons 'post_text nil)
                                  (cons 'post_thinking nil))))
                       (claude-gravity-model-add-tool session new-tool nil nil)))))))
("text"
               (let ((text (alist-get 'text data))
                     (msg-role (alist-get 'message_role data)))
                 (when (and text (not (string-empty-p text)))
                   (if (equal msg-role "user")
                       (claude-gravity-model-update-prompt-text session text)
                     (claude-gravity-model-append-streaming-text session text)))))
              ("reasoning"
               (let ((text (alist-get 'text data)))
                 (when (and text (not (string-empty-p text)))
                   (claude-gravity-model-append-streaming-text session text)))))))))

    ;; NOTE: No duplicate "UserPromptSubmit" clause here — OC source is set
    ;; via the pre-pcase source check (lines 70-74) and the original handler.
    ;; OC UserPromptSubmit has no 'prompt field; the handler creates a prompt
    ;; with display-text nil, which is fine (shows as empty turn).

    ("AssistantMessage"
     (let ((session (claude-gravity--get-session session-id)))
       (when session
         (claude-gravity-model-set-claude-status session 'responding)
         ;; Extract model name from model_id
         (let ((model-id (alist-get 'model_id data)))
           (when (and model-id (stringp model-id) (not (string-empty-p model-id)))
             (plist-put session :model-id model-id)
             (plist-put session :model-name
                        (or (claude-gravity--short-model-name model-id) model-id))))
         ;; Store cost/token data
         (let ((cost (alist-get 'cost data))
               (tokens (alist-get 'tokens data)))
           (when (numberp cost)
             (plist-put session :cost cost))
           (when tokens
             (claude-gravity-model-set-token-usage session tokens)))
         ;; If finish reason present, the message is complete
         (when (alist-get 'finish data)
           ;; Clear streaming text — it's now part of the completed message
           (claude-gravity-model-clear-streaming-text session)))))

    ("SessionUpdate"
     (let ((session (claude-gravity--get-session session-id)))
       (when session
         (let ((title (alist-get 'title data)))
           (when title
             (plist-put session :title title)))
         (let ((slug (alist-get 'slug data)))
           (when slug
             (plist-put session :slug slug))))))

    ("Notification"
     ;; Ignored — no visible UI effect, avoids wasted re-render.
     (claude-gravity--log 'debug "Notification [%s]: %s"
                          (claude-gravity--session-short-id session-id)
                          (or (alist-get 'message data) "")))

    ("PermissionAutoApproved"
     ;; Bridge auto-approved a safe read-only Bash command.
     ;; Annotate the existing tool entry so the UI can show a badge.
     (let* ((tool-use-id (alist-get 'tool_use_id data))
            (session (claude-gravity--get-session session-id)))
       (when (and session tool-use-id)
         (let ((tool (claude-gravity-model-find-tool session tool-use-id)))
           (when tool
             ;; Destructive append to preserve hash-table reference
             (nconc tool (list (cons 'auto_approved t))))))
       (claude-gravity--schedule-session-refresh session-id)))

    ;; ── Daemon-specific events ──────────────────────────────────────────

    ("StreamDelta"
     (let ((session (claude-gravity--get-session session-id)))
       (when session
         (let ((text (alist-get 'text data))
               (thinking (alist-get 'thinking data)))
           (when text
             (claude-gravity-model-append-streaming-text session text))
           (when thinking
             (claude-gravity-model-append-streaming-text session thinking)))
         (claude-gravity-model-set-claude-status session 'responding))))

    ("AssistantComplete"
     (let ((session (claude-gravity--get-session session-id)))
       (when session
         (claude-gravity-model-clear-streaming-text session))))

    ("DaemonResult"
     (let ((session (claude-gravity--get-session session-id)))
       (when session
         (claude-gravity-model-clear-streaming-text session)
         (claude-gravity-model-set-claude-status session 'idle)
         ;; Store result text as stop_text on the last turn
         (let ((result-text (alist-get 'result data)))
           (claude-gravity-model-finalize-last-prompt
            session result-text nil))
         ;; Store usage/cost
         (let ((usage (alist-get 'usage data)))
           (when usage
             (claude-gravity-model-set-token-usage session usage)))
         (let ((cost (alist-get 'total_cost_usd data)))
           (when (numberp cost)
             (plist-put session :cost cost))))))

    ("ToolProgress"
     ;; Lightweight — log but don't trigger full render
     (claude-gravity--log 'debug "ToolProgress: %s %s %.1fs"
                          (alist-get 'tool_name data)
                          (alist-get 'tool_use_id data)
                          (or (alist-get 'elapsed_time_seconds data) 0)))

    ("DaemonError"
     (let ((session (claude-gravity--get-session session-id)))
       (when session
         (claude-gravity-model-set-claude-status session 'idle)))
     (claude-gravity--log 'error "Daemon error [%s]: %s"
                          session-id (alist-get 'error data))))

  (claude-gravity--schedule-refresh)
  (when session-id
    (claude-gravity--schedule-session-refresh session-id)))

(provide 'claude-gravity-events)
;;; claude-gravity-events.el ends here