;;; claude-gravity-debug.el --- Terminal protocol debug viewer for Claude Gravity  -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'claude-gravity-core)
(require 'claude-gravity-faces)

;; Forward declarations
(defvar claude-gravity--sessions)
(declare-function claude-gravity--session-label "claude-gravity-session")
(declare-function claude-gravity--tool-signature "claude-gravity-diff")

;;; Data model

(defvar claude-gravity--debug-messages nil
  "Ring buffer of captured terminal protocol messages (newest-first).
Each entry is an alist with keys: msg-id, timestamp, direction,
msg-type, session-id, session-label, patches, patch-count, raw, parsed.")

(defvar claude-gravity--debug-msg-counter 0
  "Monotonically increasing counter for debug message IDs.")

(defvar claude-gravity--debug-messages-max 200
  "Maximum messages to retain (oldest dropped).")

(defvar claude-gravity--debug-messages-enabled nil
  "When non-nil, capture terminal protocol messages for debugging.")

;;; Session label resolution

(defun claude-gravity--debug-resolve-label (session-id)
  "Resolve SESSION-ID to a display label using the sessions hash table."
  (when session-id
    (let ((session (gethash session-id claude-gravity--sessions)))
      (when session (claude-gravity--session-label session)))))

;;; Entry building

(defun claude-gravity--debug-build-entry (direction raw-json parsed)
  "Build a debug entry alist from DIRECTION, RAW-JSON, and PARSED message."
  (let* ((msg-type (alist-get 'type parsed))
         (session-id (or (alist-get 'sessionId parsed)
                         (when-let ((item (alist-get 'item parsed)))
                           (alist-get 'sessionId item))))
         (patches (when (equal msg-type "session.update")
                    (alist-get 'patches parsed)))
         (msg-id (cl-incf claude-gravity--debug-msg-counter)))
    `((msg-id . ,msg-id)
      (timestamp . ,(float-time))
      (direction . ,direction)
      (msg-type . ,msg-type)
      (session-id . ,session-id)
      (session-label . ,(claude-gravity--debug-resolve-label session-id))
      (patches . ,patches)
      (patch-count . ,(if patches (length patches) 0))
      (raw . ,raw-json)
      (parsed . ,parsed))))

;;; Capture functions (called from client.el)

(defun claude-gravity--debug-capture-incoming (raw-json parsed)
  "Capture an incoming server message. RAW-JSON is the line, PARSED is the alist."
  (when claude-gravity--debug-messages-enabled
    (let ((entry (claude-gravity--debug-build-entry 'incoming raw-json parsed)))
      (push entry claude-gravity--debug-messages)
      (when (> (length claude-gravity--debug-messages) claude-gravity--debug-messages-max)
        (setcdr (nthcdr (1- claude-gravity--debug-messages-max)
                        claude-gravity--debug-messages) nil))
      (claude-gravity--debug-schedule-refresh))))

(defun claude-gravity--debug-capture-outgoing (raw-json parsed)
  "Capture an outgoing client message. RAW-JSON is the line, PARSED is the alist."
  (when claude-gravity--debug-messages-enabled
    (let ((entry (claude-gravity--debug-build-entry 'outgoing raw-json parsed)))
      (push entry claude-gravity--debug-messages)
      (when (> (length claude-gravity--debug-messages) claude-gravity--debug-messages-max)
        (setcdr (nthcdr (1- claude-gravity--debug-messages-max)
                        claude-gravity--debug-messages) nil))
      (claude-gravity--debug-schedule-refresh))))

;;; Refresh scheduling

(defvar claude-gravity--debug-refresh-timer nil
  "Timer for debounced debug buffer refresh.")

(defun claude-gravity--debug-schedule-refresh ()
  "Schedule debug buffer refresh if visible."
  (when claude-gravity--debug-messages-enabled
    (when (get-buffer-window "*Claude Debug: Protocol*")
      (when claude-gravity--debug-refresh-timer
        (cancel-timer claude-gravity--debug-refresh-timer))
      (setq claude-gravity--debug-refresh-timer
            (run-with-idle-timer 0.1 nil #'claude-gravity--debug-do-refresh)))))

(defun claude-gravity--debug-do-refresh ()
  "Perform actual debug buffer refresh."
  (setq claude-gravity--debug-refresh-timer nil)
  (when-let ((buf (get-buffer "*Claude Debug: Protocol*")))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (claude-gravity--debug-render)))))

;;; Mode

(defvar claude-gravity-debug-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "g") #'claude-gravity-debug-refresh)
    (define-key map (kbd "c") #'claude-gravity-debug-copy-raw)
    (define-key map (kbd "C") #'claude-gravity-debug-copy-parsed)
    (define-key map (kbd "RET") #'claude-gravity-debug-toggle-expand)
    (define-key map (kbd "f") #'claude-gravity-debug-filter-type)
    (define-key map (kbd "s") #'claude-gravity-debug-filter-session)
    (define-key map (kbd "d") #'claude-gravity-debug-filter-direction)
    (define-key map (kbd "p") #'claude-gravity-debug-filter-patch-op)
    (define-key map (kbd "/") #'claude-gravity-debug-search)
    (define-key map (kbd "x") #'claude-gravity-debug-clear)
    map)
  "Keymap for `claude-gravity-debug-mode'.")

(define-derived-mode claude-gravity-debug-mode special-mode "Claude-Debug"
  "Major mode for Claude Gravity terminal protocol debug viewer.
\\{claude-gravity-debug-mode-map}"
  (setq truncate-lines t)
  (setq buffer-read-only t))

(defvar-local claude-gravity--debug-filter-type nil
  "If non-nil, only show messages matching this message type.")

(defvar-local claude-gravity--debug-filter-session nil
  "If non-nil, only show messages from this session ID.")

(defvar-local claude-gravity--debug-filter-direction nil
  "If non-nil, only show messages with this direction (incoming/outgoing).")

(defvar-local claude-gravity--debug-filter-patch-op nil
  "If non-nil, only show session.update messages containing this patch op.")

(defvar-local claude-gravity--debug-expanded (make-hash-table :test 'equal)
  "Hash table of expanded items.
Integer keys = message expansion (level 1).
String keys \"msg-id:patch-idx\" = patch expansion (level 2).")

;;; Entry point

(defun claude-gravity-debug-show ()
  "Show the terminal protocol debug buffer."
  (interactive)
  (unless claude-gravity--debug-messages-enabled
    (setq claude-gravity--debug-messages-enabled t)
    (claude-gravity--log 'info "Debug message capture ENABLED"))
  (let ((buf (get-buffer-create "*Claude Debug: Protocol*")))
    (with-current-buffer buf
      (unless (eq major-mode 'claude-gravity-debug-mode)
        (claude-gravity-debug-mode)
        (setq claude-gravity--debug-expanded (make-hash-table :test 'equal)))
      (claude-gravity--debug-render))
    (display-buffer-in-side-window buf '((side . right) (window-width . 90)))
    (select-window (get-buffer-window buf))))

;;; Filtering

(defun claude-gravity--debug-passes-filter (msg)
  "Return non-nil if MSG passes all active filters."
  (and (or (null claude-gravity--debug-filter-type)
           (equal (alist-get 'msg-type msg) claude-gravity--debug-filter-type))
       (or (null claude-gravity--debug-filter-session)
           (equal (alist-get 'session-id msg) claude-gravity--debug-filter-session))
       (or (null claude-gravity--debug-filter-direction)
           (eq (alist-get 'direction msg) claude-gravity--debug-filter-direction))
       (or (null claude-gravity--debug-filter-patch-op)
           (and (equal (alist-get 'msg-type msg) "session.update")
                (cl-some (lambda (p) (equal (alist-get 'op p)
                                            claude-gravity--debug-filter-patch-op))
                         (alist-get 'patches msg))))))

;;; Summary generators

(defun claude-gravity--debug-message-summary (msg)
  "Generate compact summary string for MSG."
  (let ((msg-type (alist-get 'msg-type msg))
        (direction (alist-get 'direction msg))
        (parsed (alist-get 'parsed msg)))
    (condition-case nil
        (if (eq direction 'outgoing)
            (claude-gravity--debug-outgoing-summary msg-type parsed)
          (claude-gravity--debug-incoming-summary msg-type parsed))
      (error ""))))

(defun claude-gravity--debug-incoming-summary (msg-type parsed)
  "Generate summary for incoming MSG-TYPE with PARSED data."
  (pcase msg-type
    ("session.snapshot"
     (let* ((session (alist-get 'session parsed))
            (turns (alist-get 'turns session))
            (tool-count (or (alist-get 'totalToolCount session) 0)))
       (format "snapshot [%d turns, %d tools]"
               (if (listp turns) (length turns) 0)
               tool-count)))
    ("session.update"
     (claude-gravity--debug-patches-summary (alist-get 'patches parsed)))
    ("session.removed" "removed")
    ("inbox.added"
     (let* ((item (alist-get 'item parsed))
            (inbox-type (alist-get 'type item))
            (data (alist-get 'data item)))
       (pcase inbox-type
         ("permission"
          (let ((tool-name (alist-get 'toolName data)))
            (format "permission: %s" (or tool-name "?"))))
         ("question"
          (let ((question (alist-get 'question data)))
            (format "question: %s" (claude-gravity--truncate (or question "") 30))))
         ("plan-review" "plan-review")
         (_ (or inbox-type "?")))))
    ("inbox.removed"
     (format "itemId=%s" (or (alist-get 'itemId parsed) "?")))
    ("inbox.snapshot"
     (let ((items (alist-get 'items parsed)))
       (format "%d items" (if (listp items) (length items) 0))))
    ("overview.snapshot"
     (let ((projects (alist-get 'projects parsed)))
       (format "%d projects" (if (listp projects) (length projects) 0))))
    (_ "")))

(defun claude-gravity--debug-outgoing-summary (msg-type parsed)
  "Generate summary for outgoing MSG-TYPE with PARSED data."
  (pcase msg-type
    ("action.permission"
     (let ((decision (alist-get 'decision parsed))
           (message-text (alist-get 'message parsed)))
       (if message-text
           (format "%s (%s)" decision (claude-gravity--truncate message-text 20))
         (or decision "?"))))
    ("action.question"
     (let ((answers (alist-get 'answers parsed)))
       (format "%d answers" (if (listp answers) (length answers) 0))))
    ("action.plan-review"
     (let ((decision (alist-get 'decision parsed))
           (feedback (alist-get 'feedback parsed)))
       (if feedback
           (format "%s (with feedback)" decision)
         (or decision "?"))))
    ("action.turn-auto-approve" "YOLO")
    ("request.session" "subscribe")
    ("request.overview" "refresh")
    ("request.resync" "resync")
    (_ "")))

(defun claude-gravity--debug-patches-summary (patches)
  "Generate compact summary of PATCHES list for session.update."
  (if (or (null patches) (= (length patches) 0))
      "[empty]"
    (let* ((max-show 3)
           (total (length patches))
           (shown (seq-take patches max-show))
           (summaries nil))
      (dolist (p shown)
        (push (claude-gravity--debug-patch-short-summary p) summaries))
      (setq summaries (nreverse summaries))
      (let ((result (format "[%s" (string-join summaries ", "))))
        (if (> total max-show)
            (concat result (format ", +%d more]" (- total max-show)))
          (concat result "]"))))))

(defun claude-gravity--debug-patch-short-summary (patch)
  "Generate a short one-word summary for PATCH (for collapsed view)."
  (let ((op (alist-get 'op patch)))
    (pcase op
      ("add_tool"
       (let ((tool (alist-get 'tool patch)))
         (format "add_tool %s" (or (alist-get 'name tool) "?"))))
      ("complete_tool"
       "complete_tool")
      ("set_claude_status"
       (format "set_claude_status"))
      ("set_status"
       "set_status")
      ("add_turn"
       "add_turn")
      ("add_step"
       "add_step")
      ("add_agent"
       (let ((agent (alist-get 'agent patch)))
         (format "add_agent %s" (or (alist-get 'type agent) "?"))))
      ("complete_agent"
       "complete_agent")
      (_ (or op "?")))))

;;; Patch detail summaries (for expanded view)

(defun claude-gravity--debug-patch-detail-summary (patch)
  "Generate detail summary for a single PATCH (for expanded patches list)."
  (let ((op (alist-get 'op patch)))
    (condition-case nil
        (pcase op
          ("set_status"
           (format "\u2192 %s" (alist-get 'status patch)))
          ("set_claude_status"
           (format "\u2192 %s" (alist-get 'claudeStatus patch)))
          ("set_meta"
           (let ((parts nil))
             (when-let ((slug (alist-get 'slug patch)))
               (push (format "slug=%s" slug) parts))
             (when-let ((branch (alist-get 'branch patch)))
               (push (format "branch=%s" branch) parts))
             (when-let ((pid (alist-get 'pid patch)))
               (push (format "pid=%s" pid) parts))
             (string-join (nreverse parts) " ")))
          ("set_token_usage"
           (let ((usage (alist-get 'usage patch)))
             (format "in=%s out=%s"
                     (or (alist-get 'inputTokens usage) "?")
                     (or (alist-get 'outputTokens usage) "?"))))
          ("set_plan"
           (if (alist-get 'plan patch) "plan set" "plan cleared"))
          ("set_streaming_text"
           (let ((text (alist-get 'text patch)))
             (if text
                 (format "%s\u2026" (claude-gravity--truncate text 30))
               "cleared")))
          ("add_turn"
           (let ((turn (alist-get 'turn patch)))
             (format "turn=%s" (or (alist-get 'turnNumber turn) "?"))))
          ("freeze_turn"
           (format "turn=%s" (alist-get 'turnNumber patch)))
          ("set_turn_stop"
           (format "turn=%s" (alist-get 'turnNumber patch)))
          ("set_turn_tokens"
           (format "turn=%s in=%s out=%s"
                   (alist-get 'turnNumber patch)
                   (or (alist-get 'tokenIn patch) "?")
                   (or (alist-get 'tokenOut patch) "?")))
          ("add_step"
           (let ((agent-id (alist-get 'agentId patch)))
             (if agent-id
                 (format "turn=%s agent=%s"
                         (alist-get 'turnNumber patch)
                         (substring agent-id 0 (min 8 (length agent-id))))
               (format "turn=%s step=%s"
                       (alist-get 'turnNumber patch)
                       (or (alist-get 'stepIndex patch) "?")))))
          ("add_tool"
           (let* ((tool (alist-get 'tool patch))
                  (name (alist-get 'name tool))
                  (input (alist-get 'input tool))
                  (sig (condition-case nil
                           (claude-gravity--tool-signature name input)
                         (error name))))
             (format "turn=%s step=%s  %s"
                     (alist-get 'turnNumber patch)
                     (or (alist-get 'stepIndex patch) "?")
                     sig)))
          ("complete_tool"
           (let* ((tool-id (or (alist-get 'toolUseId patch) "?"))
                  (status (or (alist-get 'status patch) "?"))
                  (duration (alist-get 'duration patch))
                  (short-id (substring tool-id 0 (min 12 (length tool-id)))))
             (if duration
                 (format "%s\u2026 \u2192 %s  %.1fs" short-id status duration)
               (format "%s\u2026 \u2192 %s" short-id status))))
          ("add_agent"
           (let ((agent (alist-get 'agent patch)))
             (format "%s (%s)"
                     (or (alist-get 'type agent) "?")
                     (let ((aid (or (alist-get 'agentId agent) "?")))
                       (substring aid 0 (min 8 (length aid)))))))
          ("complete_agent"
           (let* ((agent-id (or (alist-get 'agentId patch) "?"))
                  (duration (alist-get 'duration patch))
                  (short-id (substring agent-id 0 (min 8 (length agent-id)))))
             (if duration
                 (format "%s \u2192 done  %.1fs" short-id duration)
               (format "%s \u2192 done" short-id))))
          ("update_task"
           (let ((task (alist-get 'task patch)))
             (format "%s  %s"
                     (or (alist-get 'taskId patch) "?")
                     (or (alist-get 'status task) "?"))))
          ("track_file"
           (format "%s  %s"
                   (or (alist-get 'fileOp patch) "?")
                   (claude-gravity--truncate (or (alist-get 'path patch) "?") 40)))
          ("add_prompt"
           (let ((prompt (alist-get 'prompt patch)))
             (format "turn=%s  %s"
                     (alist-get 'turnNumber patch)
                     (claude-gravity--truncate (or (alist-get 'text prompt) "") 30))))
          ("set_prompt_answer"
           (format "turn=%s  %s"
                   (alist-get 'turnNumber patch)
                   (let ((tid (or (alist-get 'toolUseId patch) "?")))
                     (substring tid 0 (min 12 (length tid))))))
          (_ (format "%s" op)))
      (error (or op "?")))))

;;; Utilities

(defun claude-gravity--debug-format-size (bytes)
  "Format BYTES as a human-readable size string (e.g. 1.2K, 3.4M)."
  (cond
   ((< bytes 1024) (format "%dB" bytes))
   ((< bytes (* 1024 1024)) (format "%.1fK" (/ bytes 1024.0)))
   (t (format "%.1fM" (/ bytes (* 1024.0 1024.0))))))

;;; Rendering

(defun claude-gravity--debug-render ()
  "Render the terminal protocol debug buffer."
  (let ((inhibit-read-only t)
        (pos (point))
        (messages claude-gravity--debug-messages))
    (erase-buffer)
    (claude-gravity--debug-insert-header (length messages))
    (dolist (msg messages)
      (when (claude-gravity--debug-passes-filter msg)
        (let ((mid (alist-get 'msg-id msg))
              (start (point)))
          (claude-gravity--debug-insert-message msg mid)
          (when (gethash mid claude-gravity--debug-expanded)
            (claude-gravity--debug-insert-expanded msg mid))
          (put-text-property start (point) 'claude-debug-mid mid))))
    (claude-gravity--debug-insert-footer)
    (goto-char (min pos (point-max)))))

(defun claude-gravity--debug-insert-header (count)
  "Insert buffer header with COUNT messages."
  (let ((filter-parts nil))
    (when claude-gravity--debug-filter-type
      (push (format "type: %s" claude-gravity--debug-filter-type) filter-parts))
    (when claude-gravity--debug-filter-session
      (push (format "session: %s..."
                    (substring claude-gravity--debug-filter-session
                               0 (min 7 (length claude-gravity--debug-filter-session))))
            filter-parts))
    (when claude-gravity--debug-filter-direction
      (push (format "dir: %s" claude-gravity--debug-filter-direction) filter-parts))
    (when claude-gravity--debug-filter-patch-op
      (push (format "patch-op: %s" claude-gravity--debug-filter-patch-op) filter-parts))
    (insert (propertize (make-string 80 ?\u2501) 'face 'claude-gravity-divider) "\n")
    (insert (propertize "Claude Debug: Terminal Protocol" 'face 'bold))
    (insert (propertize (format "  [%d msgs]" count) 'face 'claude-gravity-detail-label))
    (when filter-parts
      (insert (propertize (format "  %s" (string-join (nreverse filter-parts) "  "))
                          'face 'claude-gravity-detail-label)))
    (insert "\n")
    (insert (propertize (make-string 80 ?\u2501) 'face 'claude-gravity-divider) "\n\n")))

(defun claude-gravity--debug-insert-footer ()
  "Insert buffer footer with keybindings."
  (insert "\n" (propertize (make-string 80 ?\u2501) 'face 'claude-gravity-divider) "\n")
  (insert "RET=expand c=copy f=type s=session d=direction p=patch-op /=search x=clear\n"))

(defun claude-gravity--debug-insert-message (msg mid)
  "Insert one-line summary for MSG with message ID MID."
  (let* ((timestamp (alist-get 'timestamp msg))
         (direction (alist-get 'direction msg))
         (msg-type (alist-get 'msg-type msg))
         (raw (alist-get 'raw msg))
         (raw-size (if raw (string-bytes raw) 0))
         (size-str (claude-gravity--debug-format-size raw-size))
         (label (alist-get 'session-label msg))
         (time-str (format-time-string "%H:%M:%S" timestamp))
         (dir-char (if (eq direction 'incoming) "\u25c0" "\u25b6"))
         (dir-face (if (eq direction 'incoming)
                       'claude-gravity-tool-done
                     'claude-gravity-prompt))
         (summary (claude-gravity--debug-message-summary msg)))
    (let ((start (point)))
      (insert
       (propertize (format "[%s]" time-str) 'face 'claude-gravity-detail-label)
       " "
       (propertize dir-char 'face dir-face)
       " "
       (propertize (format "%-22s" (or msg-type "???")) 'face 'claude-gravity-tool-name)
       (propertize (format " %6s" size-str) 'face 'claude-gravity-detail-label)
       " "
       (if label
           (propertize (format "%-12s" label) 'face 'claude-gravity-slug)
         (propertize (make-string 12 ?\s) 'face 'default))
       " "
       summary
       "\n")
      (put-text-property start (point) 'claude-debug-mid mid))))

(defun claude-gravity--debug-insert-expanded (msg mid)
  "Insert expanded view of MSG with message ID MID.
For session.update: shows patches list (level 1).
For other types: shows raw JSON."
  (let ((msg-type (alist-get 'msg-type msg)))
    (if (equal msg-type "session.update")
        (claude-gravity--debug-insert-patches-expanded msg mid)
      (claude-gravity--debug-insert-raw-expanded msg))))

(defun claude-gravity--debug-insert-patches-expanded (msg mid)
  "Insert patches list for an expanded session.update MSG with MID."
  (let ((patches (alist-get 'patches msg)))
    (insert (propertize (format "  Patches (%d):\n" (length patches))
                        'face 'claude-gravity-detail-label))
    (let ((idx 0))
      (dolist (patch patches)
        (let* ((op (or (alist-get 'op patch) "?"))
               (detail (claude-gravity--debug-patch-detail-summary patch))
               (patch-json (condition-case nil (json-encode patch) (error "")))
               (patch-size (claude-gravity--debug-format-size (string-bytes patch-json)))
               (patch-key (format "%d:%d" mid idx))
               (patch-expanded (gethash patch-key claude-gravity--debug-expanded))
               (start (point)))
          (insert
           (propertize (format "    [%d] " (1+ idx)) 'face 'claude-gravity-detail-label)
           (propertize (format "%-18s" op) 'face 'claude-gravity-tool-name)
           (propertize (format " %6s " patch-size) 'face 'claude-gravity-detail-label)
           detail
           "\n")
          (put-text-property start (point) 'claude-debug-patch-idx idx)
          (when patch-expanded
            (let ((raw-start (point)))
              (insert (propertize "        " 'face 'default))
              (let ((json-str (condition-case nil
                                  (json-encode patch)
                                (error (format "%S" patch)))))
                (insert (propertize json-str 'face 'font-lock-comment-face)))
              (insert "\n")
              (put-text-property raw-start (point) 'claude-debug-patch-idx idx)))
          (cl-incf idx))))))

(defun claude-gravity--debug-insert-raw-expanded (msg)
  "Insert raw JSON expanded view for MSG."
  (let ((parsed (alist-get 'parsed msg))
        (raw (alist-get 'raw msg)))
    (insert (propertize "  Payload:\n" 'face 'claude-gravity-detail-label))
    (if parsed
        (claude-gravity--debug-insert-json parsed "    " 0)
      (insert (propertize (format "    %s\n" (or raw "(no data)"))
                          'face 'font-lock-comment-face)))
    (insert "\n")))

;;; JSON pretty-printer (reused from v2)

(defun claude-gravity--debug-insert-json (value prefix depth)
  "Insert VALUE as pretty-printed JSON at indentation PREFIX and DEPTH.
Objects and arrays are expanded with syntax highlighting."
  (let ((indent (concat prefix (make-string (* depth 2) ?\s)))
        (child-indent (concat prefix (make-string (* (1+ depth) 2) ?\s))))
    (cond
     ;; Alist (object)
     ((and (listp value) (consp (car value)) (symbolp (caar value)))
      (insert (propertize (concat indent "{\n") 'face 'claude-gravity-divider))
      (let ((items value)
            (first t))
        (while items
          (let* ((pair (car items))
                 (key (car pair))
                 (val (cdr pair)))
            (unless first (insert (propertize ",\n" 'face 'claude-gravity-divider)))
            (setq first nil)
            (insert (propertize (format "%s\"%s\"" child-indent key)
                                'face 'claude-gravity-tool-name))
            (insert (propertize ": " 'face 'claude-gravity-divider))
            (if (or (and (listp val) (consp (car-safe val)) (symbolp (car-safe (car-safe val))))
                    (and (listp val) (not (null val)) (not (symbolp (car-safe val)))))
                ;; Nested structure: newline then recurse
                (progn
                  (insert "\n")
                  (claude-gravity--debug-insert-json val prefix (1+ depth)))
              ;; Scalar: inline
              (claude-gravity--debug-insert-json-scalar val)))
          (setq items (cdr items))))
      (insert (propertize (format "\n%s}" indent) 'face 'claude-gravity-divider)))
     ;; List (array)
     ((and (listp value) (not (null value)))
      (insert (propertize (concat indent "[\n") 'face 'claude-gravity-divider))
      (let ((first t))
        (dolist (item value)
          (unless first (insert (propertize ",\n" 'face 'claude-gravity-divider)))
          (setq first nil)
          (if (or (and (listp item) (consp (car-safe item)))
                  (and (listp item) (not (null item))))
              (claude-gravity--debug-insert-json item prefix (1+ depth))
            (insert child-indent)
            (claude-gravity--debug-insert-json-scalar item))))
      (insert (propertize (format "\n%s]" indent) 'face 'claude-gravity-divider)))
     ;; Empty list / nil
     ((null value)
      (insert indent (propertize "null" 'face 'font-lock-constant-face)))
     ;; Scalar at top level
     (t
      (insert indent)
      (claude-gravity--debug-insert-json-scalar value)))))

(defun claude-gravity--debug-insert-json-scalar (value)
  "Insert scalar VALUE with appropriate face, inline (no newline)."
  (cond
   ((stringp value)
    (let ((display (claude-gravity--truncate value 120)))
      (insert (propertize (format "\"%s\"" (claude-gravity--debug-escape-json-string display))
                          'face 'font-lock-string-face))))
   ((numberp value)
    (insert (propertize (format "%s" value) 'face 'font-lock-constant-face)))
   ((eq value t)
    (insert (propertize "true" 'face 'font-lock-constant-face)))
   ((eq value :false)
    (insert (propertize "false" 'face 'font-lock-constant-face)))
   (t
    (insert (propertize (format "%S" value) 'face 'font-lock-comment-face)))))

(defun claude-gravity--debug-escape-json-string (str)
  "Escape newlines and tabs in STR for display."
  (replace-regexp-in-string
   "\t" "\\\\t"
   (replace-regexp-in-string
    "\n" "\\\\n"
    str)))

;;; Commands

(defun claude-gravity-debug-refresh ()
  "Refresh debug buffer."
  (interactive)
  (claude-gravity--debug-render))

(defun claude-gravity--debug-mid-at-point ()
  "Return the message ID at point, or nil."
  (get-text-property (point) 'claude-debug-mid))

(defun claude-gravity--debug-patch-idx-at-point ()
  "Return the patch index at point, or nil."
  (get-text-property (point) 'claude-debug-patch-idx))

(defun claude-gravity--debug-msg-at-point ()
  "Return the message alist at point, or nil."
  (when-let ((mid (claude-gravity--debug-mid-at-point)))
    (cl-find mid claude-gravity--debug-messages
             :key (lambda (m) (alist-get 'msg-id m)))))

(defun claude-gravity-debug-copy-raw ()
  "Copy raw JSON of message at point to kill ring."
  (interactive)
  (let* ((msg (claude-gravity--debug-msg-at-point))
         (raw (when msg (alist-get 'raw msg))))
    (if raw
        (progn
          (kill-new raw)
          (message "Copied raw JSON (%d chars)" (length raw)))
      (message "No message at point"))))

(defun claude-gravity-debug-copy-parsed ()
  "Copy parsed data of message at point to kill ring."
  (interactive)
  (let* ((msg (claude-gravity--debug-msg-at-point))
         (parsed (when msg (alist-get 'parsed msg))))
    (if parsed
        (let ((json-str (json-encode parsed)))
          (kill-new json-str)
          (message "Copied parsed data (%d chars)" (length json-str)))
      (message "No parsed data at point"))))

(defun claude-gravity-debug-toggle-expand ()
  "Toggle expansion at point.
On a message line: toggle message expansion (level 1).
On a patch line: toggle patch JSON expansion (level 2)."
  (interactive)
  (let ((mid (claude-gravity--debug-mid-at-point))
        (patch-idx (claude-gravity--debug-patch-idx-at-point)))
    (cond
     ;; On a patch line within an expanded session.update
     ((and mid patch-idx)
      (let ((key (format "%d:%d" mid patch-idx)))
        (if (gethash key claude-gravity--debug-expanded)
            (remhash key claude-gravity--debug-expanded)
          (puthash key t claude-gravity--debug-expanded)))
      (claude-gravity--debug-render))
     ;; On a message line
     (mid
      (if (gethash mid claude-gravity--debug-expanded)
          (progn
            ;; Also remove any expanded patches for this message
            (let ((prefix (format "%d:" mid)))
              (maphash (lambda (k _v)
                         (when (and (stringp k) (string-prefix-p prefix k))
                           (remhash k claude-gravity--debug-expanded)))
                       claude-gravity--debug-expanded))
            (remhash mid claude-gravity--debug-expanded))
        (puthash mid t claude-gravity--debug-expanded))
      (claude-gravity--debug-render)))))

(defun claude-gravity-debug-filter-type ()
  "Filter messages by message type."
  (interactive)
  (let* ((types (delete-dups
                 (delq nil (mapcar (lambda (msg) (alist-get 'msg-type msg))
                                   claude-gravity--debug-messages))))
         (choice (completing-read "Filter by type (empty=all): "
                                  (cons "ALL" types) nil t)))
    (setq claude-gravity--debug-filter-type
          (if (or (string-empty-p choice) (equal choice "ALL")) nil choice))
    (claude-gravity--debug-render)))

(defun claude-gravity-debug-filter-session ()
  "Filter messages by session."
  (interactive)
  (let* ((sessions (delete-dups
                    (delq nil (mapcar (lambda (msg)
                                       (let ((sid (alist-get 'session-id msg))
                                             (label (alist-get 'session-label msg)))
                                         (when sid (cons (or label (substring sid 0 (min 12 (length sid)))) sid))))
                                     claude-gravity--debug-messages))))
         (choice (completing-read "Filter by session (empty=all): "
                                  (cons '("ALL" . nil) sessions) nil t)))
    (setq claude-gravity--debug-filter-session
          (if (or (string-empty-p choice) (equal choice "ALL"))
              nil
            (cdr (assoc choice sessions))))
    (claude-gravity--debug-render)))

(defun claude-gravity-debug-filter-direction ()
  "Filter messages by direction."
  (interactive)
  (let ((choice (completing-read "Filter direction: "
                                 '("ALL" "incoming" "outgoing") nil t)))
    (setq claude-gravity--debug-filter-direction
          (pcase choice
            ("incoming" 'incoming)
            ("outgoing" 'outgoing)
            (_ nil)))
    (claude-gravity--debug-render)))

(defun claude-gravity-debug-filter-patch-op ()
  "Filter to session.update messages containing a specific patch op."
  (interactive)
  (let* ((ops nil)
         (_ (dolist (msg claude-gravity--debug-messages)
              (when (equal (alist-get 'msg-type msg) "session.update")
                (dolist (p (alist-get 'patches msg))
                  (when-let ((op (alist-get 'op p)))
                    (push op ops))))))
         (ops (delete-dups ops))
         (choice (completing-read "Filter by patch op (empty=all): "
                                  (cons "ALL" ops) nil t)))
    (setq claude-gravity--debug-filter-patch-op
          (if (or (string-empty-p choice) (equal choice "ALL")) nil choice))
    (claude-gravity--debug-render)))

(defun claude-gravity-debug-search ()
  "Search messages for a string in raw JSON."
  (interactive)
  (let* ((query (read-string "Search raw JSON: "))
         (matches 0))
    (dolist (msg claude-gravity--debug-messages)
      (when (string-match-p (regexp-quote query) (or (alist-get 'raw msg) ""))
        (cl-incf matches)))
    (message "Found %d message(s) matching \"%s\"" matches query)))

(defun claude-gravity-debug-clear ()
  "Clear all debug messages."
  (interactive)
  (when (yes-or-no-p "Clear all debug messages? ")
    (setq claude-gravity--debug-messages nil)
    (setq claude-gravity--debug-expanded (make-hash-table :test 'equal))
    (claude-gravity--debug-render)))

(provide 'claude-gravity-debug)
;;; claude-gravity-debug.el ends here
