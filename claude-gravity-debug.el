;;; claude-gravity-debug.el --- Debug message viewer for Claude Gravity  -*- lexical-binding: t; -*-

;;; Code:

(require 'claude-gravity-core)
(require 'claude-gravity-faces)

;;; Data model

(defvar claude-gravity--debug-messages nil
  "Ring buffer of captured bridge messages.
Each entry is an alist with keys: timestamp, event, session-id,
cwd, pid, needs-response, data, hook-input, raw, parse-error.")

(defvar claude-gravity--debug-messages-max 200
  "Maximum messages to retain (oldest dropped).")

(defvar claude-gravity--debug-messages-enabled nil
  "When non-nil, capture incoming bridge messages for debugging.")

;;; Capture

(defun claude-gravity--debug-capture-message (raw-json parsed-data parse-error)
  "Capture a bridge message for debugging.
RAW-JSON is the raw string, PARSED-DATA is the parsed alist (or nil),
PARSE-ERROR is the error object (or nil)."
  (when claude-gravity--debug-messages-enabled
    (let* ((event (when parsed-data (alist-get 'event parsed-data)))
           (session-id (when parsed-data (alist-get 'session_id parsed-data)))
           (cwd (when parsed-data (alist-get 'cwd parsed-data)))
           (pid (when parsed-data (alist-get 'pid parsed-data)))
           (needs-resp (when parsed-data (alist-get 'needs_response parsed-data)))
           (payload (when parsed-data (alist-get 'data parsed-data)))
           (hook-input (when parsed-data (alist-get 'hook_input parsed-data)))
           (entry `((timestamp . ,(current-time))
                    (event . ,event)
                    (session-id . ,session-id)
                    (cwd . ,cwd)
                    (pid . ,pid)
                    (needs-response . ,needs-resp)
                    (data . ,payload)
                    (hook-input . ,hook-input)
                    (raw . ,raw-json)
                    (parse-error . ,(when parse-error
                                      (error-message-string parse-error))))))
      (push entry claude-gravity--debug-messages)
      (when (> (length claude-gravity--debug-messages) claude-gravity--debug-messages-max)
        (setcdr (nthcdr (1- claude-gravity--debug-messages-max)
                        claude-gravity--debug-messages) nil))
      (claude-gravity--debug-schedule-refresh))))

;;; Refresh scheduling

(defvar claude-gravity--debug-refresh-timer nil
  "Timer for debounced debug buffer refresh.")

(defvar claude-gravity--debug-bridge-refresh-timer nil
  "Timer for debounced bridge debug buffer refresh.")

(defun claude-gravity--debug-schedule-refresh ()
  "Schedule debug buffer refresh if visible."
  (when claude-gravity--debug-messages-enabled
    (when (get-buffer-window "*Claude Debug: Messages*")
      (when claude-gravity--debug-refresh-timer
        (cancel-timer claude-gravity--debug-refresh-timer))
      (setq claude-gravity--debug-refresh-timer
            (run-with-idle-timer 0.1 nil #'claude-gravity--debug-do-refresh)))
    (when (get-buffer-window "*Claude Debug: Bridge*")
      (when claude-gravity--debug-bridge-refresh-timer
        (cancel-timer claude-gravity--debug-bridge-refresh-timer))
      (setq claude-gravity--debug-bridge-refresh-timer
            (run-with-idle-timer 0.1 nil #'claude-gravity--debug-bridge-do-refresh)))))

(defun claude-gravity--debug-do-refresh ()
  "Perform actual debug buffer refresh."
  (setq claude-gravity--debug-refresh-timer nil)
  (when-let ((buf (get-buffer "*Claude Debug: Messages*")))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (claude-gravity--debug-render)))))

(defun claude-gravity--debug-bridge-do-refresh ()
  "Perform actual bridge debug buffer refresh."
  (setq claude-gravity--debug-bridge-refresh-timer nil)
  (when-let ((buf (get-buffer "*Claude Debug: Bridge*")))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (claude-gravity--debug-bridge-render)))))

;;; Mode

(defvar claude-gravity-debug-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "g") #'claude-gravity-debug-refresh)
    (define-key map (kbd "c") #'claude-gravity-debug-copy-raw)
    (define-key map (kbd "C") #'claude-gravity-debug-copy-parsed)
    (define-key map (kbd "RET") #'claude-gravity-debug-toggle-expand)
    (define-key map (kbd "f") #'claude-gravity-debug-filter-event)
    (define-key map (kbd "s") #'claude-gravity-debug-filter-session)
    (define-key map (kbd "/") #'claude-gravity-debug-search)
    (define-key map (kbd "x") #'claude-gravity-debug-clear)
    map)
  "Keymap for `claude-gravity-debug-mode'.")

(define-derived-mode claude-gravity-debug-mode special-mode "Claude-Debug"
  "Major mode for Claude Gravity debug message viewer.
\\{claude-gravity-debug-mode-map}"
  (setq truncate-lines t)
  (setq buffer-read-only t))

(defvar-local claude-gravity--debug-filter-event nil
  "If non-nil, only show messages matching this event type.")

(defvar-local claude-gravity--debug-filter-session nil
  "If non-nil, only show messages from this session ID.")

(defvar-local claude-gravity--debug-expanded (make-hash-table :test 'equal)
  "Hash table of message indices that are expanded.")

;;; Entry point

(defun claude-gravity-debug-show ()
  "Show the debug message buffer."
  (interactive)
  (unless claude-gravity--debug-messages-enabled
    (setq claude-gravity--debug-messages-enabled t)
    (claude-gravity--log 'info "Debug message capture ENABLED"))
  (let ((buf (get-buffer-create "*Claude Debug: Messages*")))
    (with-current-buffer buf
      (unless (eq major-mode 'claude-gravity-debug-mode)
        (claude-gravity-debug-mode)
        (setq claude-gravity--debug-expanded (make-hash-table :test 'equal)))
      (claude-gravity--debug-render))
    (display-buffer-in-side-window buf '((side . right) (window-width . 90)))
    (select-window (get-buffer-window buf))))

;;; Filtering

(defun claude-gravity--debug-passes-filter (msg)
  "Return non-nil if MSG passes current filters."
  (and (or (null claude-gravity--debug-filter-event)
           (equal (alist-get 'event msg) claude-gravity--debug-filter-event))
       (or (null claude-gravity--debug-filter-session)
           (equal (alist-get 'session-id msg) claude-gravity--debug-filter-session))))

;;; Rendering

(defun claude-gravity--debug-render ()
  "Render the debug message buffer."
  (let ((inhibit-read-only t)
        (pos (point))
        ;; Messages are stored newest-first (push), display newest-first
        (messages claude-gravity--debug-messages)
        (idx 0))
    (erase-buffer)
    (claude-gravity--debug-insert-header (length messages))
    (dolist (msg messages)
      (when (claude-gravity--debug-passes-filter msg)
        (claude-gravity--debug-insert-message msg idx)
        (when (gethash idx claude-gravity--debug-expanded)
          (claude-gravity--debug-insert-expanded msg)))
      (setq idx (1+ idx)))
    (claude-gravity--debug-insert-footer)
    (goto-char (min pos (point-max)))))

(defun claude-gravity--debug-insert-header (count)
  "Insert buffer header with COUNT messages."
  (let ((filter-info
         (concat
          (when claude-gravity--debug-filter-event
            (format "  filter: %s" claude-gravity--debug-filter-event))
          (when claude-gravity--debug-filter-session
            (format "  session: %s..."
                    (substring claude-gravity--debug-filter-session
                               0 (min 7 (length claude-gravity--debug-filter-session))))))))
    (insert (propertize (make-string 80 ?\u2501) 'face 'claude-gravity-divider) "\n")
    (insert (propertize "Claude Debug: Bridge Messages" 'face 'bold))
    (insert (propertize (format "  [%d messages]" count) 'face 'claude-gravity-detail-label))
    (when filter-info
      (insert (propertize filter-info 'face 'claude-gravity-detail-label)))
    (insert "\n")
    (insert (propertize (make-string 80 ?\u2501) 'face 'claude-gravity-divider) "\n\n")))

(defun claude-gravity--debug-insert-footer ()
  "Insert buffer footer with keybindings."
  (insert "\n" (propertize (make-string 80 ?\u2501) 'face 'claude-gravity-divider) "\n")
  (insert "  RET=expand  c=copy-raw  C=copy-parsed  f=filter  s=session  /=search  x=clear  g=refresh  q=quit\n"))

(defun claude-gravity--debug-insert-message (msg idx)
  "Insert one-line summary for MSG at index IDX."
  (let* ((timestamp (alist-get 'timestamp msg))
         (event (alist-get 'event msg))
         (session-id (alist-get 'session-id msg))
         (needs-resp (alist-get 'needs-response msg))
         (parse-error (alist-get 'parse-error msg))
         (data (alist-get 'data msg))
         (time-str (format-time-string "%H:%M:%S.%3N" timestamp))
         (event-str (or event "???"))
         (sid-str (when session-id
                    (substring session-id 0 (min 7 (length session-id)))))
         (summary (claude-gravity--debug-message-summary event data))
         (expanded (gethash idx claude-gravity--debug-expanded)))
    (let ((start (point)))
      (insert
       (if parse-error
           (propertize "[ERROR]" 'face 'error)
         (propertize (format "[%s]" time-str) 'face 'claude-gravity-detail-label))
       " "
       (propertize (format "%-18s" event-str) 'face 'claude-gravity-tool-name)
       " "
       (if sid-str
           (propertize (format "%s " sid-str) 'face 'claude-gravity-slug)
         "")
       (if needs-resp
           (propertize "(AWAITING) " 'face 'claude-gravity-question)
         "")
       (if expanded
           (propertize "[-] " 'face 'claude-gravity-detail-label)
         (propertize "[+] " 'face 'claude-gravity-detail-label))
       summary
       "\n")
      (put-text-property start (point) 'claude-debug-idx idx))
    (when parse-error
      (insert (propertize (format "  Parse error: %s\n" parse-error) 'face 'error)))))

(defun claude-gravity--debug-message-summary (event data)
  "Generate compact summary for EVENT with DATA."
  (condition-case nil
      (pcase event
        ("PreToolUse"
         (let ((tool-name (alist-get 'tool_name data))
               (tool-id (alist-get 'tool_use_id data)))
           (format "%s  id=%s"
                   (or tool-name "?")
                   (if tool-id
                       (substring tool-id 0 (min 8 (length tool-id)))
                     "?"))))
        ("PostToolUse"
         (let ((tool-name (alist-get 'tool_name data))
               (tool-id (alist-get 'tool_use_id data)))
           (format "%s  id=%s  completed"
                   (or tool-name "?")
                   (if tool-id
                       (substring tool-id 0 (min 8 (length tool-id)))
                     "?"))))
        ("PostToolUseFailure"
         (let ((tool-name (alist-get 'tool_name data))
               (tool-id (alist-get 'tool_use_id data)))
           (format "%s  id=%s  FAILED"
                   (or tool-name "?")
                   (if tool-id
                       (substring tool-id 0 (min 8 (length tool-id)))
                     "?"))))
        ("UserPromptSubmit"
         (let ((prompt (alist-get 'prompt data)))
           (format "prompt: %s" (claude-gravity--truncate (or prompt "") 40))))
        ("SessionStart"
         (let ((project (alist-get 'project data)))
           (format "project=%s" (or project "?"))))
        ("SessionEnd" "session ended")
        ("SubagentStart"
         (let ((agent-id (alist-get 'agent_id data))
               (agent-type (alist-get 'agent_type data)))
           (format "%s (%s)" (or agent-type "?") (or agent-id "?"))))
        ("SubagentStop"
         (let ((agent-id (alist-get 'agent_id data)))
           (format "agent=%s stopped" (or agent-id "?"))))
        ("Stop" "turn complete")
        ("Notification"
         (let ((title (alist-get 'title data)))
           (claude-gravity--truncate (or title "") 50)))
        ("PermissionRequest"
         (let ((tool-name (alist-get 'tool_name data)))
           (format "tool=%s" (or tool-name "?"))))
        (_ ""))
    (error "")))

(defun claude-gravity--debug-insert-expanded (msg)
  "Insert expanded view of MSG payload with pretty-printed JSON."
  (let* ((raw (alist-get 'raw msg))
         (data (or (alist-get 'data msg)
                   (when raw
                     (condition-case nil
                         (json-parse-string raw :object-type 'alist :array-type 'list)
                       (error nil))))))
    (insert (propertize "  Payload:\n" 'face 'claude-gravity-detail-label))
    (if data
        (claude-gravity--debug-insert-json data "    " 0)
      (insert (propertize (format "    %s\n" (or raw "(no data)"))
                          'face 'font-lock-comment-face)))
    (insert "\n")))

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
    (let ((display (if (> (length value) 120)
                       (concat (substring value 0 117) "...")
                     value)))
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

(defun claude-gravity--debug-idx-at-point ()
  "Return the message index at point, or nil."
  (get-text-property (line-beginning-position) 'claude-debug-idx))

(defun claude-gravity--debug-msg-at-point ()
  "Return the message alist at point, or nil."
  (when-let ((idx (claude-gravity--debug-idx-at-point)))
    (nth idx claude-gravity--debug-messages)))

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
         (data (when msg (alist-get 'data msg))))
    (if data
        (let ((json-str (json-encode data)))
          (kill-new json-str)
          (message "Copied parsed data (%d chars)" (length json-str)))
      (message "No parsed data at point"))))

(defun claude-gravity-debug-toggle-expand ()
  "Expand/collapse message at point."
  (interactive)
  (when-let ((idx (claude-gravity--debug-idx-at-point)))
    (if (gethash idx claude-gravity--debug-expanded)
        (remhash idx claude-gravity--debug-expanded)
      (puthash idx t claude-gravity--debug-expanded))
    (claude-gravity--debug-render)))

(defun claude-gravity-debug-filter-event ()
  "Filter messages by event type."
  (interactive)
  (let* ((events (delete-dups
                  (delq nil (mapcar (lambda (msg) (alist-get 'event msg))
                                    claude-gravity--debug-messages))))
         (choice (completing-read "Filter by event (empty=all): "
                                  (cons "ALL" events) nil t)))
    (setq claude-gravity--debug-filter-event
          (if (or (string-empty-p choice) (equal choice "ALL")) nil choice))
    (claude-gravity--debug-render)))

(defun claude-gravity-debug-filter-session ()
  "Filter messages by session ID."
  (interactive)
  (let* ((sessions (delete-dups
                    (delq nil (mapcar (lambda (msg) (alist-get 'session-id msg))
                                      claude-gravity--debug-messages))))
         (display (mapcar (lambda (sid)
                            (cons (substring sid 0 (min 12 (length sid))) sid))
                          sessions))
         (choice (completing-read "Filter by session (empty=all): "
                                  (cons '("ALL" . nil) display) nil t)))
    (setq claude-gravity--debug-filter-session
          (if (or (string-empty-p choice) (equal choice "ALL"))
              nil
            (cdr (assoc choice display))))
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


;;; ================================================================
;;; Bridge Debug Viewer — raw hook input vs enriched output
;;; ================================================================

(defvar claude-gravity-debug-bridge-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "g") #'claude-gravity-debug-bridge-refresh)
    (define-key map (kbd "c") #'claude-gravity-debug-bridge-copy-hook-input)
    (define-key map (kbd "C") #'claude-gravity-debug-bridge-copy-enriched)
    (define-key map (kbd "RET") #'claude-gravity-debug-bridge-toggle-expand)
    (define-key map (kbd "f") #'claude-gravity-debug-filter-event)
    (define-key map (kbd "s") #'claude-gravity-debug-filter-session)
    (define-key map (kbd "/") #'claude-gravity-debug-search)
    (define-key map (kbd "x") #'claude-gravity-debug-clear)
    map)
  "Keymap for `claude-gravity-debug-bridge-mode'.")

(define-derived-mode claude-gravity-debug-bridge-mode special-mode "Claude-Bridge-Debug"
  "Major mode for Claude Gravity bridge debug viewer.
Shows raw hook inputs and enrichment delta.
\\{claude-gravity-debug-bridge-mode-map}"
  (setq truncate-lines t)
  (setq buffer-read-only t))

(defvar-local claude-gravity--debug-bridge-expanded (make-hash-table :test 'equal)
  "Hash table of message indices that are expanded in bridge debug buffer.")

;;; Bridge debug entry point

(defun claude-gravity-debug-bridge-show ()
  "Show the bridge debug buffer (raw hook input vs enriched output)."
  (interactive)
  (unless claude-gravity--debug-messages-enabled
    (setq claude-gravity--debug-messages-enabled t)
    (claude-gravity--log 'info "Debug message capture ENABLED"))
  (let ((buf (get-buffer-create "*Claude Debug: Bridge*")))
    (with-current-buffer buf
      (unless (eq major-mode 'claude-gravity-debug-bridge-mode)
        (claude-gravity-debug-bridge-mode)
        (setq claude-gravity--debug-bridge-expanded (make-hash-table :test 'equal)))
      (claude-gravity--debug-bridge-render))
    (display-buffer-in-side-window buf '((side . right) (window-width . 100)))
    (select-window (get-buffer-window buf))))

;;; Bridge debug rendering

(defun claude-gravity--debug-bridge-render ()
  "Render the bridge debug buffer."
  (let ((inhibit-read-only t)
        (pos (point))
        (messages claude-gravity--debug-messages)
        (idx 0))
    (erase-buffer)
    (claude-gravity--debug-bridge-insert-header (length messages))
    (dolist (msg messages)
      (when (claude-gravity--debug-passes-filter msg)
        (claude-gravity--debug-insert-message msg idx)
        (when (gethash idx claude-gravity--debug-bridge-expanded)
          (claude-gravity--debug-bridge-insert-expanded msg)))
      (setq idx (1+ idx)))
    (claude-gravity--debug-bridge-insert-footer)
    (goto-char (min pos (point-max)))))

(defun claude-gravity--debug-bridge-insert-header (count)
  "Insert bridge debug buffer header with COUNT messages."
  (let ((filter-info
         (concat
          (when claude-gravity--debug-filter-event
            (format "  filter: %s" claude-gravity--debug-filter-event))
          (when claude-gravity--debug-filter-session
            (format "  session: %s..."
                    (substring claude-gravity--debug-filter-session
                               0 (min 7 (length claude-gravity--debug-filter-session))))))))
    (insert (propertize (make-string 96 ?\u2501) 'face 'claude-gravity-divider) "\n")
    (insert (propertize "Claude Debug: Bridge" 'face 'bold))
    (insert (propertize "  (raw hook input vs enriched)" 'face 'claude-gravity-detail-label))
    (insert (propertize (format "  [%d messages]" count) 'face 'claude-gravity-detail-label))
    (when filter-info
      (insert (propertize filter-info 'face 'claude-gravity-detail-label)))
    (insert "\n")
    (insert (propertize (make-string 96 ?\u2501) 'face 'claude-gravity-divider) "\n\n")))

(defun claude-gravity--debug-bridge-insert-footer ()
  "Insert bridge debug buffer footer."
  (insert "\n" (propertize (make-string 96 ?\u2501) 'face 'claude-gravity-divider) "\n")
  (insert "  RET=expand  c=copy-hook-input  C=copy-enriched  f=filter  s=session  /=search  x=clear  g=refresh  q=quit\n"))

(defun claude-gravity--debug-bridge-insert-expanded (msg)
  "Insert expanded bridge view of MSG showing hook input and enrichment delta."
  (let ((hook-input (alist-get 'hook-input msg))
        (data (alist-get 'data msg)))
    ;; Section 1: Raw hook input
    (insert (propertize "  Hook Input (raw from Claude Code):\n"
                        'face '(:foreground "#88aacc" :weight bold)))
    (if hook-input
        (claude-gravity--debug-insert-json hook-input "    " 0)
      (insert (propertize "    (not available — bridge may need rebuild)\n"
                          'face 'font-lock-comment-face)))
    (insert "\n")
    ;; Section 2: Enrichment delta (keys in data but not in hook-input)
    (insert (propertize "  Enriched (bridge added):\n"
                        'face '(:foreground "#ccaa88" :weight bold)))
    (let ((delta (claude-gravity--debug-compute-delta hook-input data)))
      (if delta
          (claude-gravity--debug-insert-json delta "    " 0)
        (insert (propertize "    (no enrichment — identical to hook input)\n"
                            'face 'font-lock-comment-face))))
    (insert "\n\n")))

(defun claude-gravity--debug-compute-delta (hook-input data)
  "Compute enrichment delta: keys in DATA not present in HOOK-INPUT.
Returns an alist of added/changed fields, or nil if identical."
  (when (and data hook-input)
    (let ((delta nil))
      (cond
       ;; Both are alists
       ((and (listp data) (consp (car-safe data)) (symbolp (car-safe (car-safe data)))
             (listp hook-input) (consp (car-safe hook-input)) (symbolp (car-safe (car-safe hook-input))))
        (dolist (pair data)
          (let* ((key (car pair))
                 (val (cdr pair))
                 (original (alist-get key hook-input)))
            (unless (equal val original)
              (push pair delta))))
        (nreverse delta))
       ;; Data exists but hook-input is nil — entire data is delta
       (t data)))))

;;; Bridge debug commands

(defun claude-gravity-debug-bridge-refresh ()
  "Refresh bridge debug buffer."
  (interactive)
  (claude-gravity--debug-bridge-render))

(defun claude-gravity-debug-bridge-toggle-expand ()
  "Expand/collapse message at point in bridge debug buffer."
  (interactive)
  (when-let ((idx (claude-gravity--debug-idx-at-point)))
    (if (gethash idx claude-gravity--debug-bridge-expanded)
        (remhash idx claude-gravity--debug-bridge-expanded)
      (puthash idx t claude-gravity--debug-bridge-expanded))
    (claude-gravity--debug-bridge-render)))

(defun claude-gravity-debug-bridge-copy-hook-input ()
  "Copy raw hook input of message at point to kill ring."
  (interactive)
  (let* ((msg (claude-gravity--debug-msg-at-point))
         (hook-input (when msg (alist-get 'hook-input msg))))
    (if hook-input
        (let ((json-str (json-encode hook-input)))
          (kill-new json-str)
          (message "Copied hook input (%d chars)" (length json-str)))
      (message "No hook input at point"))))

(defun claude-gravity-debug-bridge-copy-enriched ()
  "Copy enrichment delta of message at point to kill ring."
  (interactive)
  (let* ((msg (claude-gravity--debug-msg-at-point))
         (hook-input (when msg (alist-get 'hook-input msg)))
         (data (when msg (alist-get 'data msg)))
         (delta (when msg (claude-gravity--debug-compute-delta hook-input data))))
    (if delta
        (let ((json-str (json-encode delta)))
          (kill-new json-str)
          (message "Copied enrichment delta (%d chars)" (length json-str)))
      (message "No enrichment delta at point"))))

(provide 'claude-gravity-debug)
;;; claude-gravity-debug.el ends here
