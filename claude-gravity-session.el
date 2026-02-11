;;; claude-gravity-session.el --- Session registry for Claude Gravity  -*- lexical-binding: t; -*-

;;; Code:

(require 'claude-gravity-core)

; Forward declarations for functions defined in other modules
(declare-function claude-gravity--load-allow-patterns "claude-gravity-session")
(declare-function claude-gravity--make-turn-node "claude-gravity-state")


;;; Session Registry

(defvar claude-gravity--sessions (make-hash-table :test 'equal)
  "Hash table mapping session-id -> session plist.
Each session plist has keys:
  :session-id      - string
  :cwd             - string (project working directory)
  :project         - string (basename of cwd)
  :turns           - tlist of turn-node alists (tree model)
  :tool-index      - hash-table tool_use_id -> tool alist
  :agent-index     - hash-table agent_id -> agent alist
  :tasks           - hash-table taskId -> task alist
  :files           - hash-table path -> file alist
  :status          - symbol: active or ended
  :start-time      - time value
  :last-event-time - time value (updated on every event)
  :pid             - integer (Claude Code process ID) or nil
  :plan            - plist (:content STRING) or nil")


(defvar-local claude-gravity--buffer-session-id nil
  "Session ID for this per-session buffer.")


(defun claude-gravity--get-session (session-id)
  "Return session plist for SESSION-ID, or nil."
  (claude-gravity--migrate-session (gethash session-id claude-gravity--sessions)))


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


(defun claude-gravity--migrate-session (session)
  "Migrate SESSION to ensure required hash tables exist.
Ensures :tool-index, :agent-index, and :turns exist. Idempotent."
  (when session
    ;; Migrate :notifications from vector to list
    (let ((val (plist-get session :notifications)))
      (when (vectorp val)
        (plist-put session :notifications (append val nil))))
    ;; Ensure agent-index exists
    (unless (hash-table-p (plist-get session :agent-index))
      (plist-put session :agent-index (make-hash-table :test 'equal)))
    ;; Ensure tool-index exists
    (unless (hash-table-p (plist-get session :tool-index))
      (plist-put session :tool-index (make-hash-table :test 'equal)))
    ;; Ensure :turns tlist exists
    (unless (plist-get session :turns)
      (let ((tl (claude-gravity--tlist-new)))
        (claude-gravity--tlist-append tl (claude-gravity--make-turn-node 0))
        (plist-put session :turns tl))))
  session)


(defun claude-gravity--ensure-session (session-id cwd)
  "Get or create session for SESSION-ID with CWD.  Returns session plist."
  (or (claude-gravity--migrate-session (gethash session-id claude-gravity--sessions))
      (let ((session (list :session-id session-id
                           :cwd (claude-gravity--normalize-cwd (or cwd ""))
                           :project (file-name-nondirectory
                                     (directory-file-name (or cwd "")))
                           :status 'active
                           :claude-status 'idle
                           :start-time (current-time)
                           :last-event-time (current-time)
                           :pid nil
                           :plan nil
                           :files (make-hash-table :test 'equal)
                           :tasks (make-hash-table :test 'equal)
                           :tool-index (make-hash-table :test 'equal)
                           :agent-index (make-hash-table :test 'equal)
                           :turns (let ((tl (claude-gravity--tlist-new)))
                                    (claude-gravity--tlist-append
                                     tl (claude-gravity--make-turn-node 0))
                                    tl)
                           :current-turn 0
                           :total-tool-count 0
                           :header-line-cache nil
                           :permission-mode nil
                           :slug nil
                           :buffer nil)))
        (puthash session-id session claude-gravity--sessions)
        (claude-gravity--load-allow-patterns session)
        session)))


(defun claude-gravity--reset-session (session)
  "Reset conversational state of SESSION, preserving identity fields.
Called when a session is restarted (e.g. via /reset or /clear)."
  (plist-put session :claude-status 'idle)
  (plist-put session :start-time (current-time))
  (plist-put session :last-event-time (current-time))
  (plist-put session :plan nil)
  (plist-put session :files (make-hash-table :test 'equal))
  (plist-put session :tasks (make-hash-table :test 'equal))
  (plist-put session :tool-index (make-hash-table :test 'equal))
  (plist-put session :agent-index (make-hash-table :test 'equal))
  (let ((tl (claude-gravity--tlist-new)))
    (claude-gravity--tlist-append tl (claude-gravity--make-turn-node 0))
    (plist-put session :turns tl))
  (plist-put session :current-turn 0)
  (plist-put session :total-tool-count 0)
  (plist-put session :header-line-cache nil)
  (plist-put session :permission-mode nil)
  (plist-put session :slug nil)
  (plist-put session :status 'active)
  (claude-gravity--load-allow-patterns session)
  (claude-gravity--log 'debug "Claude Gravity: session %s reset" (plist-get session :session-id))
  session)


(defun claude-gravity--load-allow-patterns (session)
  "Load allow patterns from .claude/settings.local.json for SESSION.
Stores the patterns list on SESSION's :allow-patterns property."
  (let* ((cwd (plist-get session :cwd))
         (settings-path (expand-file-name ".claude/settings.local.json" cwd)))
    (if (and cwd (not (string-empty-p cwd)) (file-exists-p settings-path))
        (condition-case err
            (let* ((data (claude-gravity--json-read-file settings-path))
                   (perms (alist-get 'permissions data))
                   (allow (alist-get 'allow perms)))
              (plist-put session :allow-patterns (or allow nil)))
          (error
           (claude-gravity--log 'error "Claude Gravity: failed to read allow patterns: %s" err)
           (plist-put session :allow-patterns nil)))
      (plist-put session :allow-patterns nil))))


(defun claude-gravity--session-total-elapsed (session)
  "Sum elapsed seconds across all prompts in SESSION's turn tree."
  (let ((total 0.0))
    (dolist (turn-node (claude-gravity--tlist-items (plist-get session :turns)))
      (let* ((p (alist-get 'prompt turn-node))
             (e (and p (listp p) (alist-get 'elapsed p))))
        (when (numberp e) (cl-incf total e))))
    (if (> total 0) total
      ;; Fallback: wall-clock from start to last event
      (let ((start (plist-get session :start-time))
            (last-ev (plist-get session :last-event-time)))
        (when (and start last-ev)
          (float-time (time-subtract last-ev start)))))))

(provide 'claude-gravity-session)
;;; claude-gravity-session.el ends here