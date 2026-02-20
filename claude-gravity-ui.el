;;; claude-gravity-ui.el --- UI buffers and commands for Claude Gravity  -*- lexical-binding: t; -*-

;;; Code:

(require 'claude-gravity-core)
(require 'claude-gravity-faces)
(require 'claude-gravity-session)
(require 'claude-gravity-discovery)
(require 'claude-gravity-state)
(require 'claude-gravity-render)

(declare-function claude-gravity-server-alive-p "claude-gravity-socket")
(declare-function claude-gravity--write-allow-pattern-for-tool "claude-gravity-socket")
(declare-function claude-gravity--send-permission-response "claude-gravity-socket")
(declare-function claude-gravity--current-session-tmux-p "claude-gravity-tmux")
(declare-function claude-gravity--tmux-sync-width-for-buffer "claude-gravity-tmux")
(declare-function claude-gravity-toggle-permission-mode "claude-gravity-tmux")
(declare-function claude-gravity--current-session-daemon-p "claude-gravity-daemon")
(declare-function claude-gravity-daemon-start-session "claude-gravity-daemon")
(declare-function claude-gravity-daemon-resume-session "claude-gravity-daemon")
(declare-function claude-gravity-daemon-send-prompt "claude-gravity-daemon")
(declare-function claude-gravity-daemon-stop-session "claude-gravity-daemon")
(declare-function claude-gravity-daemon-interrupt "claude-gravity-daemon")
(declare-function claude-gravity-daemon-set-model "claude-gravity-daemon")
(declare-function claude-gravity-daemon-compose-prompt "claude-gravity-daemon")
(declare-function claude-gravity--current-session-managed-p "claude-gravity-daemon")
(declare-function claude-gravity-unified-compose "claude-gravity-daemon")
(declare-function claude-gravity-unified-stop "claude-gravity-daemon")
(declare-function claude-gravity-unified-interrupt "claude-gravity-daemon")
(declare-function claude-gravity-unified-resume "claude-gravity-daemon")
(declare-function claude-gravity-debug-show "claude-gravity-debug")
(defvar claude-gravity--tmux-sessions)


(defun claude-gravity--branch-or-cwd (session)
  "Return propertized branch name or abbreviated cwd for SESSION.
Returns nil if neither is available."
  (let ((branch (plist-get session :branch))
        (cwd (plist-get session :cwd)))
    (cond
     (branch
      (let ((display (if (> (length branch) 40)
                         (concat (substring branch 0 37) "...")
                       branch)))
        (propertize (format "(%s)" display) 'face 'claude-gravity-branch)))
     ((and cwd (not (string-empty-p cwd)))
      (propertize (format "(%s)" (abbreviate-file-name cwd))
                  'face 'claude-gravity-branch))
     (t nil))))


(defun claude-gravity--source-indicator (session)
  "Return propertized source indicator for SESSION.
Shows 🖥 for tmux sessions, [OC] for OpenCode, 🔌 for hook-based sessions."
  (let ((source (plist-get session :source))
        (sid (plist-get session :session-id)))
    (cond
     ((equal source "opencode")
      (propertize "[OC]" 'face 'claude-gravity-detail-label))
     ((gethash sid claude-gravity--tmux-sessions)
      (propertize "🖥" 'face 'claude-gravity-detail-label))
     (t
      (propertize "🔌" 'face 'claude-gravity-detail-label)))))


;;; Overview Buffer

(defun claude-gravity--inbox-badges (session-id)
  "Return badge string for inbox items belonging to SESSION-ID.
Only counts non-idle items.  Returns empty string if none."
  (let ((perms 0) (questions 0) (plans 0))
    (dolist (item claude-gravity--inbox)
      (when (equal (alist-get 'session-id item) session-id)
        (pcase (alist-get 'type item)
          ('permission (cl-incf perms))
          ('question (cl-incf questions))
          ('plan-review (cl-incf plans))
          (_ nil))))
    (let ((parts nil))
      (when (> plans 0)
        (push (propertize (format "P%d" plans) 'face 'claude-gravity-question) parts))
      (when (> questions 0)
        (push (propertize (format "?%d" questions) 'face 'claude-gravity-status-responding) parts))
      (when (> perms 0)
        (push (propertize (format "!%d" perms) 'face 'claude-gravity-question) parts))
      (if parts (concat " " (string-join parts " ")) ""))))


(defun claude-gravity--insert-inbox-summary ()
  "Insert a one-line summary strip of all pending inbox items.
Groups non-idle items by session and shows badge counts."
  (let ((by-session (make-hash-table :test 'equal)))
    ;; Group non-idle items by session-id
    (dolist (item claude-gravity--inbox)
      (unless (eq (alist-get 'type item) 'idle)
        (let ((sid (alist-get 'session-id item)))
          (puthash sid (cons item (gethash sid by-session nil)) by-session))))
    (when (> (hash-table-count by-session) 0)
      (let ((parts nil)
            (total 0))
        (maphash
         (lambda (sid items)
           (let ((session (claude-gravity--get-session sid))
                 (perms 0) (questions 0) (plans 0))
             (dolist (item items)
               (cl-incf total)
               (pcase (alist-get 'type item)
                 ('permission (cl-incf perms))
                 ('question (cl-incf questions))
                 ('plan-review (cl-incf plans))))
             (let ((label (if session
                              (format "%s/%s"
                                      (plist-get session :project)
                                      (claude-gravity--session-label session))
                            (substring sid 0 (min 4 (length sid)))))
                   (badges nil))
               (when (> perms 0)
                 (push (propertize (format "!%d" perms) 'face 'claude-gravity-question) badges))
               (when (> questions 0)
                 (push (propertize (format "?%d" questions) 'face 'claude-gravity-status-responding) badges))
               (when (> plans 0)
                 (push (propertize (format "P%d" plans) 'face 'claude-gravity-question) badges))
               (push (concat (propertize label 'face 'claude-gravity-detail-label)
                              " " (string-join badges " "))
                     parts))))
         by-session)
        (magit-insert-section (inbox-summary)
          (magit-insert-heading
            (format "  %s: %s"
                    (propertize (format "%d pending" total) 'face 'claude-gravity-question)
                    (string-join (nreverse parts) (propertize " · " 'face 'claude-gravity-detail-label)))))
        (insert "\n")))))


;;; Capabilities Section (Skills, Agents, Commands)

(defun claude-gravity--insert-capability-entry (cap)
  "Insert a single capability CAP as a magit-section.
CAP is an alist with keys: name, description, scope, file-path, type."
  (let* ((type (alist-get 'type cap))
         (name (alist-get 'name cap))
         (scope (alist-get 'scope cap))
         (desc (alist-get 'description cap))
         (indent (claude-gravity--indent))
         (prefix (pcase type
                   ('skill "S ")
                   ('agent "A ")
                   ('command "/ ")
                   ('mcp-server "M ")
                   (_ "  ")))
         (name-face (pcase type
                      ('skill 'claude-gravity-tool-name)
                      ('agent 'claude-gravity-tool-name)
                      ('command 'claude-gravity-tool-signature)
                      ('mcp-server 'claude-gravity-tool-signature)
                      (_ 'default)))
         ;; Determine scope label for agents
         (scope-label (if (eq type 'agent)
                          (let ((agent-type (claude-gravity--agent-scope cap)))
                            (pcase agent-type
                              ('built-in "(built-in)")
                              ('plugin (format "(%s)" scope))
                              (_ (format "(%s)" scope))))
                        (format "(%s)" scope))))
    (magit-insert-section (capability-entry cap t)
      (magit-insert-heading
        (format "%s%s%s  %s"
                indent
                (propertize prefix 'face 'claude-gravity-detail-label)
                (propertize name 'face name-face)
                (propertize scope-label 'face 'claude-gravity-detail-label)))
      ;; Expanded content: full frontmatter fields (untruncated)
      (let ((body-indent (concat indent "    "))
            (file-path (alist-get 'file-path cap))
            (frontmatter (alist-get 'frontmatter cap))
            (shown-fields '(name description)))
        ;; Show full description without truncation
        (when (and desc (not (string-empty-p desc)))
          (insert body-indent
                  (propertize "Description: " 'face 'claude-gravity-detail-label)
                  (propertize desc 'face 'default)
                  "\n"))
        ;; Show file path if it exists
        (when file-path
          (insert body-indent
                  (propertize "File: " 'face 'claude-gravity-detail-label)
                  (propertize file-path 'face 'default)
                  "\n"))
        ;; Show other frontmatter fields (excluding already shown fields)
        (when frontmatter
          (dolist (field frontmatter)
            (let ((key (car field))
                  (val (cdr field)))
              (unless (or (member key shown-fields) (string-empty-p val))
                (insert body-indent
                        (propertize (format "%s: " (capitalize (symbol-name key)))
                                    'face 'claude-gravity-detail-label)
                        (propertize val 'face 'default)
                        "\n")))))))))


(defun claude-gravity--insert-capability-category (title caps)
  "Insert collapsible category TITLE with list of CAPS.
Used for standalone skills/agents/commands sections."
  (when caps
    (let ((indent (claude-gravity--indent)))
      (magit-insert-section (category title t)
        (magit-insert-heading
          (format "%s%s (%d)"
                  indent
                  (propertize title 'face 'claude-gravity-section-heading)
                  (length caps)))
        (dolist (cap caps)
          (claude-gravity--insert-capability-entry cap))))))


(defun claude-gravity--insert-plugin-capabilities (plugin)
  "Insert capabilities section for a single PLUGIN."
  (let* ((name (alist-get 'name plugin))
         (scope (alist-get 'scope plugin))
         (skills (alist-get 'skills plugin))
         (agents (alist-get 'agents plugin))
         (commands (alist-get 'commands plugin))
         (mcp (alist-get 'mcp-servers plugin))
         (total (alist-get 'total plugin))
         (indent (claude-gravity--indent)))
    (when (> total 0)
      (magit-insert-section (plugin-entry name t)
        ;; Plugin header with counts
        (magit-insert-heading
          (format "%s%s  %s — %d items"
                  indent
                  (propertize name 'face 'claude-gravity-section-heading)
                  (propertize (format "(%s)" scope) 'face 'claude-gravity-detail-label)
                  total))
        ;; Skills subsection
        (when skills
          (let ((subindent (concat indent "  ")))
            (magit-insert-section (skills "skills" t)
              (magit-insert-heading
                (format "%sSkills (%d)"
                        subindent
                        (length skills)))
              (dolist (s skills)
                (claude-gravity--insert-capability-entry s)))))
        ;; Agents subsection
        (when agents
          (let ((subindent (concat indent "  ")))
            (magit-insert-section (agents "agents" t)
              (magit-insert-heading
                (format "%sAgents (%d)"
                        subindent
                        (length agents)))
              (dolist (a agents)
                (claude-gravity--insert-capability-entry a)))))
        ;; Commands subsection
        (when commands
          (let ((subindent (concat indent "  ")))
            (magit-insert-section (commands "commands" t)
              (magit-insert-heading
                (format "%sCommands (%d)"
                        subindent
                        (length commands)))
              (dolist (c commands)
                (claude-gravity--insert-capability-entry c)))))
        ;; MCP servers subsection
        (when mcp
          (let ((subindent (concat indent "  ")))
            (magit-insert-section (mcp-section "mcp" t)
              (magit-insert-heading
                (format "%sMCP Tools (%d)"
                        subindent
                        (length mcp)))
              (dolist (m mcp)
                (claude-gravity--insert-capability-entry m)))))))))


(defun claude-gravity--insert-hierarchical-capabilities (project-dir)
  "Insert hierarchical capabilities section for PROJECT-DIR.
Shows plugins grouped in Plugins section, standalone categories as siblings."
  (let ((grouped (or (claude-gravity--discover-project-capabilities project-dir)
                     '((plugins . nil)
                       (standalone-skills . nil)
                       (standalone-agents . nil)
                       (standalone-commands . nil)
                       (standalone-mcp-servers . nil)))))
    (let ((plugins (alist-get 'plugins grouped))
          (standalone-skills (alist-get 'standalone-skills grouped))
          (standalone-agents (alist-get 'standalone-agents grouped))
          (standalone-commands (alist-get 'standalone-commands grouped))
          (standalone-mcp (alist-get 'standalone-mcp-servers grouped)))
      (when (or plugins standalone-skills standalone-agents standalone-commands standalone-mcp)
        (let ((indent (claude-gravity--indent))
              (total (claude-gravity--capabilities-total-count grouped)))
          (magit-insert-section (capabilities project-dir t)
            (magit-insert-heading
              (format "%s%s"
                      indent
                      (propertize (format "Capabilities (%d total)" total)
                                  'face 'claude-gravity-section-heading)))
            ;; Plugins section (same visual style as standalone categories)
            (when plugins
              (let ((indent (claude-gravity--indent)))
                (magit-insert-section (category "Plugins" t)
                  (magit-insert-heading
                    (format "%s%s (%d)"
                            indent
                            (propertize "Plugins" 'face 'claude-gravity-section-heading)
                            (length plugins)))
                  (dolist (plugin plugins)
                    (claude-gravity--insert-plugin-capabilities plugin)))))
            ;; Standalone sections
            (when standalone-skills
              (claude-gravity--insert-capability-category
               "Standalone Skills"
               standalone-skills))
            (when standalone-agents
              (claude-gravity--insert-capability-category
               "Standalone Agents"
               standalone-agents))
            (when standalone-commands
              (claude-gravity--insert-capability-category
               "Standalone Commands"
               standalone-commands))
            (when standalone-mcp
              (claude-gravity--insert-capability-category
               "MCP Servers"
               standalone-mcp))))))))


(defun claude-gravity--insert-project-capabilities (project-dir)
  "Deprecated: use claude-gravity--insert-hierarchical-capabilities instead.
Maintained for backward compatibility."
  (claude-gravity--insert-hierarchical-capabilities project-dir))


(defun claude-gravity--render-overview ()
  "Render the overview buffer with all sessions grouped by project."
  (let ((buf (get-buffer claude-gravity-buffer-name)))
    (when buf
      (with-current-buffer buf
        (let ((inhibit-read-only t)
              (section-ident (when (magit-current-section)
                               (magit-section-ident (magit-current-section))))
              (pos-in-section (when (magit-current-section)
                                (- (point) (oref (magit-current-section) start))))
              (projects (make-hash-table :test 'equal)))
          ;; Group sessions by project
          (maphash (lambda (_id session)
                     (claude-gravity--migrate-session session)
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
            ;; Inbox: summary strip at top
            (claude-gravity--insert-inbox-summary)
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
                            (n-tools (claude-gravity--tree-total-tool-count session))
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
                                             'face 'claude-gravity-status-idle))))
                            (perm-mode (plist-get session :permission-mode))
                            (mode-badge
                             (if perm-mode
                                 (propertize (format " [%s]" perm-mode)
                                             'face 'claude-gravity-detail-label)
                               "")))
                        (let* ((tmux-name (gethash sid claude-gravity--tmux-sessions))
                               (tmux-badge (if tmux-name
                                               (propertize (format " [%s]" tmux-name)
                                                           'face 'claude-gravity-detail-label)
                                             ""))
                               (branch-str (or (claude-gravity--branch-or-cwd session) ""))
                               (source-str (or (claude-gravity--source-indicator session) ""))
                               (uuid-prefix (propertize
                                            (format " %s" (substring sid 0 (min 7 (length sid))))
                                            'face 'claude-gravity-detail-label))
                               (inbox-badge (claude-gravity--inbox-badges sid)))
                          (magit-insert-section (session-entry sid)
                            (magit-insert-heading
                              (format "%s%s %s %s %s%s %s  %s%s  [%d tools]%s"
                                      (claude-gravity--indent)
                                      indicator branch-str source-str label
                                      uuid-prefix tmux-badge
                                      (or status-label "")
                                      mode-badge
                                      n-tools inbox-badge))
                           ;; Inline inbox items for this session
                           (let ((session-items
                                  (cl-remove-if-not
                                   (lambda (item)
                                     (equal (alist-get 'session-id item) sid))
                                   claude-gravity--inbox)))
                             (dolist (item session-items)
                               (claude-gravity--insert-inbox-item item)))))))
                   ;; Project capabilities (skills, agents, commands) — after sessions
                   (let ((proj-cwd (plist-get (car sessions) :cwd)))
                     (when proj-cwd
                       (claude-gravity--insert-project-capabilities proj-cwd)))))
               projects)))
          ;; Restore semantic position
          (if-let* ((ident section-ident)
                    (target (magit-get-section ident)))
              (goto-char (max (oref target start)
                              (min (+ (oref target start) pos-in-section)
                                   (oref target end))))
            (goto-char (point-min)))
          (claude-gravity--apply-visibility))))))


(defun claude-gravity--insert-inbox-item (item)
  "Render a single inbox ITEM as a magit-section line."
  (let* ((id (alist-get 'id item))
         (type (alist-get 'type item))
         (project (or (alist-get 'project item) "?"))
         (summary (truncate-string-to-width
                   (replace-regexp-in-string "[\n\r\t]+" " "
                     (or (alist-get 'summary item) ""))
                   60))
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
        (format "      %s %-60s %s"
                (propertize icon 'face icon-face)
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
              (claude-gravity--log 'debug "Inbox item not found")
            (if (eq (alist-get 'type item) 'idle)
                (progn
                  (claude-gravity--inbox-remove item-id)
                  (claude-gravity--log 'debug "Dismissed"))
              (claude-gravity--log 'debug "Cannot dismiss — this item needs a response (use RET to act on it)"))))
      (claude-gravity--log 'debug "No inbox item at point"))))


(defun claude-gravity--inbox-section-p (section)
  "Return non-nil if SECTION is an inbox-item."
  (and section (eq (oref section type) 'inbox-item)))


(defun claude-gravity--ensure-inbox-visible ()
  "Expand any collapsed inbox sections so navigation can reach items."
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (let ((section (magit-current-section)))
        (when (and section
                   (memq (oref section type) '(session-inbox inbox-summary))
                   (oref section hidden))
          (magit-section-show section)))
      (condition-case nil
          (magit-section-forward)
        (error (goto-char (point-max)))))))


(defun claude-gravity-inbox-next ()
  "Jump to the next inbox item in the buffer."
  (interactive)
  (claude-gravity--ensure-inbox-visible)
  (let ((start (point))
        (found nil))
    ;; Move forward through sections until we find an inbox-item
    (while (and (not found)
                (condition-case nil
                    (progn (magit-section-forward) t)
                  (error nil)))
      (when (claude-gravity--inbox-section-p (magit-current-section))
        (setq found t)))
    (unless found
      (goto-char start)
      (claude-gravity--log 'debug "No more inbox items"))))


(defun claude-gravity-inbox-prev ()
  "Jump to the previous inbox item in the buffer."
  (interactive)
  (claude-gravity--ensure-inbox-visible)
  (let ((start (point))
        (found nil))
    (while (and (not found)
                (condition-case nil
                    (progn (magit-section-backward) t)
                  (error nil)))
      (when (claude-gravity--inbox-section-p (magit-current-section))
        (setq found t)))
    (unless found
      (goto-char start)
      (claude-gravity--log 'debug "No more inbox items"))))


(defun claude-gravity-inbox-list ()
  "Jump to the first inbox item in the overview buffer."
  (interactive)
  (claude-gravity--ensure-inbox-visible)
  (goto-char (point-min))
  (if (claude-gravity--inbox-section-p (magit-current-section))
      t  ; already on one
    (claude-gravity-inbox-next)))


;;; Per-Session Buffer

(defun claude-gravity--session-buffer-name (session)
  "Return buffer name for SESSION."
  (format "*Claude: %s*" (claude-gravity--session-label session)))


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


(defun claude-gravity-switch-session ()
  "Switch to a session buffer via completing-read.
Pre-selects: session with oldest pending notification, else longest-idle,
else current session."
  (interactive)
  (let ((candidates nil)
        (id-map nil)
        (default-label nil)
        (self-id claude-gravity--buffer-session-id))
    ;; Build candidates
    (maphash
     (lambda (_id session)
       (when (eq (plist-get session :status) 'active)
         (let* ((sid (plist-get session :session-id))
                (label (claude-gravity--session-label session))
                (claude-st (plist-get session :claude-status))
                (n-tools (claude-gravity--tree-total-tool-count session))
                (badges (claude-gravity--inbox-badges sid))
                (last-ev (plist-get session :last-event-time))
                (idle-secs (and last-ev (float-time (time-subtract (current-time) last-ev))))
                (status-str (cond
                             ((eq claude-st 'stopping) "stopping...")
                             ((eq claude-st 'responding) "responding")
                             ((null idle-secs) "idle")
                             ((< idle-secs 60) "idle")
                             ((< idle-secs 3600) (format "idle %dm" (truncate (/ idle-secs 60))))
                             (t (format "idle %dh" (truncate (/ idle-secs 3600))))))
                (dot (if (memq claude-st '(responding stopping))
                         (propertize "●" 'face 'claude-gravity-status-responding)
                       (propertize "●" 'face 'claude-gravity-status-idle)))
                (status-face (if (memq claude-st '(responding stopping))
                                 'claude-gravity-status-responding
                               'claude-gravity-status-idle))
                (elapsed (claude-gravity--session-total-elapsed session))
                (usage (plist-get session :token-usage))
                (in-tokens (when usage
                             (+ (or (alist-get 'input_tokens usage) 0)
                                (or (alist-get 'cache_read_input_tokens usage) 0)
                                (or (alist-get 'cache_creation_input_tokens usage) 0))))
                (out-tokens (when usage (or (alist-get 'output_tokens usage) 0)))
                (cost (plist-get session :cost))
                (ctx-pct (plist-get session :context-pct))
                (model-name (plist-get session :model-name))
                (branch-str (claude-gravity--branch-or-cwd session))
                (source-str (claude-gravity--source-indicator session))
                ;; Build entry parts
                (parts (delq nil
                             (list dot " "
                                   (propertize status-str 'face status-face) "  "
                                   (when branch-str (concat branch-str " "))
                                   (when source-str (concat source-str " "))
                                   (propertize (format "%-20s" label) 'face 'claude-gravity-slug)
                                   (propertize (format "  ◆ %d tools" n-tools)
                                               'face 'claude-gravity-detail-label))))
                (tail nil))
           (when (and model-name (not (string-empty-p model-name)))
             (let ((effort (plist-get session :effort-level)))
               (push (propertize
                      (if (and effort (not (equal effort "high")))
                          (format "  %s [%s]" model-name effort)
                        (format "  %s" model-name))
                      'face 'claude-gravity-detail-label) tail)))
           (when elapsed
             (push (propertize (format "  ⏱ %s" (claude-gravity--format-elapsed elapsed))
                               'face 'claude-gravity-detail-label) tail))
           (when (and in-tokens (> in-tokens 0))
             (push (propertize (format "  ↓%s ↑%s"
                                       (claude-gravity--format-token-count in-tokens)
                                       (claude-gravity--format-token-count out-tokens))
                               'face 'claude-gravity-detail-label) tail))
           (when cost
             (push (propertize (format "  $%.2f" cost)
                               'face 'claude-gravity-detail-label) tail))
           (when ctx-pct
             (let ((face (cond ((>= ctx-pct 90) 'error)
                               ((>= ctx-pct 70) 'warning)
                               (t 'claude-gravity-detail-label))))
               (push (propertize (format "  ctx:%d%%" ctx-pct) 'face face) tail)))
           (when (and badges (not (string-empty-p badges)))
             (push badges tail))
           (let ((entry (apply #'concat (append parts (nreverse tail)))))
             (push entry candidates)
             (push (cons entry sid) id-map)))))
     claude-gravity--sessions)
    (unless candidates
      (user-error "No active sessions"))
    ;; Determine default: 1) oldest notification, 2) longest idle, 3) self
    (let ((notif-oldest-sid nil)
          (notif-oldest-time nil))
      ;; Find session with oldest non-idle inbox item
      (dolist (item claude-gravity--inbox)
        (unless (eq (alist-get 'type item) 'idle)
          (let ((ts (alist-get 'timestamp item))
                (sid (alist-get 'session-id item)))
            (when (and ts (or (null notif-oldest-time)
                              (time-less-p ts notif-oldest-time)))
              (setq notif-oldest-time ts
                    notif-oldest-sid sid)))))
      (cond
       ;; 1) Session with oldest notification
       (notif-oldest-sid
        (setq default-label
              (car (cl-find-if (lambda (pair) (equal (cdr pair) notif-oldest-sid))
                               id-map))))
       ;; 2) Longest idle active session
       (t
        (let ((best-sid nil) (best-time nil))
          (maphash
           (lambda (_id session)
             (when (and (eq (plist-get session :status) 'active)
                        (eq (plist-get session :claude-status) 'idle))
               (let ((last-ev (plist-get session :last-event-time)))
                 (when (and last-ev
                            (or (null best-time)
                                (time-less-p last-ev best-time)))
                   (setq best-time last-ev
                         best-sid (plist-get session :session-id))))))
           claude-gravity--sessions)
          (when best-sid
            (setq default-label
                  (car (cl-find-if (lambda (pair) (equal (cdr pair) best-sid))
                                   id-map))))))))
    ;; 3) Fallback: self
    (unless default-label
      (when self-id
        (setq default-label
              (car (cl-find-if (lambda (pair) (equal (cdr pair) self-id))
                               id-map)))))
    (let* ((choice (completing-read "Session: " candidates nil t nil nil default-label))
           (sid (cdr (assoc choice id-map))))
      (when sid
        (claude-gravity-open-session sid)
        ;; Auto-open inbox pop-up if session has pending actionable items
        (let ((first-item (cl-find-if
                           (lambda (item)
                             (and (equal (alist-get 'session-id item) sid)
                                  (memq (alist-get 'type item)
                                        '(permission question plan-review))))
                           claude-gravity--inbox)))
          (when first-item
            (run-at-time 0 nil
                         (lambda (it)
                           (pcase (alist-get 'type it)
                             ('permission  (claude-gravity--inbox-act-permission it))
                             ('question    (claude-gravity--inbox-act-question it))
                             ('plan-review (claude-gravity--inbox-act-plan-review it))))
                         first-item)))))))


(defun claude-gravity--apply-visibility ()
  "Apply visibility state and paint fringe indicators.
magit-section caches visibility but relies on show/hide to create
indicator overlays.  Since we render from timers (not magit-refresh-buffer),
we must trigger this manually."
  (when magit-root-section
    (let ((magit-section-cache-visibility nil))
      (magit-section-show magit-root-section))))


(defun claude-gravity--insert-session-inbox (session)
  "Insert actionable inbox items for SESSION into the session buffer.
Only shows permission, question, and plan-review items (not idle)."
  (let* ((sid (plist-get session :session-id))
         (items (cl-remove-if-not
                 (lambda (item)
                   (and (equal (alist-get 'session-id item) sid)
                        (memq (alist-get 'type item)
                              '(permission question plan-review))))
                 claude-gravity--inbox)))
    (when items
      (magit-insert-section (session-inbox nil t)
        (magit-insert-heading
          (propertize (format "Inbox (%d)" (length items))
                      'face 'claude-gravity-question))
        (dolist (item items)
          (claude-gravity--insert-inbox-item item))
        (insert "\n")))))


(defun claude-gravity--render-session-buffer (session)
  "Render the magit-section UI for SESSION into its buffer."
  (let* ((buf (or (let ((b (plist-get session :buffer)))
                    (and b (buffer-live-p b) b))
                  (get-buffer (claude-gravity--session-buffer-name session)))))
    (when buf
      (with-current-buffer buf
        (let ((inhibit-read-only t)
              (section-ident (when (magit-current-section)
                               (magit-section-ident (magit-current-section))))
              (pos-in-section (when (magit-current-section)
                                (- (point) (oref (magit-current-section) start)))))
          (erase-buffer)
          (magit-insert-section (root)
            (claude-gravity-insert-header session)
            (claude-gravity-insert-plan session)
            (claude-gravity-insert-streaming-text session)
            (claude-gravity-insert-turns session)
            (claude-gravity--insert-session-inbox session)
            (claude-gravity-insert-files session)
            (claude-gravity-insert-allow-patterns session))
          ;; Move ▎ indicators from inline text to left display margin
          (claude-gravity--margins-to-gutter)
          (dolist (win (get-buffer-window-list buf nil t))
            (set-window-margins win left-margin-width))
          ;; Restore semantic position
          (if-let* ((ident section-ident)
                    (target (magit-get-section ident)))
              (goto-char (max (oref target start)
                              (min (+ (oref target start) pos-in-section)
                                   (oref target end))))
            (goto-char (point-min)))
          (claude-gravity--apply-visibility)))
      )))


;;; Section Navigation

(defun claude-gravity--section-forward ()
  "Move to next section, silently do nothing at the last section."
  (interactive)
  (condition-case nil
      (magit-section-forward)
    (user-error nil)))

(defun claude-gravity--section-backward ()
  "Move to previous section, silently do nothing at the first section."
  (interactive)
  (condition-case nil
      (magit-section-backward)
    (user-error nil)))


;;; Modes

(defvar claude-gravity-mode-map (make-sparse-keymap)
  "Keymap for `claude-gravity-mode'.")
(set-keymap-parent claude-gravity-mode-map magit-section-mode-map)

(define-key claude-gravity-mode-map (kbd "n") 'claude-gravity--section-forward)
(define-key claude-gravity-mode-map (kbd "p") 'claude-gravity--section-backward)

(define-key claude-gravity-mode-map (kbd "g") 'claude-gravity-refresh)

(define-key claude-gravity-mode-map (kbd "c") 'claude-gravity-comment-at-point)

(define-key claude-gravity-mode-map (kbd "P") 'claude-gravity-show-plan)

(define-key claude-gravity-mode-map (kbd "?") 'claude-gravity-overview-menu)

(define-key claude-gravity-mode-map (kbd "TAB") 'magit-section-toggle)

(define-key claude-gravity-mode-map (kbd "<return>")
  (lambda ()
    "Visit or toggle section, but only when on a valid section."
    (interactive)
    (let ((section (magit-current-section)))
      (when section
        (claude-gravity-visit-or-toggle)))))

(define-key claude-gravity-mode-map (kbd "D") 'claude-gravity-cleanup-sessions)

(define-key claude-gravity-mode-map (kbd "R") 'claude-gravity-reset-status)

(define-key claude-gravity-mode-map (kbd "X") 'claude-gravity-detect-dead-sessions)

(define-key claude-gravity-mode-map (kbd "d") 'claude-gravity-delete-session)

(define-key claude-gravity-mode-map (kbd "A") 'claude-gravity-add-allow-pattern)

(define-key claude-gravity-mode-map (kbd "a") 'claude-gravity-add-allow-pattern-to-settings)

(define-key claude-gravity-mode-map (kbd "F") 'claude-gravity-open-plan-file)

(define-key claude-gravity-mode-map (kbd "t") 'claude-gravity-tail)

(define-key claude-gravity-mode-map (kbd "f") 'claude-gravity-follow-mode)

(define-key claude-gravity-mode-map (kbd "e") 'claude-gravity-edit-entry)

(define-key claude-gravity-mode-map (kbd "b") 'claude-gravity-switch-session)

(define-key claude-gravity-mode-map (kbd "k") 'claude-gravity-inbox-dismiss)


;; Inbox navigation prefix map
(defvar claude-gravity-inbox-map (make-sparse-keymap)
  "Keymap for inbox navigation commands under the `i' prefix.")

(define-key claude-gravity-inbox-map (kbd "n") 'claude-gravity-inbox-next)

(define-key claude-gravity-inbox-map (kbd "p") 'claude-gravity-inbox-prev)

(define-key claude-gravity-mode-map (kbd "i") claude-gravity-inbox-map)

(define-key claude-gravity-inbox-map (kbd "l") 'claude-gravity-inbox-list)


(define-derived-mode claude-gravity-mode magit-section-mode "Claude"
  "Major mode for Structured Claude Sessions overview.

\\{claude-gravity-mode-map}"
  (font-lock-mode -1)
  (visual-line-mode 1))


(define-key claude-gravity-mode-map (kbd "T") 'claude-gravity-view-agent-transcript)

(define-key claude-gravity-mode-map (kbd "V") 'claude-gravity-open-agent-transcript)


(defun claude-gravity--layout-header-segments (segments)
  "Split SEGMENTS across two lines based on window width.
Returns (LINE1 . LINE2-OR-NIL).  Line 1 is filled greedily;
overflow segments go to line 2.  If everything fits, LINE2 is nil."
  (let* ((width (max 40 (1- (window-width))))
         (line1 "")
         (line1-len 0)
         (overflow nil))
    (dolist (seg segments)
      (let ((seg-len (string-width seg)))
        (if (and (not overflow) (<= (+ line1-len seg-len) width))
            (setq line1 (concat line1 seg)
                  line1-len (+ line1-len seg-len))
          (push seg overflow))))
    (if overflow
        (cons line1 (concat " " (mapconcat #'identity (nreverse overflow) "")))
      (cons line1 nil))))

(defun claude-gravity--session-header-line ()
  "Return header-line string for the current session buffer.
Re-computed on every redisplay so it adapts to window width changes.
When segments overflow, dynamically enables `tab-line-format' as a
second sticky line above the header-line."
  (when-let* ((sid claude-gravity--buffer-session-id)
              (session (gethash sid claude-gravity--sessions)))
    (pcase-let ((`(,line1 . ,line2)
                 (claude-gravity--build-header-lines session sid)))
      ;; line2 non-nil means overflow: tab-line gets line1, header gets line2
      ;; line2 nil means fits: tab-line hidden, header gets line1
      ;; Escape literal % chars — mode-line format interprets them as constructs
      (let* ((esc (lambda (s) (replace-regexp-in-string "%" "%%" s)))
             (new-tab (when line2 (funcall esc line1))))
        (unless (equal tab-line-format new-tab)
          (setq-local tab-line-format new-tab)))
      (let ((result (or line2 line1)))
        (replace-regexp-in-string "%" "%%" result)))))

(defun claude-gravity--build-header-lines (session sid)
  "Build header-line segments for SESSION with SID.
Returns (LINE1 . LINE2-OR-NIL) via `claude-gravity--layout-header-segments'."
  (let* ((status (plist-get session :status))
         (claude-st (plist-get session :claude-status))
         (last-event (plist-get session :last-event-time))
         (idle-time (when (and last-event (eq claude-st 'idle))
                      (float-time (time-subtract (current-time) last-event))))
         (idle-str (when idle-time
                     (cond
                      ((< idle-time 60) "")
                      ((< idle-time 3600) (format " %dm" (truncate (/ idle-time 60))))
                      (t (format " %dh" (truncate (/ idle-time 3600)))))))
         (dot (cond
               ((eq status 'ended)
                (propertize "○" 'face 'claude-gravity-session-ended))
               ((eq claude-st 'stopping)
                (propertize "●" 'face 'claude-gravity-status-responding))
               ((eq claude-st 'responding)
                (propertize "●" 'face 'claude-gravity-status-responding))
               (t
                (propertize "●" 'face 'claude-gravity-status-idle))))
         (status-word (cond
                       ((eq status 'ended)
                        (propertize "ended" 'face 'claude-gravity-session-ended))
                       ((eq claude-st 'stopping)
                        (propertize "stopping..." 'face 'claude-gravity-status-responding))
                       ((eq claude-st 'responding)
                        (propertize "responding" 'face 'claude-gravity-status-responding))
                       (t
                        (propertize (concat "idle" (or idle-str ""))
                                   'face 'claude-gravity-status-idle))))
          (slug (propertize (claude-gravity--session-label session)
                            'face 'claude-gravity-slug))
          (branch-str (claude-gravity--branch-or-cwd session))
          (source-str (claude-gravity--source-indicator session))
          (tool-count (claude-gravity--tree-total-tool-count session))
         (elapsed (claude-gravity--session-total-elapsed session))
         (usage (plist-get session :token-usage))
         (in-tokens (when usage
                      (+ (or (alist-get 'input_tokens usage) 0)
                         (or (alist-get 'cache_read_input_tokens usage) 0)
                         (or (alist-get 'cache_creation_input_tokens usage) 0))))
         (out-tokens (when usage (or (alist-get 'output_tokens usage) 0)))
         (perm-mode (plist-get session :permission-mode))
         (model-name (plist-get session :model-name))
         (ctx-pct (plist-get session :context-pct))
         (lines-add (plist-get session :sl-lines-added))
         (lines-rm (plist-get session :sl-lines-removed))
         (inbox-badge (claude-gravity--inbox-badges sid))
         (segments nil))
    (push (concat " " dot " " status-word) segments)
    (push (concat "  " slug) segments)
    (push (propertize (format " %s" (substring sid 0 (min 7 (length sid))))
                      'face 'claude-gravity-detail-label)
          segments)
    (when branch-str
      (push (concat "  " branch-str) segments))
    (when source-str
      (push (concat "  " source-str) segments))
    (when perm-mode
      (push (propertize (format "  [%s]" perm-mode)
                        'face 'claude-gravity-detail-label) segments))
    (when model-name
      (let ((effort (plist-get session :effort-level)))
        (push (propertize
               (if (and effort (not (equal effort "high")))
                   (format "  %s [%s]" model-name effort)
                 (format "  %s" model-name))
               'face 'claude-gravity-detail-label) segments)))
    (push (propertize (format "  ◆ %d tools" tool-count)
                      'face 'claude-gravity-detail-label) segments)
    (when elapsed
      (push (propertize (format "  ⏱ %s" (claude-gravity--format-elapsed elapsed))
                        'face 'claude-gravity-detail-label) segments))
    (when (and in-tokens (> in-tokens 0))
      (push (propertize (format "  ↓%s ↑%s tokens"
                                (claude-gravity--format-token-count in-tokens)
                                (claude-gravity--format-token-count out-tokens))
                        'face 'claude-gravity-detail-label) segments))
    (when ctx-pct
      (let ((face (cond ((>= ctx-pct 90) 'error)
                        ((>= ctx-pct 70) 'warning)
                        (t 'claude-gravity-detail-label))))
        (push (propertize (format "  ctx:%d%%" ctx-pct) 'face face) segments)))
    (when (and lines-add lines-rm (or (> lines-add 0) (> lines-rm 0)))
      (push (propertize (format "  +%d -%d" lines-add lines-rm)
                        'face 'claude-gravity-detail-label) segments))
    (when (and inbox-badge (not (string-empty-p inbox-badge)))
      (push inbox-badge segments))
    (setq segments (nreverse segments))
    (claude-gravity--layout-header-segments segments)))


(defvar claude-gravity-session-mode-map (make-sparse-keymap)
  "Keymap for `claude-gravity-session-mode' (session buffer specific).")
(set-keymap-parent claude-gravity-session-mode-map claude-gravity-mode-map)

;; Session buffer specific bindings (override parent)
(define-key claude-gravity-session-mode-map (kbd "o") 'claude-gravity-return-to-overview)
(define-key claude-gravity-session-mode-map (kbd "l") 'claude-gravity-set-permission-mode)
(define-key claude-gravity-session-mode-map (kbd "?") 'claude-gravity-session-menu)
(define-key claude-gravity-session-mode-map (kbd "SPC") 'claude-gravity-popup-at-point)


(define-derived-mode claude-gravity-session-mode claude-gravity-mode "Claude"
  "Major mode for a single Structured Claude Session buffer."
  (setq mode-name '(:eval (if claude-gravity--follow-mode
                              "Claude[F]"
                            "Claude")))
  (setq header-line-format '(:eval (claude-gravity--session-header-line)))
  ;; tab-line-format is set dynamically by --session-header-line when overflow.
  ;; Make tab-line face match header-line so the two lines look consistent.
  (face-remap-add-relative 'tab-line 'header-line)
  ;; Enable left display margin for gutter indicators (▎)
  (setq left-margin-width 1)
  (add-hook 'window-selection-change-functions
            #'claude-gravity--session-on-focus nil t))

(defun claude-gravity--session-on-focus (_frame)
  "Refresh session buffer when it gains focus.
Also syncs tmux window width to match the new window size."
  (when (and (derived-mode-p 'claude-gravity-session-mode)
             claude-gravity--buffer-session-id)
    (claude-gravity--tmux-sync-width-for-buffer (current-buffer))
    (let ((session (claude-gravity--get-session claude-gravity--buffer-session-id)))
      (when session
        (claude-gravity--render-session-buffer session)))))


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


(defun claude-gravity-edit-entry ()
  "Open definition file for skill/agent/command at point."
  (interactive)
  (let ((section (magit-current-section)))
    (when section
      (if (eq (oref section type) 'capability-entry)
          (let* ((cap (oref section value))
                 (file-path (alist-get 'file-path cap)))
            (if (and file-path (file-exists-p file-path))
                (find-file file-path)
              (message "No file path for this entry")))
        (message "No editable entry at point")))))


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
        (claude-gravity--log 'debug "No tool at point")
      (let* ((name (alist-get 'name item))
             (input (alist-get 'input item))
             (suggestions (claude-gravity--suggest-patterns name input)))
        (if (not suggestions)
            (claude-gravity--log 'debug "No pattern suggestions for %s" name)
          (let ((chosen (completing-read "Allow pattern: " suggestions nil nil
                                         (car suggestions))))
            (kill-new chosen)
            (claude-gravity--log 'debug "Copied: %s" chosen)))))))


(defun claude-gravity-add-allow-pattern-to-settings ()
  "Add an allow pattern for the tool at point to settings.local.json."
  (interactive)
  (let ((item (claude-gravity--tool-item-at-point)))
    (if (not item)
        (claude-gravity--log 'debug "No tool at point")
      (let* ((sid (or claude-gravity--buffer-session-id ""))
             (session (claude-gravity--get-session sid)))
        (if (not session)
            (claude-gravity--log 'debug "No session found")
          (let* ((name (alist-get 'name item))
                 (input (alist-get 'input item))
                 (suggestions (claude-gravity--suggest-patterns name input)))
            (if (not suggestions)
                (claude-gravity--log 'debug "No pattern suggestions for %s" name)
              (let* ((chosen (completing-read "Allow pattern to add: " suggestions nil nil
                                              (car suggestions)))
                     (cwd (plist-get session :cwd))
                     (settings-path (expand-file-name ".claude/settings.local.json" cwd)))
                (when (y-or-n-p (format "Add \"%s\" to %s? " chosen
                                        (file-name-nondirectory settings-path)))
                  (let* ((data (if (file-exists-p settings-path)
                                   (claude-gravity--json-read-file settings-path)
                                 (list (cons 'permissions (list (cons 'allow nil))))))
                         (perms (or (alist-get 'permissions data)
                                    (list (cons 'allow nil))))
                         (allow (or (alist-get 'allow perms) nil)))
                    (if (member chosen allow)
                        (claude-gravity--log 'debug "Pattern already exists: %s" chosen)
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
                      (claude-gravity--log 'debug "Added: %s" chosen))))))))))))


;;; Session start helpers

(defun claude-gravity--infer-cwd-from-section ()
  "Return a project CWD inferred from the current magit section, or nil.
When point is on a session-entry section, returns that session's :cwd.
When point is on a project section, returns the :cwd of the first matching session."
  (let* ((section (magit-current-section))
         (type (and section (oref section type)))
         (value (and section (oref section value))))
    (cond
     ;; On a session-entry: value is the session-id string
     ((eq type 'session-entry)
      (let ((session (claude-gravity--get-session value)))
        (and session (plist-get session :cwd))))
     ;; On a project section: value is the project name; find first session's cwd
     ((eq type 'project)
      (let ((cwd nil))
        (maphash (lambda (_id session)
                   (when (and (not cwd)
                              (equal (plist-get session :project) value))
                     (setq cwd (plist-get session :cwd))))
                 claude-gravity--sessions)
        cwd)))))


(defun claude-gravity-start-session-here ()
  "Start a Claude session rooted at the current buffer's project.
Detects project root via `project-current' or `vc-root-dir', then
prompts to confirm the directory before starting."
  (interactive)
  (let* ((proj-root (or (and (fboundp 'project-root)
                             (when-let ((proj (project-current)))
                               (project-root proj)))
                        (vc-root-dir)
                        default-directory))
         (dir (claude-gravity--read-project-dir
               "Project directory: "
               (expand-file-name proj-root))))
    (claude-gravity-start-session dir)))


;;; Commands

;;;###autoload (autoload 'claude-gravity-overview-menu "claude-gravity" nil t)
(transient-define-prefix claude-gravity-overview-menu ()
  "Overview buffer menu: manage sessions and inbox."
  [["Actions"
    ("g" "Refresh" claude-gravity-refresh)
    ("b" "Switch session" claude-gravity-switch-session)
    ("RET" "Visit or toggle" claude-gravity-visit-or-toggle)]
   ["Session Management"
    ("N" "Start (Cloud)" claude-gravity-daemon-start-session)
    ("S" "Start (tmux)" claude-gravity-start-menu)
    ("H" "Start here" claude-gravity-start-session-here)
    ("r" "Resume session" claude-gravity-unified-resume)
    ("w" "Resume (picker)" claude-gravity-resume-in-tmux)
    ("D" "Remove ended" claude-gravity-cleanup-sessions)
    ("X" "Detect dead" claude-gravity-detect-dead-sessions)
    ("R" "Reset all idle" claude-gravity-reset-status)
    ("d" "Delete session" claude-gravity-delete-session)
    ("," "Rename session" claude-gravity-rename-session)]
   ["Navigation"
    ("TAB" "Toggle section" magit-section-toggle)
    ("e" "Edit entry" claude-gravity-edit-entry)
    ("k" "Dismiss inbox" claude-gravity-inbox-dismiss)]
   ["Debug"
    ("M" "Debug messages" claude-gravity-debug-show)]])


;;;###autoload (autoload 'claude-gravity-session-menu "claude-gravity" nil t)
(transient-define-prefix claude-gravity-session-menu ()
  "Session buffer menu: interact with current session."
  [["View & Navigate"
    ("g" "Refresh" claude-gravity-refresh)
    ("t" "Tail" claude-gravity-tail)
    ("SPC" "Detail popup" claude-gravity-popup-at-point)
    ("o" "Return to overview" claude-gravity-return-to-overview)
    ("f" "Follow mode" claude-gravity-follow-mode)
    ("TAB" "Toggle section" magit-section-toggle)]
   ["Current Session"
    ("s" "Compose prompt" claude-gravity-unified-compose
     :inapt-if-not claude-gravity--current-session-managed-p)
    ("K" "Stop session" claude-gravity-unified-stop
     :inapt-if-not claude-gravity--current-session-managed-p)
    ("E" "Interrupt" claude-gravity-unified-interrupt
     :inapt-if-not claude-gravity--current-session-managed-p)
    ("m" "Set model" claude-gravity-set-model
     :inapt-if-not claude-gravity--current-session-managed-p)
    ("l" "Set permission mode" claude-gravity-set-permission-mode
     :inapt-if-not claude-gravity--current-session-managed-p)
    ("," "Rename session" claude-gravity-rename-session)]
   ["Plan & Transcript"
    ("P" "Show Plan" claude-gravity-show-plan)
    ("F" "Open plan file" claude-gravity-open-plan-file)
    ("c" "Comment" claude-gravity-comment-at-point)
    ("T" "Parse transcript" claude-gravity-view-agent-transcript)
    ("V" "Open transcript" claude-gravity-open-agent-transcript)]
   ["Tmux Session"
    ("/" "Slash command" claude-gravity-slash-command
     :inapt-if-not claude-gravity--current-session-tmux-p)
    ("$" "Terminal" claude-gravity-terminal-session
     :inapt-if-not claude-gravity--current-session-tmux-p)
    ("C" "Reset/clear" claude-gravity-reset-session
     :inapt-if-not claude-gravity--current-session-tmux-p)]
   ["Permissions"
    ("A" "Copy allow pattern" claude-gravity-add-allow-pattern)
    ("a" "Add to settings" claude-gravity-add-allow-pattern-to-settings)]
   ["Debug"
    ("M" "Debug messages" claude-gravity-debug-show)]])


;; Compatibility alias for existing code
(defalias 'claude-gravity-menu 'claude-gravity-overview-menu)


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
    (claude-gravity--log 'debug "Removed %d ended session(s)" (length to-remove))
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
    (claude-gravity--log 'debug "Reset %d session(s) to idle" count)
    (claude-gravity--render-overview)))


(defun claude-gravity--process-alive-p (pid)
  "Return non-nil if process PID is alive.
Uses `ps' on macOS where `signal-process' returns t for zombie PIDs."
  (if (eq system-type 'darwin)
      (string-match-p (format "^ *%d " pid)
                      (shell-command-to-string
                       (format "ps -p %d -o pid,state= 2>/dev/null" pid)))
    (condition-case nil
        (progn (signal-process pid 0) t)
      (error nil))))


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
    (claude-gravity--log 'debug "Marked %d dead session(s) as ended" count)
    (claude-gravity--render-overview)))


(defun claude-gravity-delete-session ()
  "Delete the session at point from the registry."
  (interactive)
  (let ((section (magit-current-section)))
    (when section
      (let ((sid (and (eq (oref section type) 'session-entry)
                      (oref section value))))
        (if (not sid)
            (claude-gravity--log 'debug "No session at point")
          (let ((session (gethash sid claude-gravity--sessions)))
            (when session
              (let ((buf (get-buffer (claude-gravity--session-buffer-name session))))
                (when buf (kill-buffer buf)))))
          (remhash sid claude-gravity--sessions)
          (claude-gravity--log 'debug "Deleted session %s" (claude-gravity--session-short-id sid))
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
        (claude-gravity--log 'debug "Follow mode ON"))
    (remove-hook 'post-command-hook #'claude-gravity--follow-detect-manual t)
    (claude-gravity--log 'debug "Follow mode OFF"))
  (force-mode-line-update))


(defun claude-gravity--follow-detect-manual ()
  "Disable follow mode when user scrolls or navigates manually."
  (when (and claude-gravity--follow-mode
             (memq this-command '(scroll-up-command scroll-down-command
                                  scroll-up scroll-down
                                  beginning-of-buffer end-of-buffer
                                  previous-line next-line
                                  magit-section-forward magit-section-backward
                                  claude-gravity--section-forward claude-gravity--section-backward
                                  magit-section-toggle)))
    (setq claude-gravity--follow-mode nil)
    (remove-hook 'post-command-hook #'claude-gravity--follow-detect-manual t)
    (force-mode-line-update)
    (claude-gravity--log 'debug "Follow mode OFF (manual navigation)")))


(defun claude-gravity-tail ()
  "Collapse all sections and focus on the tail of the latest turn.
Hides Plan, Files, Allow Patterns, and past turns.  When a
stop-message follows the latest turn, shows only the stop-message
summary.  Otherwise expands the last turn and its last cycle."
  (interactive)
  (when magit-root-section
    ;; Single pass: collapse all top-level sections, find turns section
    (let ((turns-section nil))
      (dolist (child (oref magit-root-section children))
        (magit-section-hide child)
        (when (eq (oref child type) 'turns)
          (setq turns-section child)))
      (when turns-section
        (magit-section-show turns-section)
        ;; Single pass: hide all turns children, track last turn and stop-message
        (let ((last-turn nil)
              (last-stop nil))
          (dolist (child (oref turns-section children))
            (pcase (oref child type)
              ('turn (magit-section-hide child) (setq last-turn child))
              ('stop-message (magit-section-hide child) (setq last-stop child))))
          (if (and last-stop last-turn
                   ;; stop-message follows the last turn in buffer order
                   (> (oref last-stop start) (oref last-turn start)))
              ;; Stop message present: keep turn collapsed, show stop-message
              (progn
                (magit-section-show last-stop)
                (let ((win (get-buffer-window (current-buffer))))
                  (when win
                    (with-selected-window win
                      (goto-char (oref last-stop start))
                      (recenter -3)))))
            ;; No stop message: expand last turn and its last cycle
            (when last-turn
              (magit-section-show last-turn)
              (let ((last-cycle nil))
                (dolist (child (oref last-turn children))
                  (when (eq (oref child type) 'response-cycle)
                    (magit-section-hide child)
                    (setq last-cycle child)))
                (when last-cycle
                  (magit-section-show last-cycle)))
              (let ((win (get-buffer-window (current-buffer))))
                (when win
                  (with-selected-window win
                    (goto-char (1- (oref last-turn end)))
                    (recenter -3)))))))))))


(defun claude-gravity-return-to-overview (&optional bury)
  "Return to the session overview buffer from a session detail buffer.
With prefix arg BURY (\\[universal-argument]), bury the current session
buffer instead of switching to overview.

If the overview buffer exists and is visible in another window,
switch to that window. Otherwise, display the overview buffer
in the current window."
  (interactive "P")
  (if bury
      ;; With prefix arg: bury the current session buffer
      (bury-buffer)
    ;; Without prefix: navigate to overview
    (let* ((overview-buf (get-buffer claude-gravity-buffer-name))
           (overview-win (when overview-buf (get-buffer-window overview-buf))))
      (cond
       ;; Overview already visible in another window - switch to it
       (overview-win
        (select-window overview-win))
       ;; Overview buffer exists but not visible - display it
       (overview-buf
        (switch-to-buffer overview-buf))
       ;; Overview doesn't exist - create it
       (t
        (claude-gravity-status))))))


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


;; Keybindings for session commands
;; Unified keys dispatch to the correct backend (daemon or tmux)
(define-key claude-gravity-mode-map (kbd "N") 'claude-gravity-daemon-start-session)  ; Cloud
(define-key claude-gravity-mode-map (kbd "S") 'claude-gravity-start-menu)              ; tmux
(define-key claude-gravity-mode-map (kbd "H") 'claude-gravity-start-session-here)     ; start here
(define-key claude-gravity-mode-map (kbd "s") 'claude-gravity-unified-compose)
(define-key claude-gravity-mode-map (kbd "r") 'claude-gravity-unified-resume)
(define-key claude-gravity-mode-map (kbd "w") 'claude-gravity-resume-in-tmux)
(define-key claude-gravity-mode-map (kbd "K") 'claude-gravity-unified-stop)
(define-key claude-gravity-mode-map (kbd "E") 'claude-gravity-unified-interrupt)
(define-key claude-gravity-mode-map (kbd "m") 'claude-gravity-set-model)
;; Tmux-only
(define-key claude-gravity-mode-map (kbd "/") 'claude-gravity-slash-command)
(define-key claude-gravity-mode-map (kbd "C") 'claude-gravity-reset-session)
(define-key claude-gravity-mode-map (kbd "$") 'claude-gravity-terminal-session)
(define-key claude-gravity-mode-map (kbd "<backtab>") 'claude-gravity-toggle-permission-mode)
(define-key claude-gravity-mode-map (kbd ",") 'claude-gravity-rename-session)
;; Other
(define-key claude-gravity-mode-map (kbd "M") 'claude-gravity-debug-show)

;; Global keybinding: start a session from any buffer
;; Users can rebind by setting this before loading claude-gravity-ui
(defcustom claude-gravity-global-start-key "C-c G"
  "Global keybinding for `claude-gravity-start-session-here'.
Set to nil to disable the global binding.
Must be set before loading claude-gravity-ui to take effect,
or call `(global-set-key (kbd NEW-KEY) #\\='claude-gravity-start-session-here)'."
  :type '(choice string (const nil))
  :group 'claude-gravity)

(when claude-gravity-global-start-key
  (global-set-key (kbd claude-gravity-global-start-key)
                  #'claude-gravity-start-session-here))

;;; Popup Detail View

(defvar-local claude-gravity--popup-wconf nil
  "Saved window configuration before popup was shown.")

(defvar claude-gravity-popup-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'claude-gravity-popup-dismiss)
    (define-key map (kbd "SPC") #'claude-gravity-popup-dismiss)
    map)
  "Keymap for `claude-gravity-popup-mode'.")

(define-derived-mode claude-gravity-popup-mode markdown-view-mode "Claude Detail"
  "Major mode for Claude detail popup buffers.
Uses markdown-mode for rendering, with dismiss keybindings."
  (use-local-map claude-gravity-popup-mode-map))


(defun claude-gravity-popup-dismiss ()
  "Dismiss the popup and restore the previous window layout."
  (interactive)
  (let ((wconf claude-gravity--popup-wconf))
    (kill-buffer (current-buffer))
    (when wconf
      (set-window-configuration wconf))))


(defun claude-gravity-popup-at-point ()
  "Show full detail for the section at point in a maximized popup.
Press `q' or `SPC' to dismiss and restore the previous layout."
  (interactive)
  (let* ((section (magit-current-section))
         (type (and section (oref section type)))
         (value (and section (oref section value)))
         (session (when claude-gravity--buffer-session-id
                    (claude-gravity--get-session claude-gravity--buffer-session-id)))
         (wconf (current-window-configuration))
         (content (claude-gravity--popup-content-for type value session)))
    (if (not content)
        (user-error "No detail available for this section")
      (let ((buf (get-buffer-create "*Claude Detail*")))
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert content))
          (claude-gravity-popup-mode)
          (claude-gravity--popup-fontify-diffs)
          (setq claude-gravity--popup-wconf wconf)
          (goto-char (point-min)))
        (pop-to-buffer buf)
        (delete-other-windows)))))


(defun claude-gravity--popup-content-for (type value session)
  "Generate popup content string for section TYPE with VALUE in SESSION.
Returns nil if no meaningful content is available."
  (pcase type
    ('tool (claude-gravity--popup-tool-content value session))
    ('response-cycle (claude-gravity--popup-cycle-content value session))
    ('stop-message (claude-gravity--popup-stop-content value session))
    ('turn (claude-gravity--popup-turn-content value session))
    ('task (claude-gravity--popup-task-content value session))
    ('agent (claude-gravity--popup-agent-content value session))
    (_ nil)))


(defun claude-gravity--popup-tool-content (tool-use-id session)
  "Generate full tool detail for TOOL-USE-ID in SESSION."
  (when (and session tool-use-id)
    (let ((tool (claude-gravity-model-find-tool session tool-use-id)))
      (when tool
        (let* ((name (alist-get 'name tool))
               (input (alist-get 'input tool))
               (result (alist-get 'result tool))
               (status (alist-get 'status tool))
               (post-think (alist-get 'post_thinking tool))
               (post-text (alist-get 'post_text tool)))
          (with-temp-buffer
            (insert (format "# Tool: %s\n\n" (or name "?")))
            (insert (format "`%s`\n\n" (claude-gravity--tool-signature name input)))
            (insert (format "**Status:** %s\n\n" (or status "unknown")))
            (claude-gravity--popup-insert-tool-input name input result status)
            (claude-gravity--popup-insert-tool-result name input result status)
            (when (and post-think (not (string-empty-p post-think)))
              (insert "## Thinking\n\n" (claude-gravity--render-tables-in-text post-think) "\n\n"))
            (when (and post-text (not (string-empty-p post-text)))
              (insert "## Assistant\n\n" (claude-gravity--render-tables-in-text post-text) "\n\n"))
            ;; Agent content (for Task tools with linked agent)
            (let ((agent (alist-get 'agent tool)))
              (when agent
                (let ((stop-think (alist-get 'stop_thinking agent))
                      (stop-text (alist-get 'stop_text agent))
                      (cycles (claude-gravity--tlist-items (alist-get 'cycles agent))))
                  (when (and stop-think (stringp stop-think) (not (string-empty-p stop-think)))
                    (insert "## Agent Thinking\n\n" (claude-gravity--render-tables-in-text stop-think) "\n\n"))
                  (when (and stop-text (stringp stop-text) (not (string-empty-p stop-text)))
                    (insert "## Agent Summary\n\n" (claude-gravity--render-tables-in-text stop-text) "\n\n"))
                  (when cycles
                    (insert "## Agent Tool History\n\n")
                    (dolist (cycle cycles)
                      (let ((tools (claude-gravity--tlist-items (alist-get 'tools cycle)))
                            (athink (alist-get 'thinking cycle))
                            (atext (alist-get 'text cycle)))
                        (when (and athink (not (string-empty-p (string-trim athink))))
                          (insert "### Thinking\n\n" (string-trim athink) "\n\n"))
                        (when (and atext (not (string-empty-p (string-trim atext))))
                          (insert (string-trim atext) "\n\n"))
                        (when tools
                          (dolist (atool tools)
                            (insert (format "%s `%s`\n"
                                            (claude-gravity--popup-tool-status-mark
                                             (alist-get 'status atool))
                                            (claude-gravity--tool-signature
                                             (alist-get 'name atool)
                                             (alist-get 'input atool)))))
                          (insert "\n"))))))))
            (buffer-string)))))))


(defun claude-gravity--popup-overlay-face (start end face)
  "Create an overlay from START to END with FACE, prioritized over font-lock."
  (let ((ov (make-overlay start end)))
    (overlay-put ov 'face face)
    (overlay-put ov 'evaporate t)))

(defun claude-gravity--popup-fontify-diffs ()
  "Apply diff overlays in the current popup buffer after mode setup.
Scans for lines between ### Diff headings and next heading or end,
applying faces based on line prefix."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^### Diff$" nil t)
      (forward-line 1)
      ;; skip blank line after heading
      (when (looking-at-p "^$") (forward-line 1))
      (let ((end-bound (save-excursion
                         (if (re-search-forward "^#" nil t)
                             (line-beginning-position)
                           (point-max)))))
        (while (< (point) end-bound)
          (let ((bol (line-beginning-position))
                (eol (line-end-position))
                (line-text (buffer-substring-no-properties
                            (line-beginning-position)
                            (min (+ (line-beginning-position) 2)
                                 (line-end-position)))))
            (cond
             ((string-prefix-p "@@" line-text)
              (claude-gravity--popup-overlay-face bol eol 'claude-gravity-diff-header))
             ((string-prefix-p "-" line-text)
              (claude-gravity--popup-overlay-face bol eol 'claude-gravity-diff-removed))
             ((string-prefix-p "+" line-text)
              (claude-gravity--popup-overlay-face bol eol 'claude-gravity-diff-added))
             ((string-prefix-p " " line-text)
              (claude-gravity--popup-overlay-face bol eol 'claude-gravity-diff-context))))
          (forward-line 1))))))

(defun claude-gravity--popup-insert-edit-diff (input result status)
  "Insert plain-text unified diff for an Edit tool into the popup buffer.
Uses structuredPatch from RESULT when available (STATUS \"done\"),
otherwise falls back to old_string/new_string from INPUT.
Diff coloring is applied later by `claude-gravity--popup-fontify-diffs'."
  (let (old-str new-str patch)
    (cond
     ((equal status "done")
      (setq patch (and (listp result) (alist-get 'structuredPatch result)))
      (unless patch
        (setq old-str (or (and (listp result) (alist-get 'oldString result))
                          (alist-get 'old_string input))
              new-str (or (and (listp result) (alist-get 'newString result))
                          (alist-get 'new_string input)))))
     (t
      (setq old-str (alist-get 'old_string input)
            new-str (alist-get 'new_string input))))
    (cond
     ;; structuredPatch — render hunks as plain text
     (patch
      (insert "### Diff\n\n")
      (let ((hunks (if (vectorp patch) (append patch nil) patch)))
        (dolist (hunk hunks)
          (let ((old-start (alist-get 'oldStart hunk))
                (old-lines (alist-get 'oldLines hunk))
                (new-start (alist-get 'newStart hunk))
                (new-lines (alist-get 'newLines hunk))
                (lines-vec (alist-get 'lines hunk)))
            (insert (format "@@ -%s,%s +%s,%s @@\n"
                            (or old-start "?") (or old-lines "?")
                            (or new-start "?") (or new-lines "?")))
            (let ((raw-lines (if (vectorp lines-vec) (append lines-vec nil) lines-vec)))
              (dolist (line raw-lines)
                (insert line "\n"))))))
      (insert "\n"))
     ;; old/new strings — simple before/after
     ((and new-str (stringp new-str) (not (string-empty-p new-str)))
      (let ((old-lines (if (and old-str (stringp old-str))
                           (split-string old-str "\n")
                         nil))
            (new-lines (split-string new-str "\n")))
        (insert "### Diff\n\n")
        (dolist (line old-lines)
          (insert "- " line "\n"))
        (dolist (line new-lines)
          (insert "+ " line "\n"))
        (insert "\n")))
     (t nil))))


(defun claude-gravity--popup-insert-tool-input (name input &optional result status)
  "Insert full tool INPUT for tool NAME into current buffer as markdown.
RESULT and STATUS are optional, used for Edit tools to show structured diffs."
  (pcase name
    ("Bash"
     (let ((cmd (alist-get 'command input)))
       (when cmd
         (insert "**Command:**\n\n```bash\n" cmd "\n```\n\n"))))
    ("Read"
     (let ((path (alist-get 'file_path input)))
       (when path
         (insert (format "**File:** `%s`\n\n" path)))))
    ("Edit"
     (let ((path (alist-get 'file_path input)))
       (when path
         (insert (format "**File:** `%s`\n\n" path))))
     (claude-gravity--popup-insert-edit-diff input result status))
    ("Write"
     (let ((path (alist-get 'file_path input))
           (new (alist-get 'new_string input)))
       (when path
         (insert (format "**File:** `%s`\n\n" path)))
       (when new
         (insert "### Content\n\n```\n" new "\n```\n\n"))))
    ((or "Grep" "Glob")
     (let ((pattern (alist-get 'pattern input))
           (path (alist-get 'path input)))
       (when pattern
         (insert (format "**Pattern:** `%s`\n\n" pattern)))
       (when path
         (insert (format "**Path:** `%s`\n\n" path)))))
    ("AskUserQuestion"
     (let ((questions (alist-get 'questions input)))
       (when (vectorp questions)
         (dotimes (i (length questions))
           (let* ((q (aref questions i))
                  (text (alist-get 'question q))
                  (opts (alist-get 'options q)))
             (insert (format "### Question %d\n\n%s\n\n" (1+ i) (or text "")))
             (when (vectorp opts)
               (dotimes (j (length opts))
                 (let ((opt (aref opts j)))
                   (insert (format "%d. **%s**" (1+ j) (or (alist-get 'label opt) ""))
                           (let ((desc (alist-get 'description opt)))
                             (if desc (format " — %s" desc) ""))
                           "\n")))
               (insert "\n")))))))
    (_
     (when input
       (insert "**Input:**\n\n```\n" (pp-to-string input) "```\n\n")))))


(defun claude-gravity--popup-insert-tool-result (_name _input result status)
  "Insert full tool RESULT into current buffer as markdown.
NAME, INPUT used for context. STATUS for error display."
  (when result
    ;; Normalize MCP-style vector results
    (when (vectorp result)
      (let* ((first (and (> (length result) 0) (aref result 0)))
             (text (and (listp first) (alist-get 'text first))))
        (setq result (when text (list (cons 'stdout text))))))
    (let ((stdout (and (listp result) (alist-get 'stdout result)))
          (stderr (and (listp result) (alist-get 'stderr result)))
          (file-data (and (listp result) (alist-get 'file result))))
      (when (and stdout (not (string-empty-p stdout)))
        (insert "## Output\n\n```\n" stdout "\n```\n\n"))
      (when file-data
        (let ((content (alist-get 'content file-data)))
          (when (and content (not (string-empty-p content)))
            (insert "## Content\n\n```\n" content "\n```\n\n"))))
      (when (and stderr (not (string-empty-p stderr)))
        (insert "## Stderr\n\n```\n" stderr "\n```\n\n"))
      (when (and (equal status "error") (stringp result))
        (insert "## Error\n\n```\n" result "\n```\n\n")))))


(defun claude-gravity--popup-tool-status-mark (status)
  "Return markdown checkbox for tool STATUS."
  (cond ((equal status "done") "- [x]")
        ((equal status "error") "- [!]")
        (t "- [ ]")))


(defun claude-gravity--popup-cycle-content (cycle-idx session)
  "Generate full content for response cycle CYCLE-IDX in SESSION."
  (when session
    (let* ((turns-tl (plist-get session :turns))
           (turn-nodes (when turns-tl (claude-gravity--tlist-items turns-tl))))
      (catch 'found
        (dolist (tn turn-nodes)
          (let* ((cycles-tl (alist-get 'cycles tn))
                 (cycles (when cycles-tl (claude-gravity--tlist-items cycles-tl))))
            (when (and cycles (< cycle-idx (length cycles)))
              (let* ((cycle (nth cycle-idx cycles))
                     (athink (alist-get 'thinking cycle))
                     (atext (alist-get 'text cycle))
                     (tools (claude-gravity--tlist-items (alist-get 'tools cycle))))
                (throw 'found
                       (with-temp-buffer
                         (insert (format "# Response Cycle %d\n\n" cycle-idx))
                         (when (and athink (not (string-empty-p (string-trim athink))))
                           (insert "## Thinking\n\n" (claude-gravity--render-tables-in-text (string-trim athink)) "\n\n"))
                         (when (and atext (not (string-empty-p (string-trim atext))))
                           (insert "## Assistant\n\n" (claude-gravity--render-tables-in-text (string-trim atext)) "\n\n"))
                         (when tools
                           (insert (format "## Tools (%d)\n\n" (length tools)))
                           (dolist (tool tools)
                             (insert (format "%s `%s`\n"
                                             (claude-gravity--popup-tool-status-mark
                                              (alist-get 'status tool))
                                             (claude-gravity--tool-signature
                                              (alist-get 'name tool)
                                              (alist-get 'input tool))))))
                         (buffer-string)))))))
        nil))))


(defun claude-gravity--popup-stop-content (value session)
  "Generate full stop message content for SESSION at turn VALUE."
  (when session
    (let* ((turns-tl (plist-get session :turns))
           (turn-nodes (when turns-tl (claude-gravity--tlist-items turns-tl)))
           (tn (or (cl-find-if (lambda (n) (eql (alist-get 'turn-number n) value)) turn-nodes)
                   (car (last turn-nodes)))))
      (when tn
        (let ((stop-think (alist-get 'stop_thinking tn))
              (stop-text (alist-get 'stop_text tn)))
          (when (or stop-think stop-text)
            (with-temp-buffer
              (insert "# Stop Message\n\n")
              (when (and stop-think (not (string-empty-p stop-think)))
                (insert "## Thinking\n\n" (claude-gravity--render-tables-in-text stop-think) "\n\n"))
              (when (and stop-text (not (string-empty-p stop-text)))
                (insert "## Text\n\n" (claude-gravity--render-tables-in-text stop-text) "\n\n"))
              (buffer-string))))))))


(defun claude-gravity--popup-turn-content (turn-num session)
  "Generate full content for turn TURN-NUM in SESSION."
  (when session
    (let* ((turns-tl (plist-get session :turns))
           (turn-nodes (when turns-tl (claude-gravity--tlist-items turns-tl)))
           (tn (cl-find-if (lambda (n) (= (alist-get 'turn-number n) turn-num))
                           turn-nodes)))
      (when tn
        (let* ((prompt (alist-get 'prompt tn))
               (prompt-text (when prompt (claude-gravity--prompt-text prompt)))
               (cycles (claude-gravity--tlist-items (alist-get 'cycles tn)))
               (stop-think (alist-get 'stop_thinking tn))
               (stop-text (alist-get 'stop_text tn)))
          (with-temp-buffer
            (insert (format "# Turn %d\n\n" turn-num))
            (when (and prompt-text (not (string-empty-p prompt-text)))
              (insert "## Prompt\n\n> " (replace-regexp-in-string "\n" "\n> " prompt-text) "\n\n"))
            (let ((ci 0))
              (dolist (cycle cycles)
                (let ((athink (alist-get 'thinking cycle))
                      (atext (alist-get 'text cycle))
                      (tools (claude-gravity--tlist-items (alist-get 'tools cycle))))
                  (insert (format "## Cycle %d\n\n" ci))
                  (when (and athink (not (string-empty-p (string-trim athink))))
                    (insert "### Thinking\n\n" (claude-gravity--render-tables-in-text (string-trim athink)) "\n\n"))
                  (when (and atext (not (string-empty-p (string-trim atext))))
                    (insert (claude-gravity--render-tables-in-text (string-trim atext)) "\n\n"))
                  (when tools
                    (dolist (tool tools)
                      (insert (format "%s `%s`\n"
                                      (claude-gravity--popup-tool-status-mark
                                       (alist-get 'status tool))
                                      (claude-gravity--tool-signature
                                       (alist-get 'name tool)
                                       (alist-get 'input tool)))))
                    (insert "\n")))
                (cl-incf ci)))
            (when (and stop-think (not (string-empty-p stop-think)))
              (insert "## Stop Thinking\n\n" (claude-gravity--render-tables-in-text stop-think) "\n\n"))
            (when (and stop-text (not (string-empty-p stop-text)))
              (insert "## Stop Text\n\n" (claude-gravity--render-tables-in-text stop-text) "\n\n"))
            (buffer-string)))))))


(defun claude-gravity--popup-task-content (task-id session)
  "Generate full detail for TASK-ID in SESSION."
  (when session
    (let* ((turns-tl (plist-get session :turns))
           (turn-nodes (when turns-tl (claude-gravity--tlist-items turns-tl))))
      (catch 'found
        (dolist (tn turn-nodes)
          (dolist (task (alist-get 'tasks tn))
            (when (equal (alist-get 'taskId task) task-id)
              (throw 'found
                     (with-temp-buffer
                       (insert "# Task\n\n")
                       (insert (format "**Subject:** %s\n\n" (or (alist-get 'subject task) "(none)")))
                       (insert (format "**Status:** %s\n\n" (or (alist-get 'status task) "pending")))
                       (let ((af (alist-get 'activeForm task)))
                         (when af
                           (insert (format "**Active:** %s\n\n" af))))
                       (let ((desc (alist-get 'description task)))
                         (when (and desc (not (string-empty-p desc)))
                           (insert "## Description\n\n" desc "\n")))
                       (buffer-string))))))
        nil))))


(defun claude-gravity--popup-agent-content (value session)
  "Generate full detail for agent VALUE in SESSION."
  (when (and value (listp value) session)
    (let* ((aid (alist-get 'agent_id value))
           (agent (when aid (claude-gravity--find-agent session aid))))
      (when agent
        (let ((atype (alist-get 'type agent))
              (astatus (alist-get 'status agent))
              (adur (alist-get 'duration agent))
              (stop-think (alist-get 'stop_thinking agent))
              (stop-text (alist-get 'stop_text agent))
              (tp (alist-get 'transcript_path agent)))
          (with-temp-buffer
            (insert (format "# Agent: %s\n\n" (or atype "unknown")))
            (insert (format "**ID:** `%s`\n\n" (or aid "?")))
            (insert (format "**Status:** %s\n\n" (or astatus "?")))
            (when adur
              (insert (format "**Duration:** %s\n\n" (claude-gravity--format-duration adur))))
            (when tp
              (insert (format "**Transcript:** `%s`\n\n" tp)))
            (when (and stop-think (not (string-empty-p stop-think)))
              (insert "## Thinking\n\n" stop-think "\n\n"))
            (when (and stop-text (not (string-empty-p stop-text)))
              (insert "## Summary\n\n" stop-text "\n\n"))
            (let ((cycles (claude-gravity--tlist-items (alist-get 'cycles agent))))
              (when cycles
                (insert "## Tool History\n\n")
                (dolist (cycle cycles)
                  (let ((tools (claude-gravity--tlist-items (alist-get 'tools cycle)))
                        (athink (alist-get 'thinking cycle))
                        (atext (alist-get 'text cycle)))
                    (when (and athink (not (string-empty-p (string-trim athink))))
                      (insert "### Thinking\n\n" (string-trim athink) "\n\n"))
                    (when (and atext (not (string-empty-p (string-trim atext))))
                      (insert (string-trim atext) "\n\n"))
                    (when tools
                      (dolist (tool tools)
                        (insert (format "%s `%s`\n"
                                        (claude-gravity--popup-tool-status-mark
                                         (alist-get 'status tool))
                                        (claude-gravity--tool-signature
                                         (alist-get 'name tool)
                                         (alist-get 'input tool)))))
                      (insert "\n"))))))
            (buffer-string)))))))

(provide 'claude-gravity-ui)
;;; claude-gravity-ui.el ends here