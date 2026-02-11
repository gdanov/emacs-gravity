;;; claude-gravity-ui.el --- UI buffers and commands for Claude Gravity  -*- lexical-binding: t; -*-

;;; Code:

(require 'claude-gravity-core)
(require 'claude-gravity-faces)
(require 'claude-gravity-session)
(require 'claude-gravity-state)
(require 'claude-gravity-render)

(declare-function claude-gravity-server-alive-p "claude-gravity-socket")
(declare-function claude-gravity--write-allow-pattern-for-tool "claude-gravity-socket")
(declare-function claude-gravity--send-permission-response "claude-gravity-socket")
(declare-function claude-gravity--current-session-tmux-p "claude-gravity-tmux")
(declare-function claude-gravity-debug-show "claude-gravity-debug")
(defvar claude-gravity--tmux-sessions)


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
                       (let ((tmux-badge (if (gethash sid claude-gravity--tmux-sessions)
                                                (propertize " [tmux]" 'face 'claude-gravity-detail-label)
                                              ""))
                             (inbox-badge (claude-gravity--inbox-badges sid)))
                         (magit-insert-section (session-entry sid)
                           (magit-insert-heading
                             (format "%s%s%s %s  %s%s  [%d tools]%s"
                                     (claude-gravity--indent)
                                     indicator tmux-badge label
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
                               (claude-gravity--insert-inbox-item item)))))))))
               projects)))
          (goto-char (min pos (point-max)))
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
                (status-str (if (eq claude-st 'responding) "responding"
                              (let* ((last-ev (plist-get session :last-event-time))
                                     (idle-secs (and last-ev
                                                     (float-time
                                                      (time-subtract (current-time) last-ev)))))
                                (cond
                                 ((null idle-secs) "idle")
                                 ((< idle-secs 60) "idle")
                                 ((< idle-secs 3600) (format "idle %dm" (truncate (/ idle-secs 60))))
                                 (t (format "idle %dh" (truncate (/ idle-secs 3600))))))))
                (entry (format "%-20s %s  [%d tools]%s" label status-str n-tools badges)))
           (push entry candidates)
           (push (cons entry sid) id-map))))
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
        (claude-gravity-open-session sid)))))


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
              (pos (point)))
          (erase-buffer)
          (magit-insert-section (root)
            (claude-gravity-insert-header session)
            (claude-gravity-insert-plan session)
            (claude-gravity-insert-streaming-text session)
            (claude-gravity-insert-turns session)
            (claude-gravity--insert-session-inbox session)
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


(defun claude-gravity--session-header-line ()
  "Return header-line string for the current session buffer."
  (when-let* ((sid claude-gravity--buffer-session-id)
              (session (gethash sid claude-gravity--sessions)))
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
                 ((eq claude-st 'responding)
                  (propertize "●" 'face 'claude-gravity-status-responding))
                 (t
                  (propertize "●" 'face 'claude-gravity-status-idle))))
           (status-word (cond
                         ((eq status 'ended)
                          (propertize "ended" 'face 'claude-gravity-session-ended))
                         ((eq claude-st 'responding)
                          (propertize "responding" 'face 'claude-gravity-status-responding))
                         (t
                          (propertize (concat "idle" (or idle-str ""))
                                     'face 'claude-gravity-status-idle))))
           (slug (propertize (claude-gravity--session-label session)
                             'face 'claude-gravity-slug))
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
           (parts (list " " dot " " status-word "  " slug
                        (when perm-mode
                          (propertize (format "  [%s]" perm-mode)
                                     'face 'claude-gravity-detail-label))
                        (when model-name
                          (propertize (format "  %s" model-name)
                                     'face 'claude-gravity-detail-label))
                        (propertize (format "  ◆ %d tools" tool-count)
                                    'face 'claude-gravity-detail-label))))
      (when elapsed
        (setq parts (append parts
                            (list (propertize (format "  ⏱ %s" (claude-gravity--format-elapsed elapsed))
                                             'face 'claude-gravity-detail-label)))))
      (when (and in-tokens (> in-tokens 0))
        (setq parts (append parts
                            (list (propertize (format "  ↓%s ↑%s tokens"
                                                     (claude-gravity--format-token-count in-tokens)
                                                     (claude-gravity--format-token-count out-tokens))
                                             'face 'claude-gravity-detail-label)))))
      ;; StatusLine data: cost, context %, lines changed
      (let ((cost (plist-get session :cost))
            (ctx-pct (plist-get session :context-pct))
            (lines-add (plist-get session :sl-lines-added))
            (lines-rm (plist-get session :sl-lines-removed)))
        (when cost
          (setq parts (append parts
                              (list (propertize (format "  $%.2f" cost)
                                               'face 'claude-gravity-detail-label)))))
        (when ctx-pct
          (let ((face (cond ((>= ctx-pct 90) 'error)
                            ((>= ctx-pct 70) 'warning)
                            (t 'claude-gravity-detail-label))))
            (setq parts (append parts
                                (list (propertize (format "  ctx:%d%%" ctx-pct)
                                                 'face face))))))
        (when (and lines-add lines-rm (or (> lines-add 0) (> lines-rm 0)))
          (setq parts (append parts
                              (list (propertize (format "  +%d -%d" lines-add lines-rm)
                                               'face 'claude-gravity-detail-label))))))
      ;; Inbox badges for actionable items
      (let ((inbox-badge (claude-gravity--inbox-badges sid)))
        (unless (string-empty-p inbox-badge)
          (setq parts (append parts (list inbox-badge)))))
      (apply #'concat (delq nil parts)))))


(define-derived-mode claude-gravity-session-mode claude-gravity-mode "Claude"
  "Major mode for a single Structured Claude Session buffer."
  (setq mode-name '(:eval (if claude-gravity--follow-mode
                              "Claude[F]"
                            "Claude")))
  (setq header-line-format '(:eval (claude-gravity--session-header-line))))


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


;;; Commands

;;;###autoload (autoload 'claude-gravity-menu "claude-gravity" nil t)
(transient-define-prefix claude-gravity-menu ()
  "Interactions with Claude Code."
  [["Actions"
    ("c" "Comment" claude-gravity-comment-at-point)
    ("g" "Refresh" claude-gravity-refresh)
    ("t" "Tail" claude-gravity-tail)
    ("f" "Follow mode" claude-gravity-follow-mode)
    ("P" "Show Plan" claude-gravity-show-plan)
    ("F" "Open plan file" claude-gravity-open-plan-file)
    ("T" "Parse transcript" claude-gravity-view-agent-transcript)
    ("V" "Open transcript" claude-gravity-open-agent-transcript)
    ("M" "Debug messages" claude-gravity-debug-show)]
   ["Sessions"
    ("S" "Start (tmux)" claude-gravity-start-session)
    ("s" "Compose prompt" claude-gravity-compose-prompt
     :inapt-if-not claude-gravity--current-session-tmux-p)
    ("/" "Slash command" claude-gravity-slash-command
     :inapt-if-not claude-gravity--current-session-tmux-p)
    ("r" "Resume session" claude-gravity-resume-session)
    ("$" "Terminal" claude-gravity-terminal-session
     :inapt-if-not claude-gravity--current-session-tmux-p)
    ("<backtab>" "Cycle perm mode" claude-gravity-toggle-permission-mode
     :inapt-if-not claude-gravity--current-session-tmux-p)
    ("C" "Reset/clear" claude-gravity-reset-session
     :inapt-if-not claude-gravity--current-session-tmux-p)
    ("K" "Stop session" claude-gravity-stop-session
     :inapt-if-not claude-gravity--current-session-tmux-p)
    ("E" "Send Escape" claude-gravity-send-escape
     :inapt-if-not claude-gravity--current-session-tmux-p)]
   ["Manage"
    ("D" "Remove ended" claude-gravity-cleanup-sessions)
    ("R" "Reset all idle" claude-gravity-reset-status)
    ("X" "Detect dead" claude-gravity-detect-dead-sessions)
    ("d" "Delete session" claude-gravity-delete-session)
    ""
    "Permissions"
    ("A" "Copy allow pattern" claude-gravity-add-allow-pattern)
    ("a" "Add to settings" claude-gravity-add-allow-pattern-to-settings)]]
  ["Navigation"
   ("b" "Switch session" claude-gravity-switch-session)
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
                                  magit-section-toggle)))
    (setq claude-gravity--follow-mode nil)
    (remove-hook 'post-command-hook #'claude-gravity--follow-detect-manual t)
    (force-mode-line-update)
    (claude-gravity--log 'debug "Follow mode OFF (manual navigation)")))


(defun claude-gravity-tail ()
  "Collapse all sections and focus on the tail of the latest turn.
Hides Plan, Files, Allow Patterns, and past turns, then expands
the most recent turn and scrolls to its last response cycle or
final reply text."
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
        ;; Single pass over turns: hide all, track last
        (let ((last-turn nil))
          (dolist (child (oref turns-section children))
            (when (eq (oref child type) 'turn)
              (magit-section-hide child)
              (setq last-turn child)))
          (when last-turn
            (magit-section-show last-turn)
            ;; Single pass over turn children: hide cycles, track last
            (let ((last-cycle nil))
              (dolist (child (oref last-turn children))
                (when (eq (oref child type) 'response-cycle)
                  (magit-section-hide child)
                  (setq last-cycle child)))
              (when last-cycle
                (magit-section-show last-cycle)))
            (goto-char (1- (oref last-turn end)))
            (recenter -3)))))))


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
(define-key claude-gravity-mode-map (kbd "s") 'claude-gravity-compose-prompt)

(define-key claude-gravity-mode-map (kbd "/") 'claude-gravity-slash-command)

(define-key claude-gravity-mode-map (kbd "S") 'claude-gravity-start-session)

(define-key claude-gravity-mode-map (kbd "r") 'claude-gravity-resume-session)

(define-key claude-gravity-mode-map (kbd "C") 'claude-gravity-reset-session)

(define-key claude-gravity-mode-map (kbd "$") 'claude-gravity-terminal-session)

(define-key claude-gravity-mode-map (kbd "<backtab>") 'claude-gravity-toggle-permission-mode)

(define-key claude-gravity-mode-map (kbd "E") 'claude-gravity-send-escape)

(define-key claude-gravity-mode-map (kbd "K") 'claude-gravity-stop-session)

(define-key claude-gravity-mode-map (kbd "M") 'claude-gravity-debug-show)

(provide 'claude-gravity-ui)
;;; claude-gravity-ui.el ends here