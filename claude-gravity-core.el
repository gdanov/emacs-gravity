;;; claude-gravity-core.el --- Core utilities for Claude Gravity  -*- lexical-binding: t; -*-

;;; Code:

(require 'magit-section)
(require 'transient)
(require 'json)
(require 'seq)
(require 'cl-lib)


(defgroup claude-gravity nil
  "Claude Code interface."
  :group 'tools)


(defcustom claude-gravity-managed-statusline 'gravity
  "StatusLine override for managed (tmux) Claude sessions.
`gravity' -- use built-in gravity statusline (sends data to Emacs, minimal terminal output).
`suppress' -- gravity statusline with no terminal output.
A string -- path to a custom statusline script.
nil -- no override; use the global statusline from settings.json."
  :type '(choice (const :tag "Gravity statusline" gravity)
                 (const :tag "Suppress terminal output" suppress)
                 (string :tag "Custom script path")
                 (const :tag "No override" nil))
  :group 'claude-gravity)


(defcustom claude-gravity-diff-max-lines 30
  "Maximum number of lines to show in inline Edit diffs.
Diffs longer than this are truncated with an ellipsis indicator."
  :type 'integer
  :group 'claude-gravity)


(defvar claude-gravity-log-level 'warn
  "Minimum log level.  One of: debug, info, warn, error.
Messages below this level are suppressed.")


(defconst claude-gravity--log-levels '((debug . 0) (info . 1) (warn . 2) (error . 3))
  "Numeric values for log levels.")


(defun claude-gravity--log (level fmt &rest args)
  "Log FMT with ARGS if LEVEL >= `claude-gravity-log-level'."
  (when (>= (alist-get level claude-gravity--log-levels 0)
            (alist-get claude-gravity-log-level claude-gravity--log-levels 0))
    (apply #'message fmt args)))


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
          (claude-gravity--log 'debug "Added comment: %s" text))
      (claude-gravity--log 'debug "No section selected"))))


(defun claude-gravity--truncate (str max-len)
  "Truncate STR to MAX-LEN chars, adding ellipsis if needed."
  (if (and str (> (length str) max-len))
      (concat (substring str 0 (- max-len 1)) "\u2026")
    (or str "")))


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

(provide 'claude-gravity-core)
;;; claude-gravity-core.el ends here