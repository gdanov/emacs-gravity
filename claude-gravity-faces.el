;;; claude-gravity-faces.el --- Face definitions for Claude Gravity  -*- lexical-binding: t; -*-

;;; Code:

(require 'claude-gravity-core)


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


(defface claude-gravity-diff-added
  '((((background dark)) :foreground "#88ee88" :background "#1a3a1a")
    (((background light)) :foreground "#006600" :background "#ddffdd"))
  "Face for added text in inline Edit diffs."
  :group 'claude-gravity)


(defface claude-gravity-diff-removed
  '((((background dark)) :foreground "#ee8888" :background "#3a1a1a" :strike-through nil)
    (((background light)) :foreground "#660000" :background "#ffdddd" :strike-through nil))
  "Face for removed text in inline Edit diffs."
  :group 'claude-gravity)


(defface claude-gravity-diff-context
  '((((background dark)) :foreground "#888888")
    (((background light)) :foreground "#666666"))
  "Face for context lines in unified-style Edit diffs."
  :group 'claude-gravity)


(defface claude-gravity-diff-header
  '((((background dark)) :foreground "#7799cc")
    (((background light)) :foreground "#336699"))
  "Face for @@ hunk headers in Edit diffs."
  :group 'claude-gravity)


(defface claude-gravity-plan-margin-added
  '((((background dark)) :foreground "#88ee88")
    (((background light)) :foreground "#22aa22"))
  "Fringe face for added lines in plan revision diff."
  :group 'claude-gravity)


(defface claude-gravity-plan-margin-modified
  '((((background dark)) :foreground "#eeaa44")
    (((background light)) :foreground "#cc8800"))
  "Fringe face for modified lines in plan revision diff."
  :group 'claude-gravity)


(defface claude-gravity-plan-margin-deleted
  '((((background dark)) :foreground "#ee8888")
    (((background light)) :foreground "#cc4444"))
  "Fringe face for deleted-region markers in plan revision diff."
  :group 'claude-gravity)


(defface claude-gravity-phase-boundary
  '((((background dark)) :foreground "#66dd66" :background "#2a2a00")
    (((background light)) :foreground "#228822" :background "#ffffdd"))
  "Face for plan-approved phase boundary prompts."
  :group 'claude-gravity)


(defface claude-gravity-header-title
  '((t :weight bold :foreground "white"))
  "Face for the main buffer header title."
  :group 'claude-gravity)


(defface claude-gravity-slug
  '((t :foreground "dark gray" :slant italic))
  "Face for the session slug shown in the header."
  :group 'claude-gravity)

(when (display-graphic-p)
  (define-fringe-bitmap 'claude-gravity-plan-added
    [#b00011100] nil nil '(center repeated))
  (define-fringe-bitmap 'claude-gravity-plan-modified
    [#b00011100] nil nil '(center repeated))
  (define-fringe-bitmap 'claude-gravity-plan-deleted
    [#b00010000
     #b00011000
     #b00011100
     #b00011000
     #b00010000] nil nil '(center t)))

(provide 'claude-gravity-faces)
;;; claude-gravity-faces.el ends here