;;; claude-gravity.el --- Claude Code UI for Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  User

;; Author: User <user@example.com>
;; Version: 0.2.0
;; Package-Requires: ((emacs "27.1") (magit-section "3.0.0") (transient "0.3.0"))
;; Keywords: tools, ai, claude

;;; Commentary:
;; A Magit-like interface for Claude Code with multi-session support.

;;; Code:

(require 'claude-gravity-core)
(require 'claude-gravity-faces)
(require 'claude-gravity-session)
(require 'claude-gravity-state)
(require 'claude-gravity-events)
(require 'claude-gravity-text)
(require 'claude-gravity-diff)
(require 'claude-gravity-render)
(require 'claude-gravity-ui)
(require 'claude-gravity-socket)
(require 'claude-gravity-actions)
(require 'claude-gravity-tmux)
(require 'claude-gravity-debug)

(provide 'claude-gravity)
;;; clatude-gravity.el ends here
