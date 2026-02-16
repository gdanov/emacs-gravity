;;; claude-gravity-discovery.el --- Discover skills, agents, commands for Claude projects  -*- lexical-binding: t; -*-

;;; Code:

(require 'claude-gravity-core)

;;; Cache

(defvar claude-gravity--capabilities-cache (make-hash-table :test 'equal)
  "Hash table mapping project-dir -> (timestamp . capabilities-alist).
Capabilities alist has keys: skills, agents, commands.")

(defvar claude-gravity--capabilities-ttl 60
  "Cache TTL in seconds for capability discovery results.")


;;; YAML Frontmatter Parser

(defun claude-gravity--parse-frontmatter (file-path)
  "Parse YAML frontmatter from FILE-PATH.
Returns alist of key-value pairs from the --- delimited header.
Values are trimmed strings.  Multi-line values (e.g. >- blocks)
are joined into a single line."
  (condition-case err
      (with-temp-buffer
        (insert-file-contents file-path)
        (goto-char (point-min))
        (when (looking-at "^---[ \t]*$")
          (forward-line 1)
          (let ((start (point))
                (result nil))
            (when (re-search-forward "^---[ \t]*$" nil t)
              (let ((yaml-text (buffer-substring-no-properties start (match-beginning 0))))
                (with-temp-buffer
                  (insert yaml-text)
                  (goto-char (point-min))
                  (while (re-search-forward
                          "^\\([a-zA-Z_-]+\\)[ \t]*:[ \t]*\\(.*\\)$" nil t)
                    (let ((key (match-string 1))
                          (val (string-trim (match-string 2))))
                      ;; Handle multi-line values (>- or > or |)
                      (when (or (string= val ">-") (string= val ">") (string= val "|"))
                        (let ((lines nil))
                          (forward-line 1)
                          (while (and (not (eobp))
                                      (looking-at "^[ \t]+\\(.+\\)$"))
                            (push (string-trim (match-string 1)) lines)
                            (forward-line 1))
                          (setq val (mapconcat #'identity (nreverse lines) " "))))
                      ;; Strip surrounding quotes
                      (when (and (> (length val) 1)
                                 (or (and (string-prefix-p "\"" val) (string-suffix-p "\"" val))
                                     (and (string-prefix-p "'" val) (string-suffix-p "'" val))))
                        (setq val (substring val 1 -1)))
                      (push (cons (intern key) val) result))))))
            (nreverse result))))
    (error
     (claude-gravity--log 'warn "Failed to parse frontmatter from %s: %s" file-path err)
     nil)))


;;; Directory Scanners

(defun claude-gravity--scan-skills (base-dir scope-label)
  "Scan BASE-DIR for skill directories containing SKILL.md.
SCOPE-LABEL is a string like \"global\" or a plugin name.
Returns list of alists with keys: name, description, scope, file-path."
  (let ((skills-dir (expand-file-name "skills" base-dir))
        (result nil))
    (when (file-directory-p skills-dir)
      (dolist (entry (directory-files skills-dir t "^[^.]"))
        (cond
         ;; Directory with SKILL.md inside
         ((file-directory-p entry)
          (let ((skill-file (expand-file-name "SKILL.md" entry)))
            (when (file-exists-p skill-file)
              (let* ((fm (claude-gravity--parse-frontmatter skill-file))
                     (name (or (alist-get 'name fm)
                               (file-name-nondirectory entry))))
                (push (list (cons 'name name)
                            (cons 'description (or (alist-get 'description fm) ""))
                            (cons 'scope scope-label)
                            (cons 'file-path skill-file)
                            (cons 'type 'skill))
                      result)))))
         ;; Bare SKILL.md at top level (unusual but handle it)
         ((and (file-regular-p entry)
               (string= (file-name-nondirectory entry) "SKILL.md"))
          (let* ((fm (claude-gravity--parse-frontmatter entry))
                 (name (or (alist-get 'name fm) "skill")))
            (push (list (cons 'name name)
                        (cons 'description (or (alist-get 'description fm) ""))
                        (cons 'scope scope-label)
                        (cons 'file-path entry)
                        (cons 'type 'skill))
                  result))))))
    (nreverse result)))


(defun claude-gravity--scan-agents (base-dir scope-label)
  "Scan BASE-DIR/agents/ for .md agent definition files.
SCOPE-LABEL is a string like \"global\" or a plugin name.
Returns list of alists with keys: name, description, scope, file-path."
  (let ((agents-dir (expand-file-name "agents" base-dir))
        (result nil))
    (when (file-directory-p agents-dir)
      (dolist (entry (directory-files agents-dir t "\\.md$"))
        (when (file-regular-p entry)
          (let* ((fm (claude-gravity--parse-frontmatter entry))
                 (name (or (alist-get 'name fm)
                           (file-name-sans-extension
                            (file-name-nondirectory entry)))))
            (push (list (cons 'name name)
                        (cons 'description (or (alist-get 'description fm) ""))
                        (cons 'scope scope-label)
                        (cons 'file-path entry)
                        (cons 'type 'agent))
                  result)))))
    (nreverse result)))


(defun claude-gravity--scan-commands (base-dir scope-label)
  "Scan BASE-DIR/commands/ for .md command definition files.
SCOPE-LABEL is a string like \"global\" or a plugin name.
Returns list of alists with keys: name, description, scope, file-path."
  (let ((commands-dir (expand-file-name "commands" base-dir))
        (result nil))
    (when (file-directory-p commands-dir)
      (dolist (entry (directory-files commands-dir t "\\.md$"))
        (when (file-regular-p entry)
          (let* ((fm (claude-gravity--parse-frontmatter entry))
                 (name (or (alist-get 'name fm)
                           (file-name-sans-extension
                            (file-name-nondirectory entry)))))
            (push (list (cons 'name name)
                        (cons 'description (or (alist-get 'description fm) ""))
                        (cons 'scope scope-label)
                        (cons 'file-path entry)
                        (cons 'type 'command))
                  result)))))
    (nreverse result)))


;;; Plugin Discovery

(defun claude-gravity--discover-installed-plugins ()
  "Read ~/.claude/plugins/installed_plugins.json.
Returns list of alists with keys: name, scope, install-path, project-path."
  (let ((path (expand-file-name "~/.claude/plugins/installed_plugins.json")))
    (when (file-exists-p path)
      (condition-case err
          (let* ((data (claude-gravity--json-read-file path))
                 (plugins (alist-get 'plugins data))
                 (result nil))
            (dolist (entry plugins)
              (let* ((key (car entry))
                     (installs (cdr entry))
                     ;; key is like "beads@beads-marketplace" — extract plugin name
                     (plugin-name (car (split-string (symbol-name key) "@"))))
                (dolist (inst installs)
                  (push (list (cons 'name plugin-name)
                              (cons 'scope (alist-get 'scope inst))
                              (cons 'install-path (alist-get 'installPath inst))
                              (cons 'project-path (alist-get 'projectPath inst)))
                        result))))
            (nreverse result))
        (error
         (claude-gravity--log 'warn "Failed to read installed_plugins.json: %s" err)
         nil)))))


(defun claude-gravity--plugins-for-project (project-dir)
  "Return installed plugins relevant to PROJECT-DIR.
Includes user-scope plugins and project-scope plugins matching PROJECT-DIR."
  (let ((all-plugins (claude-gravity--discover-installed-plugins))
        (result nil))
    (dolist (plugin all-plugins)
      (let ((scope (alist-get 'scope plugin))
            (proj-path (alist-get 'project-path plugin)))
        (when (or (string= scope "user")
                  (and proj-path
                       (string-prefix-p (expand-file-name proj-path)
                                        (expand-file-name project-dir))))
          (push plugin result))))
    (nreverse result)))


;;; Main Discovery Function

(defun claude-gravity--discover-project-capabilities (project-dir)
  "Discover skills, agents, and commands for PROJECT-DIR.
Returns alist with keys: skills, agents, commands.
Each value is a list of capability alists.
Results are cached for `claude-gravity--capabilities-ttl' seconds."
  (let* ((key (expand-file-name project-dir))
         (cached (gethash key claude-gravity--capabilities-cache))
         (now (float-time)))
    (if (and cached
             (< (- now (car cached)) claude-gravity--capabilities-ttl))
        (cdr cached)
      ;; Scan all sources
      (let ((skills nil)
            (agents nil)
            (commands nil)
            (claude-dir (expand-file-name ".claude" project-dir))
            (global-dir (expand-file-name "~/.claude")))

        ;; 1. Project-local .claude/ directory
        (when (file-directory-p claude-dir)
          (setq skills (nconc skills (claude-gravity--scan-skills claude-dir "project")))
          (setq agents (nconc agents (claude-gravity--scan-agents claude-dir "project")))
          (setq commands (nconc commands (claude-gravity--scan-commands claude-dir "project"))))

        ;; 2. Global ~/.claude/ directory
        (when (file-directory-p global-dir)
          (setq skills (nconc skills (claude-gravity--scan-skills global-dir "global")))
          (setq agents (nconc agents (claude-gravity--scan-agents global-dir "global")))
          (setq commands (nconc commands (claude-gravity--scan-commands global-dir "global"))))

        ;; 3. Installed plugins relevant to this project
        (dolist (plugin (claude-gravity--plugins-for-project project-dir))
          (let ((install-path (alist-get 'install-path plugin))
                (plugin-name (alist-get 'name plugin)))
            (when (and install-path (file-directory-p install-path))
              (setq skills (nconc skills (claude-gravity--scan-skills install-path plugin-name)))
              (setq agents (nconc agents (claude-gravity--scan-agents install-path plugin-name)))
              (setq commands (nconc commands (claude-gravity--scan-commands install-path plugin-name))))))

        (let ((result (list (cons 'skills skills)
                            (cons 'agents agents)
                            (cons 'commands commands))))
          (puthash key (cons now result) claude-gravity--capabilities-cache)
          result)))))


(defun claude-gravity--invalidate-capabilities-cache (&optional project-dir)
  "Invalidate capabilities cache for PROJECT-DIR, or all if nil."
  (if project-dir
      (remhash (expand-file-name project-dir) claude-gravity--capabilities-cache)
    (clrhash claude-gravity--capabilities-cache)))


(provide 'claude-gravity-discovery)
;;; claude-gravity-discovery.el ends here
