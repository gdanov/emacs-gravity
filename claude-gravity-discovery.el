;;; claude-gravity-discovery.el --- Discover skills, agents, commands for Claude projects  -*- lexical-binding: t; -*-

;;; Code:

(require 'claude-gravity-core)

;;; Cache

(defvar claude-gravity--capabilities-cache (make-hash-table :test 'equal)
  "Hash table mapping project-dir -> (timestamp . capabilities-alist).
Capabilities alist has keys: plugins, standalone-skills, standalone-agents,
standalone-commands, standalone-mcp-servers.")

(defvar claude-gravity--capabilities-ttl 60
  "Cache TTL in seconds for capability discovery results.")

(defvar claude-gravity--frontmatter-cache (make-hash-table :test 'equal)
  "Per-file frontmatter cache. Maps file-path -> (mtime . parsed-alist).")


;;; Built-in Agents

(defconst claude-gravity--builtin-agents
  '("Explore" "Plan" "Bash" "general-purpose" "statusline-setup"
    "claude-code-guide" "linear-branch-manager" "beads:task-agent"
    "plugin-dev:agent-creator" "dev-loop:architect" "dev-loop:verifier"
    "dev-loop:story-partner" "sentry:issue-summarizer"
    "sentry:seer" "code-simplifier:code-simplifier"
    "code-review:code-review" "code-review:fix-pr-comments")
  "List of known Claude Code built-in agent types.")


(defun claude-gravity--agent-scope (agent-cap)
  "Determine the scope of AGENT-CAP: plugin, built-in, or standalone.
Returns a symbol: 'plugin, 'built-in, or 'standalone."
  (let ((scope (alist-get 'scope agent-cap))
        (name (alist-get 'name agent-cap)))
    (cond
     ;; If scope is a plugin name, it's a plugin agent
     ((and scope (not (member scope '("global" "project"))))
      'plugin)
     ;; If name is in builtin-agents list
     ((member name claude-gravity--builtin-agents)
      'built-in)
     ;; Otherwise standalone
     (t 'standalone))))


;;; YAML Frontmatter Parser

(defun claude-gravity--parse-frontmatter (file-path)
  "Parse YAML frontmatter from FILE-PATH with mtime-based caching.
Returns alist of key-value pairs from the --- delimited header.
Values are trimmed strings.  Multi-line values (e.g. >- blocks)
are joined into a single line."
  (let* ((abs-path (expand-file-name file-path))
         (attrs (file-attributes abs-path))
         (mtime (and attrs (float-time (file-attribute-modification-time attrs))))
         (cached (gethash abs-path claude-gravity--frontmatter-cache)))
    ;; Check cache: hit if mtime matches
    (if (and cached mtime (equal mtime (car cached)))
        (cdr cached)  ; Return cached alist
      ;; Cache miss: parse with 2KB limit
      (condition-case err
          (with-temp-buffer
            (insert-file-contents abs-path nil 0 2048)
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
                          (push (cons (intern key) val) result)))))
                ;; Store result in cache
                (let ((result-alist (nreverse result)))
                  (when mtime
                    (puthash abs-path (cons mtime result-alist) claude-gravity--frontmatter-cache))
                  result-alist)))))
        (error
         (ignore-errors
           (claude-gravity--log 'warn "Failed to parse frontmatter from %s: %s" file-path err))
         nil)))))


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
                            (cons 'type 'skill)
                            (cons 'frontmatter fm))
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
                        (cons 'type 'skill)
                        (cons 'frontmatter fm))
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
                        (cons 'type 'agent)
                        (cons 'frontmatter fm))
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
                        (cons 'type 'command)
                        (cons 'frontmatter fm))
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


;;; MCP Server Discovery

(defun claude-gravity--scan-mcp-servers (base-dir scope-label)
  "Scan BASE-DIR for .mcp.json file and parse MCP server configs.
SCOPE-LABEL is a string like \"global\", \"project\", or a plugin name.
Returns list of alists with keys: name, scope, config, file-path."
  (let ((mcp-file (expand-file-name ".mcp.json" base-dir))
        (result nil))
    (when (file-exists-p mcp-file)
      (condition-case err
          (let ((config (claude-gravity--json-read-file mcp-file)))
            (dolist (entry config)
              (let* ((server-name (car entry))
                     (server-config (cdr entry)))
                (push (list (cons 'name (symbol-name server-name))
                            (cons 'scope scope-label)
                            (cons 'config server-config)
                            (cons 'file-path mcp-file)
                            (cons 'type 'mcp-server))
                      result)))
            (nreverse result))
        (error
         (claude-gravity--log 'warn "Failed to parse MCP config from %s: %s" mcp-file err)
         nil)))))


;;; Capability Grouping

(defun claude-gravity--capabilities-total-count (grouped-caps)
  "Return total count of capabilities from GROUPED-CAPS structure."
  (let ((total 0))
    (dolist (plugin (alist-get 'plugins grouped-caps))
      (cl-incf total (alist-get 'total plugin)))
    (dolist (_ (alist-get 'standalone-skills grouped-caps)) (cl-incf total))
    (dolist (_ (alist-get 'standalone-agents grouped-caps)) (cl-incf total))
    (dolist (_ (alist-get 'standalone-commands grouped-caps)) (cl-incf total))
    (dolist (_ (alist-get 'standalone-mcp-servers grouped-caps)) (cl-incf total))
    total))


(defun claude-gravity--group-capabilities-by-source (skills agents commands mcp-servers plugins)
  "Group capabilities by source: plugin vs. standalone.
Returns alist with keys: plugins, standalone-skills, standalone-agents,
standalone-commands, standalone-mcp-servers.
PLUGINS is list of plugin metadata to identify which caps belong to plugins."
  (let ((plugin-names (mapcar (lambda (p) (alist-get 'name p)) plugins))
        (plugin-skills (make-hash-table :test 'equal))
        (plugin-agents (make-hash-table :test 'equal))
        (plugin-commands (make-hash-table :test 'equal))
        (plugin-mcp (make-hash-table :test 'equal))
        (standalone-skills nil)
        (standalone-agents nil)
        (standalone-commands nil)
        (standalone-mcp-servers nil))
    ;; Initialize plugin buckets
    (dolist (plugin plugins)
      (let ((name (alist-get 'name plugin)))
        (puthash name nil plugin-skills)
        (puthash name nil plugin-agents)
        (puthash name nil plugin-commands)
        (puthash name nil plugin-mcp)))
    ;; Categorize skills
    (dolist (skill skills)
      (let ((scope (alist-get 'scope skill)))
        (if (member scope plugin-names)
            (puthash scope (cons skill (gethash scope plugin-skills)) plugin-skills)
          (push skill standalone-skills))))
    ;; Categorize agents
    (dolist (agent agents)
      (let ((scope (alist-get 'scope agent)))
        (if (member scope plugin-names)
            (puthash scope (cons agent (gethash scope plugin-agents)) plugin-agents)
          (push agent standalone-agents))))
    ;; Categorize commands
    (dolist (command commands)
      (let ((scope (alist-get 'scope command)))
        (if (member scope plugin-names)
            (puthash scope (cons command (gethash scope plugin-commands)) plugin-commands)
          (push command standalone-commands))))
    ;; Categorize MCP servers
    (dolist (server mcp-servers)
      (let ((scope (alist-get 'scope server)))
        (if (member scope plugin-names)
            (puthash scope (cons server (gethash scope plugin-mcp)) plugin-mcp)
          (push server standalone-mcp-servers))))
    ;; Build plugin groups with their capabilities
    (let ((plugin-groups nil))
      (dolist (plugin plugins)
        (let* ((name (alist-get 'name plugin))
               (p-skills (nreverse (gethash name plugin-skills)))
               (p-agents (nreverse (gethash name plugin-agents)))
               (p-commands (nreverse (gethash name plugin-commands)))
               (p-mcp (nreverse (gethash name plugin-mcp)))
               (total (+ (length p-skills) (length p-agents) (length p-commands) (length p-mcp))))
          (when (> total 0)
            (push (list (cons 'name name)
                        (cons 'scope (alist-get 'scope plugin))
                        (cons 'install-path (alist-get 'install-path plugin))
                        (cons 'skills p-skills)
                        (cons 'agents p-agents)
                        (cons 'commands p-commands)
                        (cons 'mcp-servers p-mcp)
                        (cons 'total total))
                  plugin-groups))))
      (list (cons 'plugins (nreverse plugin-groups))
            (cons 'standalone-skills (nreverse standalone-skills))
            (cons 'standalone-agents (nreverse standalone-agents))
            (cons 'standalone-commands (nreverse standalone-commands))
            (cons 'standalone-mcp-servers (nreverse standalone-mcp-servers))))))


;;; Main Discovery Function

(defun claude-gravity--discover-project-capabilities (project-dir)
  "Discover skills, agents, commands, and MCP servers for PROJECT-DIR.
Returns alist with keys: plugins, standalone-skills, standalone-agents,
standalone-commands, standalone-mcp-servers.
Each value is a list of capability alists, grouped hierarchically.
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
            (mcp-servers nil)
            (claude-dir (expand-file-name ".claude" project-dir))
            (global-dir (expand-file-name "~/.claude"))
            (plugins nil))

        ;; Get plugins with error handling
        (condition-case scan-err
            (setq plugins (claude-gravity--plugins-for-project project-dir))
          (error
           (claude-gravity--log 'warn "Failed to scan plugins for %s: %s" project-dir scan-err)
           (setq plugins nil)))

        ;; 1. Project-local .claude/ directory
        (when (file-directory-p claude-dir)
          (condition-case scan-err
              (setq skills (nconc skills (claude-gravity--scan-skills claude-dir "project")))
            (error
             (claude-gravity--log 'warn "Failed to scan project skills in %s: %s" claude-dir scan-err)))
          (condition-case scan-err
              (setq agents (nconc agents (claude-gravity--scan-agents claude-dir "project")))
            (error
             (claude-gravity--log 'warn "Failed to scan project agents in %s: %s" claude-dir scan-err)))
          (condition-case scan-err
              (setq commands (nconc commands (claude-gravity--scan-commands claude-dir "project")))
            (error
             (claude-gravity--log 'warn "Failed to scan project commands in %s: %s" claude-dir scan-err)))
          (condition-case scan-err
              (setq mcp-servers (nconc mcp-servers (claude-gravity--scan-mcp-servers claude-dir "project")))
            (error
             (claude-gravity--log 'warn "Failed to scan project MCP servers in %s: %s" claude-dir scan-err))))

        ;; 2. Global ~/.claude/ directory
        (when (file-directory-p global-dir)
          (condition-case scan-err
              (setq skills (nconc skills (claude-gravity--scan-skills global-dir "global")))
            (error
             (claude-gravity--log 'warn "Failed to scan global skills: %s" scan-err)))
          (condition-case scan-err
              (setq agents (nconc agents (claude-gravity--scan-agents global-dir "global")))
            (error
             (claude-gravity--log 'warn "Failed to scan global agents: %s" scan-err)))
          (condition-case scan-err
              (setq commands (nconc commands (claude-gravity--scan-commands global-dir "global")))
            (error
             (claude-gravity--log 'warn "Failed to scan global commands: %s" scan-err)))
          (condition-case scan-err
              (setq mcp-servers (nconc mcp-servers (claude-gravity--scan-mcp-servers global-dir "global")))
            (error
             (claude-gravity--log 'warn "Failed to scan global MCP servers: %s" scan-err))))

        ;; 3. Installed plugins relevant to this project
        (dolist (plugin plugins)
          (let ((install-path (alist-get 'install-path plugin))
                (plugin-name (alist-get 'name plugin)))
            (when (and install-path (file-directory-p install-path))
              (condition-case scan-err
                  (setq skills (nconc skills (claude-gravity--scan-skills install-path plugin-name)))
                (error
                 (claude-gravity--log 'warn "Failed to scan plugin %s skills: %s" plugin-name scan-err)))
              (condition-case scan-err
                  (setq agents (nconc agents (claude-gravity--scan-agents install-path plugin-name)))
                (error
                 (claude-gravity--log 'warn "Failed to scan plugin %s agents: %s" plugin-name scan-err)))
              (condition-case scan-err
                  (setq commands (nconc commands (claude-gravity--scan-commands install-path plugin-name)))
                (error
                 (claude-gravity--log 'warn "Failed to scan plugin %s commands: %s" plugin-name scan-err)))
              (condition-case scan-err
                  (setq mcp-servers (nconc mcp-servers (claude-gravity--scan-mcp-servers install-path plugin-name)))
                (error
                 (claude-gravity--log 'warn "Failed to scan plugin %s MCP servers: %s" plugin-name scan-err))))))

        ;; Group by source
        (let ((result (claude-gravity--group-capabilities-by-source
                       skills agents commands mcp-servers (or plugins nil))))
          (puthash key (cons now result) claude-gravity--capabilities-cache)
          result)))))


(defun claude-gravity--invalidate-capabilities-cache (&optional project-dir)
  "Invalidate capabilities cache for PROJECT-DIR, or all if nil."
  (if project-dir
      (remhash (expand-file-name project-dir) claude-gravity--capabilities-cache)
    (clrhash claude-gravity--capabilities-cache)))


(defun claude-gravity-refresh-capabilities ()
  "Force refresh of capabilities cache and re-render overview buffer.
Useful when plugins, skills, agents, or commands are added/removed."
  (interactive)
  (claude-gravity--invalidate-capabilities-cache)
  (claude-gravity--render-overview)
  (message "Capabilities cache cleared and overview refreshed"))


(defun claude-gravity--invalidate-frontmatter-cache (&optional file-path)
  "Invalidate frontmatter cache for FILE-PATH, or all if nil."
  (if file-path
      (remhash (expand-file-name file-path) claude-gravity--frontmatter-cache)
    (clrhash claude-gravity--frontmatter-cache)))


(provide 'claude-gravity-discovery)
;;; claude-gravity-discovery.el ends here
