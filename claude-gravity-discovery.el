;;; claude-gravity-discovery.el --- Discover skills, agents, commands, rules, hooks for Claude projects  -*- lexical-binding: t; -*-

;;; Code:

(require 'claude-gravity-core)

;;; Cache

(defvar claude-gravity--capabilities-cache (make-hash-table :test 'equal)
  "Hash table mapping project-dir -> (timestamp . capabilities-alist).
Capabilities alist has keys: plugins, standalone-skills, standalone-agents,
standalone-commands, standalone-rules, standalone-mcp-servers, standalone-hooks,
standalone-instructions, standalone-settings.")

(defvar claude-gravity--capabilities-by-scope-cache (make-hash-table :test 'equal)
  "Hash table mapping project-dir -> (timestamp . scope-grouped-alist).
Scope-grouped alist has keys: project, user, plugins.")

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


(defun claude-gravity--scan-rules (base-dir scope-label)
  "Scan BASE-DIR/rules/ for .md rule definition files.
SCOPE-LABEL is a string like \"global\" or a plugin name.
Returns list of alists with keys: name, description, scope, file-path."
  (let ((rules-dir (expand-file-name "rules" base-dir))
        (result nil))
    (when (file-directory-p rules-dir)
      (dolist (entry (directory-files rules-dir t "\\.md$"))
        (when (file-regular-p entry)
          (let* ((fm (claude-gravity--parse-frontmatter entry))
                 (name (or (alist-get 'name fm)
                           (file-name-sans-extension
                            (file-name-nondirectory entry)))))
            (push (list (cons 'name name)
                        (cons 'description (or (alist-get 'description fm) ""))
                        (cons 'scope scope-label)
                        (cons 'file-path entry)
                        (cons 'type 'rule)
                        (cons 'frontmatter fm))
                  result)))))
    (nreverse result)))


;;; Instruction File Discovery

(defun claude-gravity--scan-instruction-files (project-dir)
  "Discover CLAUDE.md instruction files for PROJECT-DIR.
Checks project root, .claude/ dir, and global ~/.claude/.
Returns list of alists with keys: name, description, scope, file-path, type."
  (let ((result nil)
        (candidates
         (list (cons (expand-file-name "CLAUDE.md" project-dir) "project")
               (cons (expand-file-name ".claude/CLAUDE.md" project-dir) "project")
               (cons (expand-file-name "~/.claude/CLAUDE.md") "global"))))
    (dolist (candidate candidates)
      (let ((path (car candidate))
            (scope (cdr candidate)))
        (when (file-exists-p path)
          (let ((lines (with-temp-buffer
                         (insert-file-contents path)
                         (count-lines (point-min) (point-max)))))
            (push (list (cons 'name (file-name-nondirectory path))
                        (cons 'description (format "%d lines" lines))
                        (cons 'scope scope)
                        (cons 'file-path path)
                        (cons 'type 'instruction))
                  result)))))
    (nreverse result)))


;;; Hook Discovery




(defun claude-gravity--scan-hooks-from-data (data scope-label file-path)
  "Extract hook entries from parsed JSON DATA.
DATA can be a plugin.json hooks object (event → matchers) or an array/list.
Returns list of hook alists."
  (let ((result nil))
    (cond
     ;; Object with event-name keys (plugin.json format):
     ;; { "PreToolUse": [{ "matcher": "", "hooks": [{ "command": "..." }] }] }
     ((and (listp data) (symbolp (caar data)))
      (dolist (entry data)
        (let* ((event (symbol-name (car entry)))
               (matchers (claude-gravity--as-list (cdr entry))))
          (dolist (matcher-obj matchers)
            (let* ((matcher (alist-get 'matcher matcher-obj))
                   (inner-hooks (claude-gravity--as-list (alist-get 'hooks matcher-obj))))
              (dolist (hook inner-hooks)
                (let* ((command (alist-get 'command hook))
                       (status-msg (alist-get 'statusMessage hook))
                       (desc (concat event
                                     (when (and matcher (stringp matcher)
                                                (not (string-empty-p matcher)))
                                       (format " [%s]" matcher))
                                     (when status-msg
                                       (format " — %s" status-msg)))))
                  (push (list (cons 'name event)
                              (cons 'description desc)
                              (cons 'scope scope-label)
                              (cons 'file-path file-path)
                              (cons 'type 'hook))
                        result))))))))
     ;; Array/list of hook entries (simpler format)
     (t
      (dolist (hook (claude-gravity--as-list data))
        (let* ((event (or (alist-get 'event hook) "unknown"))
               (desc (format "%s" event)))
          (push (list (cons 'name event)
                      (cons 'description desc)
                      (cons 'scope scope-label)
                      (cons 'file-path file-path)
                      (cons 'type 'hook))
                result)))))
    (nreverse result)))


(defun claude-gravity--scan-hooks-json (base-dir scope-label)
  "Scan BASE-DIR for hooks in plugin.json or hooks.json.
SCOPE-LABEL is a string like \"global\" or a plugin name.
Returns list of alists with keys: name, description, scope, file-path, type."
  (let ((result nil))
    ;; Check .claude-plugin/plugin.json (standard plugin format)
    (let ((plugin-json (expand-file-name ".claude-plugin/plugin.json" base-dir)))
      (when (file-exists-p plugin-json)
        (condition-case err
            (let* ((data (claude-gravity--json-read-file plugin-json))
                   (hooks-data (alist-get 'hooks data)))
              (when hooks-data
                (setq result (nconc result
                                    (claude-gravity--scan-hooks-from-data
                                     hooks-data scope-label plugin-json)))))
          (error
           (claude-gravity--log 'warn "Failed to parse plugin.json hooks from %s: %s"
                                plugin-json err)))))
    ;; Check standalone hooks.json
    (let ((hooks-file (expand-file-name "hooks.json" base-dir)))
      (when (file-exists-p hooks-file)
        (condition-case err
            (let ((data (claude-gravity--json-read-file hooks-file)))
              (setq result (nconc result
                                  (claude-gravity--scan-hooks-from-data
                                   data scope-label hooks-file))))
          (error
           (claude-gravity--log 'warn "Failed to parse hooks.json from %s: %s"
                                hooks-file err)))))
    result))


(defun claude-gravity--scan-settings-hooks (settings-file scope-label)
  "Extract hooks from SETTINGS-FILE (settings.json).
Returns list of hook alists."
  (when (file-exists-p settings-file)
    (condition-case err
        (let* ((data (claude-gravity--json-read-file settings-file))
               (hooks-data (alist-get 'hooks data)))
          (when hooks-data
            (claude-gravity--scan-hooks-from-data hooks-data scope-label settings-file)))
      (error
       (claude-gravity--log 'warn "Failed to parse hooks from %s: %s" settings-file err)
       nil))))


;;; Settings File Discovery

(defun claude-gravity--scan-settings-files (project-dir)
  "Discover settings files for PROJECT-DIR.
Checks project .claude/ dir and global ~/.claude/.
Returns list of alists with keys: name, description, scope, file-path, type."
  (let ((result nil)
        (candidates
         (list (cons (expand-file-name ".claude/settings.json" project-dir) "project")
               (cons (expand-file-name ".claude/settings.local.json" project-dir) "project")
               (cons (expand-file-name "~/.claude/settings.json") "global")
               (cons (expand-file-name "~/.claude/settings.local.json") "global"))))
    (dolist (candidate candidates)
      (let ((path (car candidate))
            (scope (cdr candidate)))
        (when (file-exists-p path)
          (condition-case err
              (let* ((data (claude-gravity--json-read-file path))
                     (parts nil))
                ;; Summarize key settings
                (when-let ((perm (alist-get 'permissions data)))
                  (let ((allow (length (or (alist-get 'allow perm) nil)))
                        (deny (length (or (alist-get 'deny perm) nil))))
                    (when (> (+ allow deny) 0)
                      (push (format "allow:%d deny:%d" allow deny) parts))))
                (when-let ((allowed (alist-get 'allowedTools data)))
                  (push (format "allowedTools:%d" (length allowed)) parts))
                (when-let ((disallowed (alist-get 'disallowedTools data)))
                  (push (format "disallowedTools:%d" (length disallowed)) parts))
                (when-let ((model (alist-get 'model data)))
                  (push (format "model:%s" model) parts))
                (let ((desc (if parts
                                (mapconcat #'identity (nreverse parts) ", ")
                              "")))
                  (push (list (cons 'name (file-name-nondirectory path))
                              (cons 'description desc)
                              (cons 'scope scope)
                              (cons 'file-path path)
                              (cons 'type 'setting))
                        result)))
            (error
             ;; File exists but can't parse — still show it
             (push (list (cons 'name (file-name-nondirectory path))
                         (cons 'description "")
                         (cons 'scope scope)
                         (cons 'file-path path)
                         (cons 'type 'setting))
                   result))))))
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

(defun claude-gravity--group-capabilities-by-scope
    (skills agents commands rules hooks mcp-servers instructions settings plugins project-dir)
  "Group capabilities by scope: project, user, plugins.
Returns alist with keys: project, user, plugins.
PROJECT and USER values are alists with keys: rules, skills, agents,
commands, instructions, settings, mcp-servers, hooks, base-dir.
PLUGINS value is same format as from `--group-capabilities-by-source'.
PROJECT-DIR is the project root (for computing .claude/ path)."
  (let ((plugin-names (mapcar (lambda (p) (alist-get 'name p)) plugins))
        (claude-dir (expand-file-name ".claude" project-dir))
        (global-dir (expand-file-name "~/.claude"))
        ;; Per-scope buckets
        (proj-rules nil) (proj-skills nil) (proj-agents nil) (proj-commands nil)
        (proj-hooks nil) (proj-mcp nil) (proj-instructions nil) (proj-settings nil)
        (user-rules nil) (user-skills nil) (user-agents nil) (user-commands nil)
        (user-hooks nil) (user-mcp nil) (user-instructions nil) (user-settings nil)
        ;; Plugin buckets
        (plugin-skills-ht (make-hash-table :test 'equal))
        (plugin-agents-ht (make-hash-table :test 'equal))
        (plugin-commands-ht (make-hash-table :test 'equal))
        (plugin-rules-ht (make-hash-table :test 'equal))
        (plugin-hooks-ht (make-hash-table :test 'equal))
        (plugin-mcp-ht (make-hash-table :test 'equal)))
    ;; Initialize plugin buckets
    (dolist (plugin plugins)
      (let ((name (alist-get 'name plugin)))
        (puthash name nil plugin-skills-ht)
        (puthash name nil plugin-agents-ht)
        (puthash name nil plugin-commands-ht)
        (puthash name nil plugin-rules-ht)
        (puthash name nil plugin-hooks-ht)
        (puthash name nil plugin-mcp-ht)))
    ;; Classify each capability by scope
    (cl-flet ((classify (item plugin-ht proj-var user-var)
                (let ((scope (alist-get 'scope item)))
                  (cond
                   ((member scope plugin-names)
                    (puthash scope (cons item (gethash scope plugin-ht)) plugin-ht))
                   ((string= scope "project") (push item proj-var))
                   (t (push item user-var))))))
      ;; Classify all types
      (dolist (s skills)
        (let ((scope (alist-get 'scope s)))
          (cond ((member scope plugin-names)
                 (puthash scope (cons s (gethash scope plugin-skills-ht)) plugin-skills-ht))
                ((string= scope "project") (push s proj-skills))
                (t (push s user-skills)))))
      (dolist (a agents)
        (let ((scope (alist-get 'scope a)))
          (cond ((member scope plugin-names)
                 (puthash scope (cons a (gethash scope plugin-agents-ht)) plugin-agents-ht))
                ((string= scope "project") (push a proj-agents))
                (t (push a user-agents)))))
      (dolist (c commands)
        (let ((scope (alist-get 'scope c)))
          (cond ((member scope plugin-names)
                 (puthash scope (cons c (gethash scope plugin-commands-ht)) plugin-commands-ht))
                ((string= scope "project") (push c proj-commands))
                (t (push c user-commands)))))
      (dolist (r rules)
        (let ((scope (alist-get 'scope r)))
          (cond ((member scope plugin-names)
                 (puthash scope (cons r (gethash scope plugin-rules-ht)) plugin-rules-ht))
                ((string= scope "project") (push r proj-rules))
                (t (push r user-rules)))))
      (dolist (h hooks)
        (let ((scope (alist-get 'scope h)))
          (cond ((member scope plugin-names)
                 (puthash scope (cons h (gethash scope plugin-hooks-ht)) plugin-hooks-ht))
                ((string= scope "project") (push h proj-hooks))
                (t (push h user-hooks)))))
      (dolist (m mcp-servers)
        (let ((scope (alist-get 'scope m)))
          (cond ((member scope plugin-names)
                 (puthash scope (cons m (gethash scope plugin-mcp-ht)) plugin-mcp-ht))
                ((string= scope "project") (push m proj-mcp))
                (t (push m user-mcp)))))
      ;; Instructions and settings: classify by scope field
      (dolist (i instructions)
        (if (string= (alist-get 'scope i) "project")
            (push i proj-instructions)
          (push i user-instructions)))
      (dolist (s settings)
        (if (string= (alist-get 'scope s) "project")
            (push s proj-settings)
          (push s user-settings))))
    ;; Build plugin groups
    (let ((plugin-groups nil))
      (dolist (plugin plugins)
        (let* ((name (alist-get 'name plugin))
               (p-skills (nreverse (gethash name plugin-skills-ht)))
               (p-agents (nreverse (gethash name plugin-agents-ht)))
               (p-commands (nreverse (gethash name plugin-commands-ht)))
               (p-rules (nreverse (gethash name plugin-rules-ht)))
               (p-hooks (nreverse (gethash name plugin-hooks-ht)))
               (p-mcp (nreverse (gethash name plugin-mcp-ht)))
               (total (+ (length p-skills) (length p-agents) (length p-commands)
                         (length p-rules) (length p-hooks) (length p-mcp))))
          (when (> total 0)
            (push (list (cons 'name name)
                        (cons 'scope (alist-get 'scope plugin))
                        (cons 'install-path (alist-get 'install-path plugin))
                        (cons 'skills p-skills)
                        (cons 'agents p-agents)
                        (cons 'commands p-commands)
                        (cons 'rules p-rules)
                        (cons 'hooks p-hooks)
                        (cons 'mcp-servers p-mcp)
                        (cons 'total total))
                  plugin-groups))))
      (list
       (cons 'project
             (list (cons 'rules (nreverse proj-rules))
                   (cons 'skills (nreverse proj-skills))
                   (cons 'agents (nreverse proj-agents))
                   (cons 'commands (nreverse proj-commands))
                   (cons 'instructions (nreverse proj-instructions))
                   (cons 'settings (nreverse proj-settings))
                   (cons 'mcp-servers (nreverse proj-mcp))
                   (cons 'hooks (nreverse proj-hooks))
                   (cons 'base-dir claude-dir)))
       (cons 'user
             (list (cons 'rules (nreverse user-rules))
                   (cons 'skills (nreverse user-skills))
                   (cons 'agents (nreverse user-agents))
                   (cons 'commands (nreverse user-commands))
                   (cons 'instructions (nreverse user-instructions))
                   (cons 'settings (nreverse user-settings))
                   (cons 'mcp-servers (nreverse user-mcp))
                   (cons 'hooks (nreverse user-hooks))
                   (cons 'base-dir global-dir)))
       (cons 'plugins (nreverse plugin-groups))))))


(defun claude-gravity--capabilities-total-count (grouped-caps)
  "Return total count of capabilities from GROUPED-CAPS structure."
  (let ((total 0))
    (dolist (plugin (alist-get 'plugins grouped-caps))
      (cl-incf total (alist-get 'total plugin)))
    (dolist (_ (alist-get 'standalone-skills grouped-caps)) (cl-incf total))
    (dolist (_ (alist-get 'standalone-agents grouped-caps)) (cl-incf total))
    (dolist (_ (alist-get 'standalone-commands grouped-caps)) (cl-incf total))
    (dolist (_ (alist-get 'standalone-rules grouped-caps)) (cl-incf total))
    (dolist (_ (alist-get 'standalone-mcp-servers grouped-caps)) (cl-incf total))
    (dolist (_ (alist-get 'standalone-hooks grouped-caps)) (cl-incf total))
    (dolist (_ (alist-get 'standalone-instructions grouped-caps)) (cl-incf total))
    (dolist (_ (alist-get 'standalone-settings grouped-caps)) (cl-incf total))
    total))


(defun claude-gravity--group-capabilities-by-source
    (skills agents commands rules hooks mcp-servers instructions settings plugins)
  "Group capabilities by source: plugin vs. standalone.
Returns alist with keys: plugins, standalone-skills, standalone-agents,
standalone-commands, standalone-rules, standalone-mcp-servers, standalone-hooks,
standalone-instructions, standalone-settings.
PLUGINS is list of plugin metadata to identify which caps belong to plugins.
INSTRUCTIONS and SETTINGS are standalone-only (never grouped under plugins)."
  (let ((plugin-names (mapcar (lambda (p) (alist-get 'name p)) plugins))
        (plugin-skills (make-hash-table :test 'equal))
        (plugin-agents (make-hash-table :test 'equal))
        (plugin-commands (make-hash-table :test 'equal))
        (plugin-rules (make-hash-table :test 'equal))
        (plugin-hooks (make-hash-table :test 'equal))
        (plugin-mcp (make-hash-table :test 'equal))
        (standalone-skills nil)
        (standalone-agents nil)
        (standalone-commands nil)
        (standalone-rules nil)
        (standalone-hooks nil)
        (standalone-mcp-servers nil))
    ;; Initialize plugin buckets
    (dolist (plugin plugins)
      (let ((name (alist-get 'name plugin)))
        (puthash name nil plugin-skills)
        (puthash name nil plugin-agents)
        (puthash name nil plugin-commands)
        (puthash name nil plugin-rules)
        (puthash name nil plugin-hooks)
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
    ;; Categorize rules
    (dolist (rule rules)
      (let ((scope (alist-get 'scope rule)))
        (if (member scope plugin-names)
            (puthash scope (cons rule (gethash scope plugin-rules)) plugin-rules)
          (push rule standalone-rules))))
    ;; Categorize hooks
    (dolist (hook hooks)
      (let ((scope (alist-get 'scope hook)))
        (if (member scope plugin-names)
            (puthash scope (cons hook (gethash scope plugin-hooks)) plugin-hooks)
          (push hook standalone-hooks))))
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
               (p-rules (nreverse (gethash name plugin-rules)))
               (p-hooks (nreverse (gethash name plugin-hooks)))
               (p-mcp (nreverse (gethash name plugin-mcp)))
               (total (+ (length p-skills) (length p-agents) (length p-commands)
                         (length p-rules) (length p-hooks) (length p-mcp))))
          (when (> total 0)
            (push (list (cons 'name name)
                        (cons 'scope (alist-get 'scope plugin))
                        (cons 'install-path (alist-get 'install-path plugin))
                        (cons 'skills p-skills)
                        (cons 'agents p-agents)
                        (cons 'commands p-commands)
                        (cons 'rules p-rules)
                        (cons 'hooks p-hooks)
                        (cons 'mcp-servers p-mcp)
                        (cons 'total total))
                  plugin-groups))))
      (list (cons 'plugins (nreverse plugin-groups))
            (cons 'standalone-skills (nreverse standalone-skills))
            (cons 'standalone-agents (nreverse standalone-agents))
            (cons 'standalone-commands (nreverse standalone-commands))
            (cons 'standalone-rules (nreverse standalone-rules))
            (cons 'standalone-mcp-servers (nreverse standalone-mcp-servers))
            (cons 'standalone-hooks (nreverse standalone-hooks))
            (cons 'standalone-instructions instructions)
            (cons 'standalone-settings settings)))))


;;; Main Discovery Function

(defun claude-gravity--discover-project-capabilities (project-dir)
  "Discover all capabilities for PROJECT-DIR.
Returns alist with keys: plugins, standalone-skills, standalone-agents,
standalone-commands, standalone-rules, standalone-mcp-servers,
standalone-hooks, standalone-instructions, standalone-settings.
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
            (rules nil)
            (hooks nil)
            (mcp-servers nil)
            (instructions nil)
            (settings nil)
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
              (setq rules (nconc rules (claude-gravity--scan-rules claude-dir "project")))
            (error
             (claude-gravity--log 'warn "Failed to scan project rules in %s: %s" claude-dir scan-err)))
          (condition-case scan-err
              (setq hooks (nconc hooks (claude-gravity--scan-hooks-json claude-dir "project")))
            (error
             (claude-gravity--log 'warn "Failed to scan project hooks in %s: %s" claude-dir scan-err)))
          (condition-case scan-err
              (setq hooks (nconc hooks (claude-gravity--scan-settings-hooks
                                        (expand-file-name "settings.json" claude-dir) "project")))
            (error
             (claude-gravity--log 'warn "Failed to scan project settings hooks: %s" scan-err)))
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
              (setq rules (nconc rules (claude-gravity--scan-rules global-dir "global")))
            (error
             (claude-gravity--log 'warn "Failed to scan global rules: %s" scan-err)))
          (condition-case scan-err
              (setq hooks (nconc hooks (claude-gravity--scan-hooks-json global-dir "global")))
            (error
             (claude-gravity--log 'warn "Failed to scan global hooks: %s" scan-err)))
          (condition-case scan-err
              (setq hooks (nconc hooks (claude-gravity--scan-settings-hooks
                                        (expand-file-name "settings.json" global-dir) "global")))
            (error
             (claude-gravity--log 'warn "Failed to scan global settings hooks: %s" scan-err)))
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
                  (setq rules (nconc rules (claude-gravity--scan-rules install-path plugin-name)))
                (error
                 (claude-gravity--log 'warn "Failed to scan plugin %s rules: %s" plugin-name scan-err)))
              (condition-case scan-err
                  (setq hooks (nconc hooks (claude-gravity--scan-hooks-json install-path plugin-name)))
                (error
                 (claude-gravity--log 'warn "Failed to scan plugin %s hooks: %s" plugin-name scan-err)))
              (condition-case scan-err
                  (setq mcp-servers (nconc mcp-servers (claude-gravity--scan-mcp-servers install-path plugin-name)))
                (error
                 (claude-gravity--log 'warn "Failed to scan plugin %s MCP servers: %s" plugin-name scan-err))))))

        ;; 4. Standalone-only: instruction files and settings
        (condition-case scan-err
            (setq instructions (claude-gravity--scan-instruction-files project-dir))
          (error
           (claude-gravity--log 'warn "Failed to scan instruction files: %s" scan-err)))
        (condition-case scan-err
            (setq settings (claude-gravity--scan-settings-files project-dir))
          (error
           (claude-gravity--log 'warn "Failed to scan settings files: %s" scan-err)))

        ;; Group by source
        (let ((result (claude-gravity--group-capabilities-by-source
                       skills agents commands rules hooks
                       mcp-servers instructions settings
                       (or plugins nil))))
          (puthash key (cons now result) claude-gravity--capabilities-cache)
          result)))))


(defun claude-gravity--discover-project-capabilities-by-scope (project-dir)
  "Discover all capabilities for PROJECT-DIR, grouped by scope.
Returns alist with keys: project, user, plugins.
Each scope has keys: rules, skills, agents, commands, instructions,
settings, mcp-servers, hooks, base-dir.
Results are cached for `claude-gravity--capabilities-ttl' seconds."
  (let* ((key (expand-file-name project-dir))
         (cached (gethash key claude-gravity--capabilities-by-scope-cache))
         (now (float-time)))
    (if (and cached
             (< (- now (car cached)) claude-gravity--capabilities-ttl))
        (cdr cached)
      ;; Reuse the same scanning code — duplicate the scan block
      (let ((skills nil) (agents nil) (commands nil) (rules nil)
            (hooks nil) (mcp-servers nil) (instructions nil) (settings nil)
            (claude-dir (expand-file-name ".claude" project-dir))
            (global-dir (expand-file-name "~/.claude"))
            (plugins nil))
        ;; Get plugins
        (condition-case scan-err
            (setq plugins (claude-gravity--plugins-for-project project-dir))
          (error
           (claude-gravity--log 'warn "Failed to scan plugins for %s: %s" project-dir scan-err)
           (setq plugins nil)))
        ;; 1. Project-local .claude/ directory
        (when (file-directory-p claude-dir)
          (condition-case scan-err
              (setq skills (nconc skills (claude-gravity--scan-skills claude-dir "project")))
            (error (claude-gravity--log 'warn "Scan project skills: %s" scan-err)))
          (condition-case scan-err
              (setq agents (nconc agents (claude-gravity--scan-agents claude-dir "project")))
            (error (claude-gravity--log 'warn "Scan project agents: %s" scan-err)))
          (condition-case scan-err
              (setq commands (nconc commands (claude-gravity--scan-commands claude-dir "project")))
            (error (claude-gravity--log 'warn "Scan project commands: %s" scan-err)))
          (condition-case scan-err
              (setq rules (nconc rules (claude-gravity--scan-rules claude-dir "project")))
            (error (claude-gravity--log 'warn "Scan project rules: %s" scan-err)))
          (condition-case scan-err
              (setq hooks (nconc hooks (claude-gravity--scan-hooks-json claude-dir "project")))
            (error (claude-gravity--log 'warn "Scan project hooks: %s" scan-err)))
          (condition-case scan-err
              (setq hooks (nconc hooks (claude-gravity--scan-settings-hooks
                                        (expand-file-name "settings.json" claude-dir) "project")))
            (error (claude-gravity--log 'warn "Scan project settings hooks: %s" scan-err)))
          (condition-case scan-err
              (setq mcp-servers (nconc mcp-servers (claude-gravity--scan-mcp-servers claude-dir "project")))
            (error (claude-gravity--log 'warn "Scan project MCP: %s" scan-err))))
        ;; 2. Global ~/.claude/ directory
        (when (file-directory-p global-dir)
          (condition-case scan-err
              (setq skills (nconc skills (claude-gravity--scan-skills global-dir "global")))
            (error (claude-gravity--log 'warn "Scan global skills: %s" scan-err)))
          (condition-case scan-err
              (setq agents (nconc agents (claude-gravity--scan-agents global-dir "global")))
            (error (claude-gravity--log 'warn "Scan global agents: %s" scan-err)))
          (condition-case scan-err
              (setq commands (nconc commands (claude-gravity--scan-commands global-dir "global")))
            (error (claude-gravity--log 'warn "Scan global commands: %s" scan-err)))
          (condition-case scan-err
              (setq rules (nconc rules (claude-gravity--scan-rules global-dir "global")))
            (error (claude-gravity--log 'warn "Scan global rules: %s" scan-err)))
          (condition-case scan-err
              (setq hooks (nconc hooks (claude-gravity--scan-hooks-json global-dir "global")))
            (error (claude-gravity--log 'warn "Scan global hooks: %s" scan-err)))
          (condition-case scan-err
              (setq hooks (nconc hooks (claude-gravity--scan-settings-hooks
                                        (expand-file-name "settings.json" global-dir) "global")))
            (error (claude-gravity--log 'warn "Scan global settings hooks: %s" scan-err)))
          (condition-case scan-err
              (setq mcp-servers (nconc mcp-servers (claude-gravity--scan-mcp-servers global-dir "global")))
            (error (claude-gravity--log 'warn "Scan global MCP: %s" scan-err))))
        ;; 3. Installed plugins
        (dolist (plugin plugins)
          (let ((install-path (alist-get 'install-path plugin))
                (plugin-name (alist-get 'name plugin)))
            (when (and install-path (file-directory-p install-path))
              (condition-case scan-err
                  (setq skills (nconc skills (claude-gravity--scan-skills install-path plugin-name)))
                (error (claude-gravity--log 'warn "Scan plugin %s skills: %s" plugin-name scan-err)))
              (condition-case scan-err
                  (setq agents (nconc agents (claude-gravity--scan-agents install-path plugin-name)))
                (error (claude-gravity--log 'warn "Scan plugin %s agents: %s" plugin-name scan-err)))
              (condition-case scan-err
                  (setq commands (nconc commands (claude-gravity--scan-commands install-path plugin-name)))
                (error (claude-gravity--log 'warn "Scan plugin %s commands: %s" plugin-name scan-err)))
              (condition-case scan-err
                  (setq rules (nconc rules (claude-gravity--scan-rules install-path plugin-name)))
                (error (claude-gravity--log 'warn "Scan plugin %s rules: %s" plugin-name scan-err)))
              (condition-case scan-err
                  (setq hooks (nconc hooks (claude-gravity--scan-hooks-json install-path plugin-name)))
                (error (claude-gravity--log 'warn "Scan plugin %s hooks: %s" plugin-name scan-err)))
              (condition-case scan-err
                  (setq mcp-servers (nconc mcp-servers (claude-gravity--scan-mcp-servers install-path plugin-name)))
                (error (claude-gravity--log 'warn "Scan plugin %s MCP: %s" plugin-name scan-err))))))
        ;; 4. Instructions and settings
        (condition-case scan-err
            (setq instructions (claude-gravity--scan-instruction-files project-dir))
          (error (claude-gravity--log 'warn "Scan instructions: %s" scan-err)))
        (condition-case scan-err
            (setq settings (claude-gravity--scan-settings-files project-dir))
          (error (claude-gravity--log 'warn "Scan settings: %s" scan-err)))
        ;; Group by scope
        (let ((result (claude-gravity--group-capabilities-by-scope
                       skills agents commands rules hooks
                       mcp-servers instructions settings
                       (or plugins nil) project-dir)))
          (puthash key (cons now result) claude-gravity--capabilities-by-scope-cache)
          result)))))


(defun claude-gravity--invalidate-capabilities-cache (&optional project-dir)
  "Invalidate capabilities cache for PROJECT-DIR, or all if nil."
  (if project-dir
      (let ((key (expand-file-name project-dir)))
        (remhash key claude-gravity--capabilities-cache)
        (remhash key claude-gravity--capabilities-by-scope-cache)
        (remhash key claude-gravity--config-render-data))
    (clrhash claude-gravity--capabilities-cache)
    (clrhash claude-gravity--capabilities-by-scope-cache)
    (clrhash claude-gravity--config-render-data)))


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
