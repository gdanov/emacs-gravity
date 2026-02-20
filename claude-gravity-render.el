;;; claude-gravity-render.el --- Section renderers for Claude Gravity  -*- lexical-binding: t; -*-

;;; Code:

(require 'claude-gravity-core)
(require 'claude-gravity-faces)
(require 'claude-gravity-session)
(require 'claude-gravity-state)
(require 'claude-gravity-text)
(require 'claude-gravity-diff)


;;; Section renderers (used by per-session buffers)

(defun claude-gravity-insert-header (session)
  "Insert header section showing session slug and tool count from SESSION."
  (let* ((tool-count (claude-gravity--tree-total-tool-count session))
         (slug (claude-gravity--session-label session))
         (elapsed (claude-gravity--session-total-elapsed session))
         (usage (plist-get session :token-usage))
         (in-tokens (when usage
                      (+ (or (alist-get 'input_tokens usage) 0)
                         (or (alist-get 'cache_read_input_tokens usage) 0)
                         (or (alist-get 'cache_creation_input_tokens usage) 0))))
         (out-tokens (when usage (or (alist-get 'output_tokens usage) 0)))
         (width (max 40 (- (or (window-width) 80) 2)))
         (top-line (make-string width ?━)))
    (magit-insert-section (header)
      (insert (propertize top-line 'face 'claude-gravity-divider) "\n")
      (magit-insert-heading
        (concat
         (propertize "Structured Claude Session" 'face 'claude-gravity-header-title)
         (propertize (format "  %s" slug) 'face 'claude-gravity-slug)
         (let ((sid (plist-get session :session-id)))
           (when sid
             (propertize (format " %s" (substring sid 0 (min 7 (length sid))))
                         'face 'claude-gravity-detail-label)))
         (let ((tmux-name (gethash (plist-get session :session-id) claude-gravity--tmux-sessions)))
           (when tmux-name
             (propertize (format " [%s]" tmux-name) 'face 'claude-gravity-detail-label)))
         (propertize (format "  ◆ %d tools" tool-count) 'face 'claude-gravity-detail-label)
         (when elapsed
           (propertize (format "  ⏱ %s" (claude-gravity--format-elapsed elapsed))
                       'face 'claude-gravity-detail-label))
         (when (and in-tokens (> in-tokens 0))
           (propertize (format "  ↑%s ↓%s tokens"
                               (claude-gravity--format-token-count out-tokens)
                               (claude-gravity--format-token-count in-tokens))
                       'face 'claude-gravity-detail-label))))
      (insert (propertize top-line 'face 'claude-gravity-divider) "\n")
      (insert "\n"))))


(defun claude-gravity--format-turn-tokens (turn-node)
  "Format per-turn token delta for TURN-NODE as '  ↓N ↑M' or empty string."
  (let ((in (alist-get 'token-in turn-node))
        (out (alist-get 'token-out turn-node)))
    (if (and in out (> (+ in out) 0))
        (propertize (format "  ↑%s ↓%s"
                            (claude-gravity--format-token-count out)
                            (claude-gravity--format-token-count in))
                    'face 'claude-gravity-detail-label)
      "")))


(defun claude-gravity--turn-counts-from-node (turn-node)
  "Format count string from TURN-NODE's pre-computed counts."
  (let ((parts nil)
        (tool-count (or (alist-get 'tool-count turn-node) 0))
        (agent-count (or (alist-get 'agent-count turn-node) 0))
        (task-count (length (alist-get 'tasks turn-node))))
    (when (> tool-count 0)
      (push (format "%d tools" tool-count) parts))
    (when (> agent-count 0)
      (push (format "%da" agent-count) parts))
    (when (> task-count 0)
      (push (format "%dt" task-count) parts))
    (if parts
        (propertize (format "[%s]" (string-join parts " "))
                    'face 'claude-gravity-detail-label)
      "")))


(defun claude-gravity--insert-turn-children-from-tree (turn-node)
  "Insert response cycles, agent branches, and tasks from TURN-NODE's tree.
Cycles are pre-computed — no dedup or grouping needed."
  (let* ((cycles-tl (alist-get 'cycles turn-node))
         (cycles (when cycles-tl (claude-gravity--tlist-items cycles-tl)))
         (n-cycles (length cycles))
         (cycle-idx 0)
         (tasks (alist-get 'tasks turn-node)))
    ;; Render cycles
    (when (> n-cycles 0)
      (dolist (cycle cycles)
        (let* ((is-last (= cycle-idx (1- n-cycles)))
               (cycle-tools (claude-gravity--tlist-items (alist-get 'tools cycle)))
               (all-done (cl-every (lambda (item) (equal (alist-get 'status item) "done"))
                                   cycle-tools))
               (should-collapse (and (not is-last) all-done))
               (athink (alist-get 'thinking cycle))
               (atext (alist-get 'text cycle))
               (athink (when athink (string-trim athink)))
               (atext (when atext (string-trim atext)))
               (split (when (and atext (not (string-empty-p atext)))
                        (claude-gravity--split-margin-text atext 'claude-gravity-assistant-text)))
               (tools-label (format "%d tool%s" (length cycle-tools)
                                    (if (= (length cycle-tools) 1) "" "s"))))
          ;; Thinking stays outside the section (always visible)
          (when (and athink (not (string-empty-p athink)))
            (let* ((indent (or (* (claude-gravity--section-depth) claude-gravity--indent-step) 0))
                   (margin (propertize (concat claude-gravity--margin-char " ")
                                      'face 'claude-gravity-thinking))
                   (prefix (concat (make-string indent ?\s) margin)))
              (insert prefix (propertize "Thinking..." 'face 'claude-gravity-thinking) "\n")
              (claude-gravity--insert-wrapped athink (+ indent 4) 'claude-gravity-thinking)))
          ;; Response cycle section
          (magit-insert-section (response-cycle cycle-idx should-collapse)
            (magit-insert-heading
              (if split
                  (car split)
                (format "%s%s%s"
                        (claude-gravity--indent)
                        (propertize (concat claude-gravity--margin-char " ")
                                    'face 'claude-gravity-detail-label)
                        (propertize tools-label 'face 'claude-gravity-detail-label))))
            ;; Body: remaining assistant text lines
            (when (cdr split)
              (insert (cdr split))
              (oset magit-insert-section--current content
                    (if magit-section-inhibit-markers (point) (point-marker))))
            ;; Body: tool count summary (when heading was assistant text)
            (when split
              (insert (format "%s%s%s\n"
                              (claude-gravity--indent)
                              (propertize (concat claude-gravity--margin-char " ")
                                          'face 'claude-gravity-detail-label)
                              (propertize tools-label 'face 'claude-gravity-detail-label))))
            ;; Tools — first tool's context already rendered as heading
            (when cycle-tools
              (claude-gravity--insert-tool-item-from-tree (car cycle-tools))
              (dolist (item (cdr cycle-tools))
                (claude-gravity--insert-tool-context item)
                (claude-gravity--insert-tool-item-from-tree item))))
          ;; Separator between cycles
          (unless is-last
            (claude-gravity--turn-separator)))
        (cl-incf cycle-idx)))
    ;; Tasks subsection at the end
    (when (and tasks (> (length tasks) 0))
      (let ((sorted (sort (copy-sequence tasks)
                          (lambda (a b)
                            (< (claude-gravity--task-sort-key (alist-get 'status a))
                               (claude-gravity--task-sort-key (alist-get 'status b)))))))
        (let ((completed (cl-count-if (lambda (tk) (equal (alist-get 'status tk) "completed")) sorted))
              (total (length sorted)))
          (magit-insert-section (turn-tasks nil t)
            (magit-insert-heading
              (format "%sTasks (%d/%d)" (claude-gravity--indent) completed total))
            (dolist (task sorted)
              (claude-gravity--insert-task-item task))))))))


(defun claude-gravity--insert-tool-item-from-tree (item)
  "Insert tool ITEM using tree-based agent lookup.
If the tool has an 'agent pointer (from bidirectional link), renders as agent branch."
  (let* ((name (alist-get 'name item))
         (status (alist-get 'status item))
         (input (alist-get 'input item))
         (result (alist-get 'result item))
         (done-p (equal status "done"))
         (error-p (equal status "error"))
         (agent (alist-get 'agent item)))
    ;; Skip AskUserQuestion (shown as prompt entries)
    (unless (equal name "AskUserQuestion")
      (if (and agent (> (claude-gravity--tlist-length (alist-get 'cycles agent)) 0))
          ;; Agent with tools → render as agent branch
          (claude-gravity--insert-agent-branch-from-tree item agent)
        ;; Regular tool or agent without tools
        (let* ((indicator (propertize (cond (error-p "[!]")
                                            (done-p "[x]")
                                            (t "[/]"))
                                      'face (cond (error-p 'claude-gravity-tool-error)
                                                  (done-p 'claude-gravity-tool-done)
                                                  (t 'claude-gravity-tool-running))))
               (tool-face (propertize (or name "?") 'face 'claude-gravity-tool-name))
               (summary (claude-gravity--tool-summary name input))
               (desc (claude-gravity--tool-description input))
               ;; Model badge: show when different from session default
               (tool-model (alist-get 'model item))
               (req-model (alist-get 'requested_model item))
               (session-model
                (when claude-gravity--buffer-session-id
                  (let ((s (gethash claude-gravity--buffer-session-id
                                    claude-gravity--sessions)))
                    (when s (plist-get s :model-id)))))
               (display-model (or req-model
                                  (and tool-model
                                       (not (equal tool-model session-model))
                                       tool-model)))
               (model-badge
                (let ((short (claude-gravity--short-model-name display-model)))
                  (if short
                      (propertize (format "  %s" short)
                                  'face 'claude-gravity-detail-label)
                    "")))
               (agent-suffix
                (if agent
                    (let* ((atype (alist-get 'type agent))
                           (aid (alist-get 'agent_id agent))
                           (short-id (if (and aid (> (length aid) 7))
                                         (substring aid 0 7)
                                       (or aid "?")))
                           (adur (alist-get 'duration agent))
                           (astatus (alist-get 'status agent))
                           (dur-str (if (and (equal astatus "done") adur)
                                        (format "  %s" (claude-gravity--format-duration adur))
                                      "")))
                      (format "  %s %s (%s)%s%s"
                              (propertize "→" 'face 'claude-gravity-detail-label)
                              (propertize (or atype "?") 'face 'claude-gravity-tool-name)
                              (propertize short-id 'face 'claude-gravity-detail-label)
                              model-badge
                              dur-str))
                  ""))
               (agent-icon (if agent "🤖 " ""))
               (tool-use-id (alist-get 'tool_use_id item))
               (section-start (point)))
          (magit-insert-section (tool tool-use-id t)
            (magit-insert-heading
              (if desc
                  (format "%s%s%s %s\n%s%s%s%s"
                          (claude-gravity--indent)
                          agent-icon
                          indicator
                          (propertize desc 'face 'claude-gravity-tool-description)
                          (claude-gravity--indent 2)
                          (propertize (claude-gravity--tool-signature name input)
                                      'face 'claude-gravity-tool-signature)
                          (if (string-empty-p agent-suffix) model-badge "")
                          agent-suffix)
                (format "%s%s%s %s  %s%s%s"
                        (claude-gravity--indent)
                        agent-icon
                        indicator
                        tool-face
                        (propertize summary 'face 'claude-gravity-detail-label)
                        (if (string-empty-p agent-suffix) model-badge "")
                        agent-suffix)))
            ;; Show permission-format signature in detail
            (unless desc
              (let ((sig (claude-gravity--tool-signature name input)))
                (claude-gravity--insert-wrapped sig nil 'claude-gravity-tool-signature)))
            (insert "\n")
            (claude-gravity--insert-tool-detail name input result status)
            ;; Post-tool text and thinking
            (let ((post-think (alist-get 'post_thinking item))
                  (post-text (alist-get 'post_text item)))
              (when (and post-think (not (string-empty-p post-think)))
                (claude-gravity--insert-wrapped-with-margin post-think nil 'claude-gravity-thinking)
                (insert "\n"))
              (when (and post-text (not (string-empty-p post-text)))
                (claude-gravity--insert-wrapped-with-margin post-text nil 'claude-gravity-assistant-text)
                (insert "\n")))
            ;; Agent detail (transcript, etc.) in expanded view
            (when agent
              (let ((tp (alist-get 'transcript_path agent))
                    (parsed (alist-get 'transcript_parsed agent)))
                (when parsed
                  (let ((prompt (alist-get 'transcript_prompt agent))
                        (model (alist-get 'transcript_model agent))
                        (tc (alist-get 'transcript_tool_count agent)))
                    (when (and prompt (not (string-empty-p prompt)))
                      (claude-gravity--insert-label "Agent task: ")
                      (claude-gravity--insert-wrapped prompt nil))
                    (when (and model (not (string-empty-p model)))
                      (claude-gravity--insert-label "Model: ")
                      (insert model "\n"))
                    (when (and tc (> tc 0))
                      (claude-gravity--insert-label "Agent tools: ")
                      (insert (format "%d" tc) "\n"))))
                (when tp
                  (claude-gravity--insert-label "Transcript: ")
                  (claude-gravity--insert-wrapped tp nil 'claude-gravity-detail-label)))))
          ;; Apply background tint to running tools
          (unless done-p
            (add-face-text-property section-start (point) 'claude-gravity-running-bg)))))))


(defun claude-gravity--insert-agent-branch-from-tree (item agent &optional depth)
  "Insert ITEM (a Task tool) as a sub-branch with AGENT's nested cycles from tree.
DEPTH tracks nesting level for background tint."
  (let* ((name (alist-get 'name item))
         (status (alist-get 'status item))
         (input (alist-get 'input item))
         (done-p (equal status "done"))
         (error-p (equal status "error"))
         (agent-done-p (equal (alist-get 'status agent) "done"))
         (indicator (propertize (cond (error-p "[!]")
                                      (done-p "[x]")
                                      (t "[/]"))
                                'face (cond (error-p 'claude-gravity-tool-error)
                                            (done-p 'claude-gravity-tool-done)
                                            (t 'claude-gravity-tool-running))))
         (desc (claude-gravity--tool-description input))
         (atype (alist-get 'type agent))
         (aid (alist-get 'agent_id agent))
         (short-id (if (and aid (> (length aid) 7))
                       (substring aid 0 7)
                     (or aid "?")))
         (adur (alist-get 'duration agent))
         (dur-str (if (and agent-done-p adur)
                      (format "  %s" (claude-gravity--format-duration adur))
                    ""))
         ;; Model badge for agent branch
         (tool-model (alist-get 'model item))
         (req-model (alist-get 'requested_model item))
         (session-model
          (when claude-gravity--buffer-session-id
            (let ((s (gethash claude-gravity--buffer-session-id
                              claude-gravity--sessions)))
              (when s (plist-get s :model-id)))))
         (display-model (or req-model
                            (and tool-model
                                 (not (equal tool-model session-model))
                                 tool-model)))
         (model-badge
          (let ((short (claude-gravity--short-model-name display-model)))
            (if short
                (propertize (format "  %s" short)
                            'face 'claude-gravity-detail-label)
              "")))
         (agent-suffix (format "  %s %s (%s)%s%s"
                               (propertize "→" 'face 'claude-gravity-detail-label)
                               (propertize (or atype "?") 'face 'claude-gravity-tool-name)
                               (propertize short-id 'face 'claude-gravity-detail-label)
                               model-badge
                               dur-str))
         (agent-cycles (claude-gravity--tlist-items (alist-get 'cycles agent)))
         (collapsed (and agent-done-p (not (null agent-cycles))))
         (tool-use-id (alist-get 'tool_use_id item)))
    (let ((section-start (point)))
      (magit-insert-section (tool tool-use-id collapsed)
        (magit-insert-heading
          (if desc
              (format "%s🤖 %s %s\n%s%s%s"
                      (claude-gravity--indent) indicator
                      (propertize desc 'face 'claude-gravity-tool-description)
                      (claude-gravity--indent 2)
                      (propertize (claude-gravity--tool-signature name input)
                                  'face 'claude-gravity-tool-signature)
                      agent-suffix)
            (format "%s🤖 %s %s  %s%s"
                    (claude-gravity--indent) indicator
                    (propertize (or name "?") 'face 'claude-gravity-tool-name)
                    (propertize (claude-gravity--tool-summary name input)
                                'face 'claude-gravity-detail-label)
                    agent-suffix)))
        ;; Render agent's cycles with agent-specific styling
        (let ((claude-gravity--margin-face 'claude-gravity-agent-margin)
              (claude-gravity--agent-depth (1+ (or depth claude-gravity--agent-depth 0)))
              (body-start (point)))
          (when agent-cycles
            (let ((n-cycles (length agent-cycles))
                  (cycle-idx 0))
              (dolist (cycle agent-cycles)
                (let* ((is-last (= cycle-idx (1- n-cycles)))
                       (cycle-tools (claude-gravity--tlist-items (alist-get 'tools cycle)))
                       (all-done (cl-every (lambda (ti) (equal (alist-get 'status ti) "done"))
                                           cycle-tools))
                       (should-collapse (and (not is-last) all-done))
                       (athink (alist-get 'thinking cycle))
                       (atext (alist-get 'text cycle))
                       (athink (when athink (string-trim athink)))
                       (atext (when atext (string-trim atext)))
                       (split (when (and atext (not (string-empty-p atext)))
                                (claude-gravity--split-margin-text atext 'claude-gravity-assistant-text)))
                       (tools-label (format "%d tool%s" (length cycle-tools)
                                            (if (= (length cycle-tools) 1) "" "s"))))
                  ;; Thinking
                  (when (and athink (not (string-empty-p athink)))
                    (let* ((indent (or (* (claude-gravity--section-depth) claude-gravity--indent-step) 0))
                           (margin (propertize (concat claude-gravity--margin-char " ")
                                              'face 'claude-gravity-thinking))
                           (prefix (concat (make-string indent ?\s) margin)))
                      (insert prefix (propertize "Thinking..." 'face 'claude-gravity-thinking) "\n")
                      (claude-gravity--insert-wrapped athink (+ indent 4) 'claude-gravity-thinking)))
                  ;; Response cycle section
                  (magit-insert-section (response-cycle cycle-idx should-collapse)
                    (magit-insert-heading
                      (if split
                          (car split)
                        (format "%s%s%s"
                                (claude-gravity--indent)
                                (propertize (concat claude-gravity--margin-char " ")
                                            'face 'claude-gravity-detail-label)
                                (propertize tools-label 'face 'claude-gravity-detail-label))))
                    (when (cdr split)
                      (insert (cdr split))
                      (oset magit-insert-section--current content
                            (if magit-section-inhibit-markers (point) (point-marker))))
                    (when split
                      (insert (format "%s%s%s\n"
                                      (claude-gravity--indent)
                                      (propertize (concat claude-gravity--margin-char " ")
                                                  'face 'claude-gravity-detail-label)
                                      (propertize tools-label 'face 'claude-gravity-detail-label))))
                    (when cycle-tools
                      (claude-gravity--insert-tool-item-from-tree (car cycle-tools))
                      (dolist (ti (cdr cycle-tools))
                        (claude-gravity--insert-tool-context ti)
                        (claude-gravity--insert-tool-item-from-tree ti))))
                  (unless is-last
                    (claude-gravity--turn-separator)))
                (cl-incf cycle-idx))))
          ;; Agent's trailing summary text
          (let ((agent-stop-think (alist-get 'stop_thinking agent))
                (agent-stop-text (alist-get 'stop_text agent)))
            (when (and agent-stop-think (stringp agent-stop-think)
                       (not (string-empty-p agent-stop-think)))
              (claude-gravity--insert-wrapped-with-margin
               agent-stop-think nil 'claude-gravity-thinking))
            (when (and agent-stop-text (stringp agent-stop-text)
                       (not (string-empty-p agent-stop-text)))
              (claude-gravity--insert-wrapped-with-margin
               agent-stop-text nil 'claude-gravity-assistant-text)))
          ;; If no agent tools yet but agent is running, show status
          (when (and (not agent-cycles) (not agent-done-p))
            (insert (format "%s%s\n"
                            (claude-gravity--indent)
                            (propertize "Agent running..." 'face 'claude-gravity-detail-label))))
          ;; Apply agent background tint
          (add-face-text-property body-start (point)
                                  (if (> claude-gravity--agent-depth 1)
                                      'claude-gravity-agent-nested-bg
                                    'claude-gravity-agent-bg))))
      ;; Apply background tint to running tools
      (unless done-p
        (add-face-text-property section-start (point) 'claude-gravity-running-bg)))))


(defun claude-gravity--insert-stop-text (turn-node)
  "Insert trailing stop_text/stop_thinking from TURN-NODE.
Falls back to prompt entry for pre-fix data.
Deduplicates against the last tool's post_text/post_thinking."
  (let* ((prompt-entry (alist-get 'prompt turn-node))
         (stop-think (or (alist-get 'stop_thinking turn-node)
                         (and (listp prompt-entry)
                              (alist-get 'stop_thinking prompt-entry))))
         (stop-text (or (alist-get 'stop_text turn-node)
                        (and (listp prompt-entry)
                             (alist-get 'stop_text prompt-entry)))))
    ;; Find the last tool across all cycles for dedup
    (when (or stop-think stop-text)
      (let* ((last-cycle (claude-gravity--tlist-last-item (alist-get 'cycles turn-node)))
             (last-tool (when last-cycle
                          (claude-gravity--tlist-last-item (alist-get 'tools last-cycle)))))
        (let ((last-post-text (when last-tool (alist-get 'post_text last-tool)))
              (last-post-think (when last-tool (alist-get 'post_thinking last-tool))))
          ;; Dedup stop_thinking vs last tool's post_thinking
          (when (and stop-think last-post-think
                     (claude-gravity--text-subsumes-p stop-think last-post-think))
            (if (>= (length last-post-think) (length stop-think))
                (setq stop-think nil)
              (setf (alist-get 'post_thinking last-tool) nil)))
          ;; Dedup stop_text vs last tool's post_text
          (when (and stop-text last-post-text
                     (claude-gravity--text-subsumes-p stop-text last-post-text))
            (if (>= (length last-post-text) (length stop-text))
                (setq stop-text nil)
              (setf (alist-get 'post_text last-tool) nil)))
          (let ((has-think (and stop-think (not (string-empty-p stop-think))))
                (has-text (and stop-text (not (string-empty-p stop-text)))))
            (when (or has-think has-text)
              (magit-insert-section (stop-message (alist-get 'turn-number turn-node) t)
                (magit-insert-heading
                  (format "%s%s\n"
                          (claude-gravity--indent)
                          (propertize (concat claude-gravity--margin-char " ⏹ ") 'face 'claude-gravity-agent-stop-text)))
                (when has-think
                  (claude-gravity--insert-wrapped-with-margin
                   stop-think nil 'claude-gravity-thinking))
                (when has-text
                  (claude-gravity--insert-wrapped-with-margin
                   stop-text nil 'claude-gravity-agent-stop-text))))))))))


(defun claude-gravity--insert-task-item (task)
  "Insert a single TASK as a magit-section."
  (let* ((task-id (alist-get 'taskId task))
         (subject (or (alist-get 'subject task) "(no subject)"))
         (status (or (alist-get 'status task) "pending"))
         (active-form (alist-get 'activeForm task))
         (checkbox (pcase status
                     ("completed"
                      (propertize "[x]" 'face 'claude-gravity-task-done))
                     ("in_progress"
                      (propertize "[/]" 'face 'claude-gravity-task-in-progress))
                     (_
                      (propertize "[ ]" 'face 'claude-gravity-task-pending))))
         (suffix (if (and (equal status "in_progress") active-form)
                     (concat "  " (propertize active-form 'face 'claude-gravity-task-active-form))
                   "")))
    (magit-insert-section (task task-id)
      (insert (format "%s%s %s%s\n" (claude-gravity--indent) checkbox subject suffix)))))


(defun claude-gravity--insert-agent-completions (agents)
  "Insert top-level completion messages for done agents.
AGENTS is the list of agents for this turn.
Shows stop_thinking and stop_text for all agents with status='done'."
  (dolist (agent agents)
    (when (equal (alist-get 'status agent) "done")
      (let ((agent-type (alist-get 'type agent))
            (stop-think (alist-get 'stop_thinking agent))
            (stop-text (alist-get 'stop_text agent)))
        ;; Render thinking if present
        (when (and stop-think (stringp stop-think) (not (string-empty-p stop-think)))
          (claude-gravity--insert-wrapped-with-margin
           stop-think nil 'claude-gravity-thinking))
        ;; Render text with agent type label
        (when (and stop-text (stringp stop-text) (not (string-empty-p stop-text)))
          (let ((label (format "Agent \"%s\" completed:"
                              (or agent-type "unknown"))))
            (insert (claude-gravity--indent)
                    (propertize label 'face 'claude-gravity-detail-label)
                    "\n"))
          (claude-gravity--insert-wrapped-with-margin
           stop-text nil 'claude-gravity-agent-stop-text))))))


(defun claude-gravity-insert-turns (session)
  "Insert unified turns section for SESSION.
Iterates the :turns tree directly — no grouping or hash construction needed."
  (let* ((turns-tl (plist-get session :turns))
         (turn-nodes (when turns-tl (claude-gravity--tlist-items turns-tl)))
         (current-turn (or (plist-get session :current-turn) 0)))
    ;; Render if there are any turns with content
    (when (cl-some (lambda (tn)
                     (or (> (or (alist-get 'tool-count tn) 0) 0)
                         (> (or (alist-get 'agent-count tn) 0) 0)
                         (alist-get 'tasks tn)
                         (alist-get 'prompt tn)))
                   turn-nodes)
      (magit-insert-section (turns nil t)
        (magit-insert-heading
          (claude-gravity--section-divider (format "Turns (%d)" current-turn)))
        (let ((prev-turn-rendered nil))
          (dolist (turn-node turn-nodes)
            (let* ((turn-num (alist-get 'turn-number turn-node))
                   (prompt-entry (alist-get 'prompt turn-node))
                   (turn-agents (claude-gravity--tlist-items (alist-get 'agents turn-node)))
                   (turn-tasks (alist-get 'tasks turn-node))
                   (tool-count (or (alist-get 'tool-count turn-node) 0))
                   (agent-count (or (alist-get 'agent-count turn-node) 0))
                   (is-current (= turn-num current-turn))
                   (has-content (or (> tool-count 0) (> agent-count 0)
                                    turn-tasks prompt-entry)))
              (when has-content
                (if (= turn-num 0)
                    ;; Turn 0: pre-prompt activity
                    (progn
                      (magit-insert-section (turn 0 t)
                        (magit-insert-heading
                          (format "%s  %s"
                                  (propertize "Pre-prompt activity" 'face 'claude-gravity-detail-label)
                                  (claude-gravity--turn-counts-from-node turn-node)))
                        (claude-gravity--insert-turn-children-from-tree turn-node))
                      (claude-gravity--insert-stop-text turn-node))
                  ;; Normal turns
                  (let* ((prompt-text (when prompt-entry
                                        (claude-gravity--prompt-text prompt-entry)))
                         (is-question (when (listp prompt-entry)
                                        (eq (alist-get 'type prompt-entry) 'question)))
                         (is-phase-boundary (when (listp prompt-entry)
                                              (eq (alist-get 'type prompt-entry) 'phase-boundary)))
                         (elapsed (when (listp prompt-entry)
                                    (or (alist-get 'elapsed prompt-entry)
                                        (when (alist-get 'submitted prompt-entry)
                                          (float-time (time-subtract (current-time)
                                                                     (alist-get 'submitted prompt-entry)))))))
                         (elapsed-str (claude-gravity--format-elapsed elapsed))
                         (indicator (cond (is-phase-boundary
                                           (propertize "→" 'face 'claude-gravity-phase-boundary))
                                          (is-question
                                           (propertize "?" 'face 'claude-gravity-question))
                                          (t
                                           (propertize "❯" 'face 'claude-gravity-prompt))))
                         (counts (claude-gravity--turn-counts-from-node turn-node))
                         (answer (when is-question (alist-get 'answer prompt-entry)))
                         (answer-suffix (if answer
                                            (format "  → %s" (claude-gravity--truncate answer 40))
                                          ""))
                         (prompt-face (cond (is-phase-boundary 'claude-gravity-phase-boundary)
                                            (is-question 'claude-gravity-question)
                                            (t 'claude-gravity-prompt)))
                         (frozen (alist-get 'frozen turn-node)))
                    ;; Turn separator between turns
                    (when prev-turn-rendered
                      (claude-gravity--turn-separator))
                    (setq prev-turn-rendered t)
                    ;; Full prompt text outside the collapsible section
                    (when prompt-text
                      (let* ((indent (claude-gravity--indent))
                             (cont-indent (+ (length indent) 2)))
                        (insert (format "%s%s " indent indicator))
                        (let ((start (point))
                              (fill-column (max 40 (- (or (window-width) 80) 2)))
                              (fill-prefix (make-string cont-indent ?\s)))
                          (insert prompt-text "\n")
                          (when (> (length prompt-text) (- fill-column cont-indent))
                            (fill-region start (point)))
                          ;; Prompt indicator (❯/?) already propertized; text stays default color.
                          )))
                    (if frozen
                        ;; Frozen turn: collapsed section with children
                        (let* ((stop (alist-get 'stop_text turn-node))
                               (summary (if (and stop (stringp stop)
                                                 (not (string-empty-p stop)))
                                            (claude-gravity--truncate
                                             (replace-regexp-in-string "\n" " " stop) 60)
                                          ""))
                               (summary-str (if (string-empty-p summary) ""
                                              (concat "  " (propertize summary 'face 'claude-gravity-detail-label)))))
                          (magit-insert-section (turn turn-num t)
                            (magit-insert-heading
                              (format "%s%s%s  %s%s%s"
                                      (claude-gravity--indent)
                                      (propertize counts 'face 'claude-gravity-detail-label)
                                      (propertize answer-suffix 'face 'claude-gravity-detail-label)
                                      (propertize elapsed-str 'face 'claude-gravity-detail-label)
                                      (claude-gravity--format-turn-tokens turn-node)
                                      summary-str))
                            (claude-gravity--insert-turn-children-from-tree turn-node)
                            (claude-gravity--insert-agent-completions turn-agents))
                          (claude-gravity--insert-stop-text turn-node))
                      ;; Active turn: full render
                      (magit-insert-section (turn turn-num (not is-current))
                        (magit-insert-heading
                          (format "%s%s%s  %s%s"
                                  (claude-gravity--indent)
                                  (propertize counts 'face 'claude-gravity-detail-label)
                                  (propertize answer-suffix 'face 'claude-gravity-detail-label)
                                  (propertize elapsed-str 'face 'claude-gravity-detail-label)
                                  (claude-gravity--format-turn-tokens turn-node)))
                        ;; Children from tree
                        (claude-gravity--insert-turn-children-from-tree turn-node)
                        ;; Agent completions at top level
                        (claude-gravity--insert-agent-completions turn-agents))
                      ;; Stop message: top-level section (sibling of turn)
                      (claude-gravity--insert-stop-text turn-node))))))))
        (insert "\n")))))


(defun claude-gravity--task-sort-key (status)
  "Return sort key for task STATUS.  Lower = higher priority."
  (pcase status
    ("in_progress" 0)
    ("pending" 1)
    ("completed" 2)
    (_ 3)))


(defun claude-gravity--strip-system-xml (text)
  "Strip system-injected XML tags from TEXT.
Claude Code injects tags like <system-reminder>, <task-notification>,
<local-command-caveat>, <command-name>, <command-message>, <command-args>,
<local-command-stdout> into user prompt text.  Remove them and any
surrounding whitespace so the UI shows only the actual user prompt."
  (if (and text (string-match-p "<" text))
      (let ((result text)
            (tag-re "system-reminder\\|task-notification\\|local-command-caveat\\|command-name\\|command-message\\|command-args\\|local-command-stdout"))
        ;; Remove matched pairs: <tag ...>content</tag>
        ;; Use \\(?:.\\|\n\\)*? for multi-line non-greedy match
        (setq result (replace-regexp-in-string
                      (concat "<\\(" tag-re "\\)[^>]*>"
                              "\\(?:.\\|\n\\)*?"
                              "</\\1>")
                      "" result))
        ;; Remove any remaining unpaired/self-closing tags
        (setq result (replace-regexp-in-string
                      (concat "</?\\(" tag-re "\\)[^>]*>")
                      "" result))
        ;; Collapse excessive blank lines left behind
        (setq result (replace-regexp-in-string "\n\\{3,\\}" "\n\n" result))
        (setq result (string-trim result))
        (if (string-empty-p result) nil result))
    text))


(defun claude-gravity--prompt-text (prompt-entry)
  "Extract text from PROMPT-ENTRY (alist or legacy string)."
  (if (listp prompt-entry)
      (or (alist-get 'text prompt-entry) "")
    (or prompt-entry "")))



(defun claude-gravity--parse-agent-transcript (path)
  "Parse agent transcript JSONL at PATH.
Returns alist with prompt, model, and tool-count.
The JSONL format uses top-level `type` (user/assistant) and nests
the message under `message` with `role`, `content`, and `model`."
  (when (and path (file-exists-p path))
    (condition-case err
        (let ((prompt nil)
              (model nil)
              (tool-count 0))
          (with-temp-buffer
            (insert-file-contents path)
            (goto-char (point-min))
            (while (not (eobp))
              (let ((line (buffer-substring-no-properties
                           (line-beginning-position) (line-end-position))))
                (when (> (length line) 0)
                  (condition-case nil
                      (let* ((obj (json-parse-string line :object-type 'alist :array-type 'array))
                             (entry-type (alist-get 'type obj))
                             (msg (alist-get 'message obj))
                             (content (when msg (alist-get 'content msg))))
                        ;; Extract prompt from first user entry
                        (when (and (equal entry-type "user") (not prompt))
                          (cond
                           ((and msg (stringp content))
                            (setq prompt (car (split-string content "\n" t))))
                           ((and msg (vectorp content))
                            (dotimes (i (length content))
                              (let ((block (aref content i)))
                                (when (and (not prompt)
                                           (equal (alist-get 'type block) "text"))
                                  (setq prompt (car (split-string
                                                     (alist-get 'text block) "\n" t)))))))))
                        ;; Extract model from first assistant entry
                        (when (and (equal entry-type "assistant") msg (not model))
                          (setq model (alist-get 'model msg)))
                        ;; Count tool_use blocks in assistant messages
                        (when (and (equal entry-type "assistant") msg (vectorp content))
                          (dotimes (i (length content))
                            (let ((block (aref content i)))
                              (when (equal (alist-get 'type block) "tool_use")
                                (cl-incf tool-count))))))
                    (error nil))))
              (forward-line 1)))
          (list (cons 'prompt (or prompt ""))
                (cons 'model (or model ""))
                (cons 'tool-count tool-count)))
      (error
       (claude-gravity--log 'error "Claude Gravity: failed to parse transcript %s: %s" path err)
       nil))))


(defun claude-gravity-view-agent-transcript ()
  "Parse and display transcript for the agent at point."
  (interactive)
  (let ((section (magit-current-section)))
    (when section
      (let ((val (oref section value)))
        (when (and val (listp val) (alist-get 'agent_id val))
          (let ((tp (alist-get 'transcript_path val))
                (agent-id (alist-get 'agent_id val)))
            (if (not tp)
                (claude-gravity--log 'debug "No transcript path for this agent")
              (if (alist-get 'transcript_parsed val)
                  ;; Already parsed, just refresh
                  (claude-gravity-refresh)
                ;; Parse and store into the session's agent alist directly.
                ;; We must look up the agent in session data because setf on
                ;; alist-get with new keys only rebinds the local variable.
                (let* ((session (claude-gravity--get-session
                                 claude-gravity--buffer-session-id))
                       (agent (when session
                                (claude-gravity--find-agent session agent-id)))
                       (info (claude-gravity--parse-agent-transcript tp)))
                  (when (and agent info)
                    (setf (alist-get 'transcript_prompt agent)
                          (alist-get 'prompt info))
                    (setf (alist-get 'transcript_model agent)
                          (alist-get 'model info))
                    (setf (alist-get 'transcript_tool_count agent)
                          (alist-get 'tool-count info))
                    (setf (alist-get 'transcript_parsed agent) t))
                  (claude-gravity-refresh))))))))))



(defun claude-gravity-open-agent-transcript ()
  "Open the raw transcript JSONL file for the agent at point."
  (interactive)
  (let ((section (magit-current-section)))
    (when section
      (let ((val (oref section value)))
        (when (and val (listp val) (alist-get 'agent_id val))
          (let ((tp (alist-get 'transcript_path val)))
            (if (and tp (file-exists-p tp))
                (find-file tp)
              (claude-gravity--log 'debug "No transcript file available"))))))))



(defun claude-gravity-insert-files (session)
  "Insert files section for SESSION."
  (let ((files-ht (plist-get session :files)))
    (when (and files-ht (> (hash-table-count files-ht) 0))
      (let ((file-list nil))
        ;; Collect into list for sorting
        (maphash (lambda (path entry)
                   (push (list path
                               (alist-get 'ops entry)
                               (alist-get 'last-touched entry))
                         file-list))
                 files-ht)
        ;; Sort by last-touched, most recent first
        (setq file-list (sort file-list
                              (lambda (a b)
                                (time-less-p (nth 2 b) (nth 2 a)))))
        (magit-insert-section (files nil t)
          (magit-insert-heading
            (claude-gravity--section-divider (format "Files (%d)" (length file-list))))
          (dolist (entry file-list)
            (let* ((path (nth 0 entry))
                   (ops (nth 1 entry))
                   (basename (file-name-nondirectory path))
                   (ops-str (string-join (reverse ops) ", ")))
              (magit-insert-section (file-entry path t)
                (magit-insert-heading
                  (format "%s%-30s %s"
                          (claude-gravity--indent)
                          (propertize basename 'face 'claude-gravity-tool-name)
                          (propertize ops-str 'face 'claude-gravity-file-ops)))
                (claude-gravity--insert-wrapped path nil 'claude-gravity-detail-label))))
          (insert "\n"))))))


(defun claude-gravity-insert-allow-patterns (session)
  "Insert allow patterns section for SESSION."
  (let ((patterns (plist-get session :allow-patterns)))
    (when patterns
      (magit-insert-section (allow-patterns nil t)
        (magit-insert-heading
          (claude-gravity--section-divider (format "Allow Patterns (%d)" (length patterns))))
        (dolist (pat patterns)
          (magit-insert-section (allow-pattern pat)
            (insert (format "%s%s\n" (claude-gravity--indent) (propertize pat 'face 'claude-gravity-detail-label)))))
        (insert "\n")))))

(provide 'claude-gravity-render)
;;; claude-gravity-render.el ends here