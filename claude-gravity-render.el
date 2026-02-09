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
  "Insert header section showing session slug, tool count, elapsed time, and tokens from SESSION."
  (let* ((state (plist-get session :state))
         (tool-count (length (alist-get 'tools state)))
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
         (when (gethash (plist-get session :session-id) claude-gravity--tmux-sessions)
           (propertize " [tmux]" 'face 'claude-gravity-detail-label))
         (propertize (format "  ◆ %d tools" tool-count) 'face 'claude-gravity-detail-label)
         (when elapsed
           (propertize (format "  ⏱ %s" (claude-gravity--format-elapsed elapsed))
                       'face 'claude-gravity-detail-label))
         (when (and in-tokens (> in-tokens 0))
           (propertize (format "  ↓%s ↑%s tokens"
                               (claude-gravity--format-token-count in-tokens)
                               (claude-gravity--format-token-count out-tokens))
                       'face 'claude-gravity-detail-label))))
      (insert (propertize top-line 'face 'claude-gravity-divider) "\n")
      (insert "\n"))))


(defun claude-gravity--tasks-by-turn (tasks-ht)
  "Group tasks from TASKS-HT by turn number.
Returns a hash table mapping turn -> list of task alists."
  (let ((groups (make-hash-table :test 'equal)))
    (when tasks-ht
      (maphash (lambda (key val)
                 (unless (string-prefix-p "_pending_" key)
                   (let ((turn (or (alist-get 'turn val) 0)))
                     (puthash turn (cons val (gethash turn groups)) groups))))
               tasks-ht))
    (maphash (lambda (k v) (puthash k (nreverse v) groups)) groups)
    groups))


(defun claude-gravity--turn-counts (tools agents tasks)
  "Format count string for TOOLS, AGENTS, TASKS lists.
Format: [Nt Ma P tools] — tasks, agents, total tools."
  (let ((parts nil))
    ;; Build in reverse order (push reverses), final: tasks agents tools
    (when (and tools (> (length tools) 0))
      (push (format "%d tools" (length tools)) parts))
    (when (and agents (> (length agents) 0))
      (push (format "%da" (length agents)) parts))
    (when (and tasks (> (length tasks) 0))
      (push (format "%dt" (length tasks)) parts))
    (if parts
        (propertize (format "[%s]" (string-join parts " "))
                    'face 'claude-gravity-detail-label)
      "")))


(defun claude-gravity--text-subsumes-p (a b)
  "Return non-nil if text A fully contains B's content.
Checks equality and paragraph-boundary prefix match."
  (and a b
       (not (string-empty-p a))
       (not (string-empty-p b))
       (or (equal a b)
           (string-prefix-p (concat b "\n\n") a)
           (string-prefix-p (concat a "\n\n") b))))


(defun claude-gravity--dedup-assistant-text (tools)
  "Remove duplicate assistant_text and assistant_thinking on consecutive TOOLS.
Handles three cases:
1. Parallel tool calls get the same preceding content — clear on 2nd+ tool.
2. Post-tool text on tool N overlaps with assistant_text on tool N+1 — keep
   the more complete version, clear the other.
3. Post-tool thinking similarly deduped."
  (let ((prev-text nil)
        (prev-thinking nil)
        (prev-post-text nil)
        (prev-post-thinking nil))
    (dolist (item tools)
      (let ((atext (alist-get 'assistant_text item))
            (athink (alist-get 'assistant_thinking item)))
        ;; Case 1: parallel tools — same preceding content
        (when (and atext (equal atext prev-text))
          (setf (alist-get 'assistant_text item) nil)
          (setq atext nil))
        (when (and athink (equal athink prev-thinking))
          (setf (alist-get 'assistant_thinking item) nil)
          (setq athink nil))
        ;; Case 2: post_text of previous tool overlaps with this tool's assistant_text
        ;; Keep the more complete version, clear the shorter one
        (when (and atext prev-post-text
                   (claude-gravity--text-subsumes-p atext prev-post-text))
          (if (>= (length prev-post-text) (length atext))
              ;; post_text is more complete or equal — clear assistant_text
              (progn
                (setf (alist-get 'assistant_text item) nil)
                (setq atext nil))
            ;; assistant_text is more complete — clear post_text on previous tool
            ;; (iterate to find previous tool with this post_text)
            (claude-gravity--clear-post-text-matching tools prev-post-text)
            (setq prev-post-text nil)))
        ;; Same for thinking
        (when (and athink prev-post-thinking
                   (claude-gravity--text-subsumes-p athink prev-post-thinking))
          (if (>= (length prev-post-thinking) (length athink))
              (progn
                (setf (alist-get 'assistant_thinking item) nil)
                (setq athink nil))
            (claude-gravity--clear-post-thinking-matching tools prev-post-thinking)
            (setq prev-post-thinking nil)))
        ;; Track for next iteration
        (setq prev-text atext
              prev-thinking athink
              prev-post-text (alist-get 'post_text item)
              prev-post-thinking (alist-get 'post_thinking item))))))


(defun claude-gravity--clear-post-text-matching (tools text)
  "Clear post_text on the tool in TOOLS whose post_text equals TEXT."
  (dolist (item tools)
    (when (equal (alist-get 'post_text item) text)
      (setf (alist-get 'post_text item) nil))))


(defun claude-gravity--clear-post-thinking-matching (tools text)
  "Clear post_thinking on the tool in TOOLS whose post_thinking equals TEXT."
  (dolist (item tools)
    (when (equal (alist-get 'post_thinking item) text)
      (setf (alist-get 'post_thinking item) nil))))


(defun claude-gravity--group-response-cycles (tools)
  "Group TOOLS into response cycles based on assistant text boundaries.
A new cycle starts at each tool with non-nil assistant_text or
assistant_thinking (after dedup).  Returns a list of tool-lists."
  (let ((cycles nil)
        (current nil))
    (dolist (item tools)
      (let ((atext (alist-get 'assistant_text item))
            (athink (alist-get 'assistant_thinking item)))
        (when (and current
                   (or (and atext (not (string-empty-p atext)))
                       (and athink (not (string-empty-p athink)))))
          ;; New assistant text = new response cycle
          (push (nreverse current) cycles)
          (setq current nil))
        (push item current)))
    (when current
      (push (nreverse current) cycles))
    (nreverse cycles)))


(defun claude-gravity--build-agent-lookup (agents)
  "Build lookup table from AGENTS for correlating with Task tool calls.
Returns a hash table mapping \"TURN:TYPE\" to a list of agent entries.
Multiple agents with the same turn+type are returned in order."
  (let ((ht (make-hash-table :test 'equal)))
    (dolist (agent agents)
      (let* ((turn (or (alist-get 'turn agent) 0))
             (atype (or (alist-get 'type agent) ""))
             (key (format "%d:%s" turn atype)))
        (puthash key (cons agent (gethash key ht)) ht)))
    (maphash (lambda (k v) (puthash k (nreverse v) ht)) ht)
    ht))


(defun claude-gravity--pop-matching-agent (lookup turn subagent-type)
  "Pop and return the first matching agent from LOOKUP for TURN and SUBAGENT-TYPE."
  (when (and turn subagent-type)
    (let* ((key (format "%d:%s" turn subagent-type))
           (agents (gethash key lookup)))
      (when agents
        (puthash key (cdr agents) lookup)
        (car agents)))))


(defun claude-gravity--response-cycle-heading (cycle)
  "Return (TEXT . FACE) for a response CYCLE heading.
Shows tool count; full assistant text is rendered in the expanded body."
  (cons (format "%d tool%s" (length cycle) (if (= (length cycle) 1) "" "s"))
        'claude-gravity-detail-label))


(defun claude-gravity--insert-turn-children (tools agents tasks)
  "Insert response cycles, inline agent annotations, and tasks for a turn.
Tools are grouped into response cycles (each assistant message + its tool calls)."
  ;; Filter out AskUserQuestion (shown as prompt entries, not tool items)
  (setq tools (cl-remove-if (lambda (item) (equal (alist-get 'name item) "AskUserQuestion")) tools))
  ;; Dedup assistant text across parallel tool calls
  (when tools (claude-gravity--dedup-assistant-text tools))
  ;; Build agent lookup for inline annotation
  (let ((agent-lookup (claude-gravity--build-agent-lookup agents)))
    ;; Render tools as response cycles
    (when (and tools (> (length tools) 0))
      (let* ((cycles (claude-gravity--group-response-cycles tools))
             (n-cycles (length cycles))
             (cycle-idx 0))
        (dolist (cycle cycles)
          (let* ((is-last (= cycle-idx (1- n-cycles)))
                 (all-done (cl-every (lambda (item) (equal (alist-get 'status item) "done")) cycle))
                 (should-collapse (and (not is-last) all-done))
                 (first-item (car cycle))
                 (athink (alist-get 'assistant_thinking first-item))
                 (atext (alist-get 'assistant_text first-item))
                 (athink (when athink (string-trim athink)))
                 (atext (when atext (string-trim atext)))
                 (split (when (and atext (not (string-empty-p atext)))
                          (claude-gravity--split-margin-text atext 'claude-gravity-assistant-text)))
                 (tools-label (format "%d tool%s" (length cycle)
                                      (if (= (length cycle) 1) "" "s"))))
            ;; Thinking stays outside the section (always visible)
            (when (and athink (not (string-empty-p athink)))
              (let* ((indent (or (* (claude-gravity--section-depth) claude-gravity--indent-step) 0))
                     (margin (propertize (concat claude-gravity--margin-char " ")
                                        'face claude-gravity--margin-face))
                     (prefix (concat (make-string indent ?\s) margin)))
                (insert prefix (propertize "Thinking..." 'face 'claude-gravity-thinking) "\n")
                (claude-gravity--insert-wrapped athink (+ indent 4) 'claude-gravity-thinking)))
            ;; Response cycle section — heading is first line of assistant text
            (magit-insert-section (response-cycle cycle-idx should-collapse)
              (magit-insert-heading
                (if split
                    (car split)
                  (format "%s%s%s"
                          (claude-gravity--indent)
                          (propertize (concat claude-gravity--margin-char " ")
                                      'face claude-gravity--margin-face)
                          (propertize tools-label 'face 'claude-gravity-detail-label))))
              ;; Body: remaining assistant text lines (visible when collapsed)
              (when (cdr split)
                (insert (cdr split))
                ;; Move content marker past assistant text so it stays
                ;; visible when the section is collapsed
                (oset magit-insert-section--current content
                      (if magit-section-inhibit-markers (point) (point-marker))))
              ;; Body: tool count summary (when heading was assistant text)
              (when split
                (insert (format "%s%s%s\n"
                                (claude-gravity--indent)
                                (propertize (concat claude-gravity--margin-char " ")
                                            'face claude-gravity--margin-face)
                                (propertize tools-label 'face 'claude-gravity-detail-label))))
              ;; First tool (context already rendered as heading/body above)
              (claude-gravity--insert-tool-item first-item agent-lookup)
              ;; Remaining tools with their context
              (dolist (item (cdr cycle))
                (claude-gravity--insert-tool-context item)
                (claude-gravity--insert-tool-item item agent-lookup)))
            ;; Separator between cycles (not after the last)
            (unless is-last
              (claude-gravity--turn-separator)))
          (cl-incf cycle-idx)))))
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
            (claude-gravity--insert-task-item task)))))))


(defun claude-gravity--insert-task-item (task)
  "Insert a single TASK as a magit-section."
  (let* ((subject (or (alist-get 'subject task) "(no subject)"))
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
    (magit-insert-section (task task)
      (insert (format "%s%s %s%s\n" (claude-gravity--indent) checkbox subject suffix)))))


(defun claude-gravity-insert-turns (session)
  "Insert unified turns section for SESSION.
Each turn groups its prompt, tools, agents, and tasks together."
  (let* ((state (plist-get session :state))
         (tools (alist-get 'tools state))
         (prompts (plist-get session :prompts))
         (agents (plist-get session :agents))
         (tasks-ht (plist-get session :tasks))
         (current-turn (or (plist-get session :current-turn) 0))
         (tool-groups (make-hash-table :test 'equal))
         (agent-groups (make-hash-table :test 'equal))
         (task-groups (claude-gravity--tasks-by-turn tasks-ht))
         (max-turn current-turn))
    ;; Build tool groups (excluding AskUserQuestion, shown as prompt entries)
    (dolist (item tools)
      (unless (equal (alist-get 'name item) "AskUserQuestion")
        (let ((turn (or (alist-get 'turn item) 0)))
          (when (> turn max-turn) (setq max-turn turn))
          (puthash turn (cons item (gethash turn tool-groups)) tool-groups))))
    (maphash (lambda (k v) (puthash k (nreverse v) tool-groups)) tool-groups)
    ;; Build agent groups
    (dolist (agent agents)
      (let ((turn (or (alist-get 'turn agent) 0)))
        (when (> turn max-turn) (setq max-turn turn))
        (puthash turn (cons agent (gethash turn agent-groups)) agent-groups)))
    (maphash (lambda (k v) (puthash k (nreverse v) agent-groups)) agent-groups)
    ;; Check max-turn from task groups
    (maphash (lambda (turn _)
               (when (and (numberp turn) (> turn max-turn))
                 (setq max-turn turn)))
             task-groups)
    ;; Render if there are any turns or pre-prompt activity
    (when (or (> max-turn 0)
              (gethash 0 tool-groups)
              (gethash 0 agent-groups)
              (gethash 0 task-groups))
      (magit-insert-section (turns nil t)
        (magit-insert-heading
          (claude-gravity--section-divider (format "Turns (%d)" current-turn)))
        ;; Turn 0: pre-prompt activity
        (let ((t0-tools (gethash 0 tool-groups))
              (t0-agents (gethash 0 agent-groups))
              (t0-tasks (gethash 0 task-groups)))
          (when (or t0-tools t0-agents t0-tasks)
            (magit-insert-section (turn 0 t)
              (magit-insert-heading
                (format "%s  %s"
                        (propertize "Pre-prompt activity" 'face 'claude-gravity-detail-label)
                        (claude-gravity--turn-counts t0-tools t0-agents t0-tasks)))
              (claude-gravity--insert-turn-children t0-tools t0-agents t0-tasks))))
        ;; Turns 1..max-turn
        (let ((prev-turn-rendered nil))
          (dotimes (j max-turn)
            (let* ((i (1+ j))
                   (prompt-entry (when (and prompts (<= i (length prompts)))
                                   (nth (1- i) prompts)))
                   (turn-tools (gethash i tool-groups))
                   (turn-agents (gethash i agent-groups))
                   (turn-tasks (gethash i task-groups))
                   (is-current (= i current-turn))
                 (prompt-text (when prompt-entry
                                (claude-gravity--prompt-text prompt-entry)))
                 (is-question (when (listp prompt-entry)
                                (eq (alist-get 'type prompt-entry) 'question)))
                 (is-phase-boundary (when (listp prompt-entry)
                                      (eq (alist-get 'type prompt-entry) 'phase-boundary)))
                 (elapsed (when (listp prompt-entry)
                            (alist-get 'elapsed prompt-entry)))
                 (elapsed-str (claude-gravity--format-elapsed elapsed))
                 (indicator (cond (is-phase-boundary
                                   (propertize "→" 'face 'claude-gravity-phase-boundary))
                                  (is-question
                                   (propertize "?" 'face 'claude-gravity-question))
                                  (t
                                   (propertize "❯" 'face 'claude-gravity-prompt))))
                 (counts (claude-gravity--turn-counts turn-tools turn-agents turn-tasks))
                 (answer (when is-question (alist-get 'answer prompt-entry)))
                 (answer-suffix (if answer
                                    (format "  → %s" (claude-gravity--truncate answer 40))
                                  ""))
                 (prompt-face (cond (is-phase-boundary 'claude-gravity-phase-boundary)
                                    (is-question 'claude-gravity-question)
                                    (t 'claude-gravity-prompt)))
                 (_heading-parts (list indicator
                                       (propertize (claude-gravity--truncate (or prompt-text "(no prompt)") 60)
                                                   'face prompt-face)
                                       (propertize answer-suffix 'face 'claude-gravity-detail-label)
                                       (propertize counts 'face 'claude-gravity-detail-label)
                                       (propertize elapsed-str 'face 'claude-gravity-detail-label))))
            (when (or turn-tools turn-agents turn-tasks prompt-entry)
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
                    (add-face-text-property start (point) prompt-face))))
              (magit-insert-section (turn i (not is-current))
                (magit-insert-heading
                  (format "%s%s%s  %s"
                          (claude-gravity--indent)
                          (propertize counts 'face 'claude-gravity-detail-label)
                          (propertize answer-suffix 'face 'claude-gravity-detail-label)
                          (propertize elapsed-str 'face 'claude-gravity-detail-label)))
                ;; Children: tools, agents, tasks
                (claude-gravity--insert-turn-children turn-tools turn-agents turn-tasks)
                ;; Trailing assistant text (conclusion after last tool)
                ;; Dedup against last tool's post_text/post_thinking
                (let* ((stop-think (and (listp prompt-entry) (alist-get 'stop_thinking prompt-entry)))
                       (stop-text (and (listp prompt-entry) (alist-get 'stop_text prompt-entry)))
                       (last-tool (and turn-tools
                                       (car (last turn-tools))))
                       (last-post-text (when last-tool (alist-get 'post_text last-tool)))
                       (last-post-think (when last-tool (alist-get 'post_thinking last-tool))))
                  ;; Dedup stop_thinking vs last tool's post_thinking
                  (when (and stop-think last-post-think
                             (claude-gravity--text-subsumes-p stop-think last-post-think))
                    (if (>= (length last-post-think) (length stop-think))
                        (setq stop-think nil) ; post_thinking covers it
                      (setf (alist-get 'post_thinking last-tool) nil))) ; stop has more
                  ;; Dedup stop_text vs last tool's post_text
                  (when (and stop-text last-post-text
                             (claude-gravity--text-subsumes-p stop-text last-post-text))
                    (if (>= (length last-post-text) (length stop-text))
                        (setq stop-text nil) ; post_text covers it
                      (setf (alist-get 'post_text last-tool) nil))) ; stop has more
                  (when (and stop-think (not (string-empty-p stop-think)))
                    (claude-gravity--insert-wrapped-with-margin
                     stop-think nil 'claude-gravity-thinking))
                  (when (and stop-text (not (string-empty-p stop-text)))
                    (claude-gravity--insert-wrapped-with-margin stop-text nil 'claude-gravity-assistant-text))))))))
        (insert "\n")))))


(defun claude-gravity--insert-agent-branch (item agent agent-lookup &optional depth)
  "Insert ITEM (a Task tool) as a sub-branch with AGENT's nested tools.
AGENT-LOOKUP is passed through for nested agent resolution.
DEPTH tracks nesting level (0 = first agent, 1+ = nested sub-agents).
The agent's tools are rendered using the same response-cycle grouping
as root tools, allowing recursive sub-branches for nested agents."
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
         (agent-suffix (format "  %s %s (%s)%s"
                               (propertize "→" 'face 'claude-gravity-detail-label)
                               (propertize (or atype "?") 'face 'claude-gravity-tool-name)
                               (propertize short-id 'face 'claude-gravity-detail-label)
                               dur-str))
         (agent-tools (alist-get 'agent-tools agent))
         (tool-list agent-tools)
         ;; Filter AskUserQuestion from agent tools
         (tool-list (cl-remove-if
                     (lambda (item) (equal (alist-get 'name item) "AskUserQuestion"))
                     tool-list))
         ;; Collapse when agent is done
         (collapsed (and agent-done-p (not (null tool-list)))))
    (let ((section-start (point)))
      (magit-insert-section (tool item collapsed)
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
        ;; Render agent's tools as response cycles with agent-specific styling
        (let ((claude-gravity--margin-char "┃")
              (claude-gravity--margin-face 'claude-gravity-agent-margin)
              (claude-gravity--agent-depth (1+ (or depth claude-gravity--agent-depth 0)))
              (body-start (point)))
          (when tool-list
            (claude-gravity--dedup-assistant-text tool-list)
            ;; Build nested agent lookup from agents that have the same turn
            ;; and were spawned within this agent's tools
            (let* ((nested-agents (claude-gravity--collect-nested-agents
                                   agent tool-list (plist-get
                                                    (claude-gravity--get-session
                                                     (when (boundp 'claude-gravity--buffer-session-id)
                                                       claude-gravity--buffer-session-id))
                                                    :agents)))
                   (nested-lookup (claude-gravity--build-agent-lookup nested-agents))
                   (cycles (claude-gravity--group-response-cycles tool-list))
                   (n-cycles (length cycles))
                   (cycle-idx 0))
              (dolist (cycle cycles)
                (let* ((is-last (= cycle-idx (1- n-cycles)))
                       (all-done (cl-every (lambda (ti) (equal (alist-get 'status ti) "done")) cycle))
                       (should-collapse (and (not is-last) all-done))
                       (first-item (car cycle))
                       (athink (alist-get 'assistant_thinking first-item))
                       (atext (alist-get 'assistant_text first-item))
                       (athink (when athink (string-trim athink)))
                       (atext (when atext (string-trim atext)))
                       (split (when (and atext (not (string-empty-p atext)))
                                (claude-gravity--split-margin-text atext 'claude-gravity-assistant-text)))
                       (tools-label (format "%d tool%s" (length cycle)
                                            (if (= (length cycle) 1) "" "s"))))
                  ;; Thinking stays outside the section (always visible)
                  (when (and athink (not (string-empty-p athink)))
                    (let* ((indent (or (* (claude-gravity--section-depth) claude-gravity--indent-step) 0))
                           (margin (propertize (concat claude-gravity--margin-char " ")
                                              'face claude-gravity--margin-face))
                           (prefix (concat (make-string indent ?\s) margin)))
                      (insert prefix (propertize "Thinking..." 'face 'claude-gravity-thinking) "\n")
                      (claude-gravity--insert-wrapped athink (+ indent 4) 'claude-gravity-thinking)))
                  ;; Response cycle section — heading is first line of assistant text
                  (magit-insert-section (response-cycle cycle-idx should-collapse)
                    (magit-insert-heading
                      (if split
                          (car split)
                        (format "%s%s%s"
                                (claude-gravity--indent)
                                (propertize (concat claude-gravity--margin-char " ")
                                            'face claude-gravity--margin-face)
                                (propertize tools-label 'face 'claude-gravity-detail-label))))
                    ;; Body: remaining assistant text lines (visible when collapsed)
                    (when (cdr split)
                      (insert (cdr split))
                      ;; Move content marker past assistant text so it stays
                      ;; visible when the section is collapsed
                      (oset magit-insert-section--current content
                            (if magit-section-inhibit-markers (point) (point-marker))))
                    ;; Body: tool count summary (when heading was assistant text)
                    (when split
                      (insert (format "%s%s%s\n"
                                      (claude-gravity--indent)
                                      (propertize (concat claude-gravity--margin-char " ")
                                                  'face claude-gravity--margin-face)
                                      (propertize tools-label 'face 'claude-gravity-detail-label))))
                    (claude-gravity--insert-tool-item first-item nested-lookup)
                    (dolist (ti (cdr cycle))
                      (claude-gravity--insert-tool-context ti)
                      (claude-gravity--insert-tool-item ti nested-lookup)))
                  (unless is-last
                    (claude-gravity--turn-separator)))
                (cl-incf cycle-idx))))
          ;; Agent's trailing summary text (final result returned to parent)
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
          (when (and (not tool-list) (not agent-done-p))
            (insert (format "%s%s\n"
                            (claude-gravity--indent)
                            (propertize "Agent running..." 'face 'claude-gravity-detail-label))))
          ;; Apply agent background tint to body (after heading)
          (add-face-text-property body-start (point)
                                  (if (> claude-gravity--agent-depth 1)
                                      'claude-gravity-agent-nested-bg
                                    'claude-gravity-agent-bg))))
      ;; Apply background tint to running tools (stacks with agent-bg)
      (unless done-p
        (add-face-text-property section-start (point) 'claude-gravity-running-bg)))))


(defun claude-gravity--collect-nested-agents (parent-agent parent-tools all-agents)
  "Collect agents that are nested within PARENT-AGENT.
PARENT-TOOLS are the tools belonging to the parent agent.
ALL-AGENTS is the session's full agents vector.
Returns a list of agents whose turn:type matches a Task tool in parent-tools."
  (let ((task-keys (make-hash-table :test 'equal))
        (result nil))
    ;; Collect turn:subagent_type keys from Task tool calls in parent's tools
    (dolist (tool parent-tools)
      (when (equal (alist-get 'name tool) "Task")
        (let* ((turn (or (alist-get 'turn tool) 0))
               (stype (or (alist-get 'subagent_type (alist-get 'input tool)) ""))
               (key (format "%d:%s" turn stype)))
          (puthash key t task-keys))))
    ;; Only include agents whose turn:type matches one of the parent's Task keys
    (when all-agents
      (dolist (agent all-agents)
        (let ((atools (alist-get 'agent-tools agent)))
          (when (and atools (> (length atools) 0)
                     (not (equal (alist-get 'agent_id agent)
                                 (alist-get 'agent_id parent-agent))))
            (let* ((aturn (or (alist-get 'turn agent) 0))
                   (atype (or (alist-get 'type agent) ""))
                   (key (format "%d:%s" aturn atype)))
              (when (gethash key task-keys)
                (push agent result)))))))
    (nreverse result)))


(defun claude-gravity--insert-tool-item (item &optional agent-lookup)
  "Insert a single tool ITEM as a magit-section.
When AGENT-LOOKUP is provided and ITEM is a Task tool, annotate with agent info."
  (let* ((name (alist-get 'name item))
         (status (alist-get 'status item))
         (input (alist-get 'input item))
         (result (alist-get 'result item))
         (done-p (equal status "done"))
         (error-p (equal status "error"))
         (indicator (propertize (cond (error-p "[!]")
                                      (done-p "[x]")
                                      (t "[/]"))
                                'face (cond (error-p 'claude-gravity-tool-error)
                                            (done-p 'claude-gravity-tool-done)
                                            (t 'claude-gravity-tool-running))))
         (tool-face (propertize (or name "?") 'face 'claude-gravity-tool-name))
         (summary (claude-gravity--tool-summary name input))
         (desc (claude-gravity--tool-description input))
         ;; Agent annotation for Task tools
         (agent (when (and agent-lookup (equal name "Task"))
                  (claude-gravity--pop-matching-agent
                   agent-lookup
                   (alist-get 'turn item)
                   (alist-get 'subagent_type input)))))
    ;; If agent has nested tools, render as sub-branch instead
    (if (and agent (let ((at (alist-get 'agent-tools agent)))
                     (and at (> (length at) 0))))
        (claude-gravity--insert-agent-branch item agent agent-lookup)
      (let* ((agent-suffix
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
                    (format "  %s %s (%s)%s"
                            (propertize "→" 'face 'claude-gravity-detail-label)
                            (propertize (or atype "?") 'face 'claude-gravity-tool-name)
                            (propertize short-id 'face 'claude-gravity-detail-label)
                            dur-str))
                ""))
             (agent-icon (if agent "🤖 " ""))
             (section-start (point)))
      (magit-insert-section (tool item t)
        (magit-insert-heading
          (if desc
              (format "%s%s%s %s\n%s%s%s"
                      (claude-gravity--indent)
                      agent-icon
                      indicator
                      (propertize desc 'face 'claude-gravity-tool-description)
                      (claude-gravity--indent 2)
                      (propertize (claude-gravity--tool-signature name input)
                                  'face 'claude-gravity-tool-signature)
                      agent-suffix)
            (format "%s%s%s %s  %s%s"
                    (claude-gravity--indent)
                    agent-icon
                    indicator
                    tool-face
                    (propertize summary 'face 'claude-gravity-detail-label)
                    agent-suffix)))
        ;; Show permission-format signature in detail (only for single-line tools)
        (unless desc
          (let ((sig (claude-gravity--tool-signature name input)))
            (claude-gravity--insert-wrapped sig nil 'claude-gravity-tool-signature)))
        (insert "\n")
        (claude-gravity--insert-tool-detail name input result status)
        ;; Post-tool text and thinking (text generated after this tool completes)
        (let ((post-think (alist-get 'post_thinking item))
              (post-text (alist-get 'post_text item)))
          (when (and post-think (not (string-empty-p post-think)))
            (claude-gravity--insert-wrapped-with-margin post-think nil 'claude-gravity-thinking)
            (insert "\n"))
          (when (and post-text (not (string-empty-p post-text)))
            (claude-gravity--insert-wrapped-with-margin post-text nil 'claude-gravity-assistant-text)
            (insert "\n")))
        ;; Agent detail (transcript, model, etc.) in expanded view
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
        (add-face-text-property section-start (point) 'claude-gravity-running-bg))))))



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



(defun claude-gravity--insert-agent-item (agent)
  "Insert a single AGENT as a magit-section with expandable detail."
  (let* ((agent-type (alist-get 'type agent))
         (agent-id (alist-get 'agent_id agent))
         (status (alist-get 'status agent))
         (duration (alist-get 'duration agent))
         (done-p (equal status "done"))
         (indicator (propertize (if done-p "[x]" "[/]")
                                'face (if done-p
                                           'claude-gravity-tool-done
                                         'claude-gravity-tool-running)))
         (short-id (if (and agent-id (> (length agent-id) 7))
                       (substring agent-id 0 7)
                     (or agent-id "?")))
         (duration-str (if (and done-p duration)
                           (format "  %s" (claude-gravity--format-duration duration))
                         "")))
    (let ((section-start (point)))
      (magit-insert-section (agent agent t)
        (magit-insert-heading
          (format "%s🤖 %s %s  (%s)%s"
                  (claude-gravity--indent)
                  indicator
                  (propertize (or agent-type "?") 'face 'claude-gravity-tool-name)
                  (propertize short-id 'face 'claude-gravity-detail-label)
                  duration-str))
        ;; Expanded detail
        (let ((tp (alist-get 'transcript_path agent))
              (parsed (alist-get 'transcript_parsed agent)))
          (when parsed
            (let ((prompt (alist-get 'transcript_prompt agent))
                  (model (alist-get 'transcript_model agent))
                  (tc (alist-get 'transcript_tool_count agent)))
              (when (and prompt (not (string-empty-p prompt)))
                (claude-gravity--insert-label "Task: ")
                (claude-gravity--insert-wrapped prompt nil))
              (when (and model (not (string-empty-p model)))
                (claude-gravity--insert-label "Model: ")
                (insert model "\n"))
              (when (and tc (> tc 0))
                (claude-gravity--insert-label "Tools: ")
                (insert (format "%d" tc) "\n"))))
          (when tp
            (claude-gravity--insert-label "Transcript: ")
            (claude-gravity--insert-wrapped tp nil 'claude-gravity-detail-label))
          (unless parsed
            (when tp
              (claude-gravity--insert-label "RET to parse transcript\n")))))
      ;; Apply background tint to running agents
      (unless done-p
        (add-face-text-property section-start (point) 'claude-gravity-running-bg)))))


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