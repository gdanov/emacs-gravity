;;; claude-gravity-diff.el --- Diff and tool display for Claude Gravity  -*- lexical-binding: t; -*-

;;; Code:

(require 'claude-gravity-core)
(require 'claude-gravity-faces)
(require 'claude-gravity-text)


;;; Tool display helpers

(defun claude-gravity--extract-ask-answer (response)
  "Extract the user's answer text from AskUserQuestion RESPONSE."
  (cond
   ;; MCP-style vector result
   ((vectorp response)
    (let* ((first (and (> (length response) 0) (aref response 0)))
           (text (and (listp first) (alist-get 'text first))))
      text))
   ;; Alist with answers dict (AskUserQuestion format)
   ((and (listp response) (alist-get 'answers response))
    (let ((answers (alist-get 'answers response)))
      (when (listp answers)
        (cdar answers))))
   ;; Alist with stdout
   ((and (listp response) (alist-get 'stdout response))
    (alist-get 'stdout response))
   ;; Plain string
   ((stringp response) response)
   (t nil)))


;;; ---- Inline diff helpers for Edit tool (word-level LCS) ----

(defun claude-gravity--word-tokenize (text)
  "Split TEXT into a list of word, whitespace, and punctuation tokens.
Newlines are separate tokens.  Runs of spaces/tabs are single tokens.
Word characters [a-zA-Z0-9_] form word tokens."
  (let ((pos 0)
        (len (length text))
        tokens)
    (while (< pos len)
      (let ((ch (aref text pos)))
        (cond
         ;; Newline
         ((= ch ?\n)
          (push "\n" tokens)
          (setq pos (1+ pos)))
         ;; Whitespace (non-newline)
         ((memq ch '(?\s ?\t))
          (let ((start pos))
            (while (and (< pos len)
                        (memq (aref text pos) '(?\s ?\t)))
              (setq pos (1+ pos)))
            (push (substring text start pos) tokens)))
         ;; Word characters
         ((or (and (>= ch ?a) (<= ch ?z))
              (and (>= ch ?A) (<= ch ?Z))
              (and (>= ch ?0) (<= ch ?9))
              (= ch ?_))
          (let ((start pos))
            (while (and (< pos len)
                        (let ((c (aref text pos)))
                          (or (and (>= c ?a) (<= c ?z))
                              (and (>= c ?A) (<= c ?Z))
                              (and (>= c ?0) (<= c ?9))
                              (= c ?_))))
              (setq pos (1+ pos)))
            (push (substring text start pos) tokens)))
         ;; Everything else (punctuation) — single char
         (t
          (push (substring text pos (1+ pos)) tokens)
          (setq pos (1+ pos))))))
    (nreverse tokens)))


(defun claude-gravity--lcs-diff (old-tokens new-tokens)
  "Compute word-level diff between OLD-TOKENS and NEW-TOKENS.
Returns a list of (TAG . TOKEN) where TAG is `equal', `added', or `removed'.
Falls back to bulk add/remove if token product exceeds 250000."
  (let ((n (length old-tokens))
        (m (length new-tokens)))
    (if (> (* n m) 250000)
        ;; Too large — mark all old as removed, all new as added
        (append (mapcar (lambda (tok) (cons 'removed tok)) old-tokens)
                (mapcar (lambda (tok) (cons 'added tok)) new-tokens))
      ;; Standard LCS with DP table
      (let ((old-vec (vconcat old-tokens))
            (new-vec (vconcat new-tokens))
            (dp (make-vector (1+ n) nil)))
        ;; Initialize DP table
        (dotimes (i (1+ n))
          (aset dp i (make-vector (1+ m) 0)))
        ;; Fill DP table
        (dotimes (i n)
          (dotimes (j m)
            (if (equal (aref old-vec i) (aref new-vec j))
                (aset (aref dp (1+ i)) (1+ j)
                      (1+ (aref (aref dp i) j)))
              (aset (aref dp (1+ i)) (1+ j)
                    (max (aref (aref dp i) (1+ j))
                         (aref (aref dp (1+ i)) j))))))
        ;; Backtrace
        (let ((i n) (j m) result)
          (while (or (> i 0) (> j 0))
            (cond
             ((and (> i 0) (> j 0)
                   (equal (aref old-vec (1- i)) (aref new-vec (1- j)))
                   (= (aref (aref dp i) j)
                      (1+ (aref (aref dp (1- i)) (1- j)))))
              (push (cons 'equal (aref old-vec (1- i))) result)
              (setq i (1- i) j (1- j)))
             ((and (> j 0)
                   (or (= i 0)
                       (>= (aref (aref dp i) (1- j))
                            (aref (aref dp (1- i)) j))))
              (push (cons 'added (aref new-vec (1- j))) result)
              (setq j (1- j)))
             (t
              (push (cons 'removed (aref old-vec (1- i))) result)
              (setq i (1- i)))))
          result)))))


(defun claude-gravity--propertize-diff (diff-ops)
  "Convert DIFF-OPS list of (TAG . TOKEN) into a propertized string.
Added tokens get `claude-gravity-diff-added' face.
Removed tokens get `claude-gravity-diff-removed' face."
  (let (parts)
    (dolist (op diff-ops)
      (let ((tag (car op))
            (tok (cdr op)))
        (push (pcase tag
                ('added (propertize tok 'face 'claude-gravity-diff-added))
                ('removed (propertize tok 'face 'claude-gravity-diff-removed))
                (_ tok))
              parts)))
    (apply #'concat (nreverse parts))))


(defun claude-gravity--insert-edit-diff (input result status)
  "Insert inline word-level diff for an Edit tool.
When STATUS is \"running\", uses `old_string'/`new_string' from INPUT.
When STATUS is \"done\", uses `structuredPatch' from RESULT if available,
falling back to INPUT fields."
  (let (old-str new-str patch label)
    ;; Extract data based on phase
    (cond
     ;; Done — try structuredPatch first
     ((equal status "done")
      (setq patch (and (listp result) (alist-get 'structuredPatch result)))
      (unless patch
        ;; Fallback to camelCase result fields, then input fields
        (setq old-str (or (and (listp result) (alist-get 'oldString result))
                          (alist-get 'old_string input))
              new-str (or (and (listp result) (alist-get 'newString result))
                          (alist-get 'new_string input))))
      (setq label "Diff:"))
     ;; Running — use input fields
     (t
      (setq old-str (alist-get 'old_string input)
            new-str (alist-get 'new_string input))
      (setq label "Diff (pending):")))
    ;; Render
    (cond
     ;; structuredPatch available — unified style with word-level refinement
     (patch
      (claude-gravity--insert-structured-patch patch label))
     ;; old/new strings available — inline diff
     ((and new-str (stringp new-str) (not (string-empty-p new-str)))
      (claude-gravity--insert-inline-diff old-str new-str label))
     ;; Nothing to show
     (t nil))))


(defvar claude-gravity--diff-cache (make-hash-table :test 'equal :size 128)
  "Cache of LCS diff results.  Key is (old-str . new-str), value is diff-text.")

(defun claude-gravity--diff-cache-clear ()
  "Clear the LCS diff result cache."
  (clrhash claude-gravity--diff-cache))

(defun claude-gravity--insert-inline-diff (old-str new-str label)
  "Insert inline word-level diff between OLD-STR and NEW-STR.
LABEL is the section header like \"Diff:\" or \"Diff (pending):\".
Caches diff computation results for reuse across renders."
  (let* ((prefix (claude-gravity--indent))
         (old-str (or old-str ""))
         (cache-key (cons old-str new-str))
         (diff-text (or (gethash cache-key claude-gravity--diff-cache)
                        (let* ((old-tokens (claude-gravity--word-tokenize old-str))
                               (new-tokens (claude-gravity--word-tokenize new-str))
                               (diff-ops (claude-gravity--lcs-diff old-tokens new-tokens))
                               (text (claude-gravity--propertize-diff diff-ops)))
                          (when (> (hash-table-count claude-gravity--diff-cache) 256)
                            (clrhash claude-gravity--diff-cache))
                          (puthash cache-key text claude-gravity--diff-cache)
                          text)))
         (lines (split-string diff-text "\n"))
         (nlines (length lines))
         (max-lines claude-gravity-diff-max-lines)
         (show-lines (if (> nlines max-lines)
                         (seq-take lines max-lines)
                       lines)))
    (claude-gravity--insert-label (concat label "\n"))
    (dolist (line show-lines)
      (insert prefix "  " line "\n"))
    (when (> nlines max-lines)
      (insert (propertize (format "%s  ... (%d more lines)\n"
                                  prefix (- nlines max-lines))
                          'face 'claude-gravity-detail-label)))))


(defun claude-gravity--insert-structured-patch (patch label)
  "Insert PATCH (vector of hunk objects) in unified style with word-level refinement.
LABEL is the section header."
  (let ((prefix (claude-gravity--indent))
        (total-lines 0))
    (claude-gravity--insert-label (concat label "\n"))
    (let ((hunks (if (vectorp patch) (append patch nil) patch)))
      (dolist (hunk hunks)
        (when (< total-lines claude-gravity-diff-max-lines)
          (let ((old-start (alist-get 'oldStart hunk))
                (old-lines (alist-get 'oldLines hunk))
                (new-start (alist-get 'newStart hunk))
                (new-lines (alist-get 'newLines hunk))
                (lines-vec (alist-get 'lines hunk)))
            ;; Hunk header
            (insert prefix "  "
                    (propertize (format "@@ -%d,%d +%d,%d @@"
                                        (or old-start 0) (or old-lines 0)
                                        (or new-start 0) (or new-lines 0))
                                'face 'claude-gravity-diff-header)
                    "\n")
            (setq total-lines (1+ total-lines))
            ;; Process lines — collect adjacent -/+ groups for word-level refinement
            (let* ((raw-lines (if (vectorp lines-vec)
                                  (append lines-vec nil)
                                lines-vec))
                   (i 0)
                   (n (length raw-lines)))
              (while (and (< i n) (< total-lines claude-gravity-diff-max-lines))
                (let ((line (nth i raw-lines)))
                  (cond
                   ;; Context line
                   ((string-prefix-p " " line)
                    (insert prefix "  "
                            (propertize line 'face 'claude-gravity-diff-context)
                            "\n")
                    (setq total-lines (1+ total-lines))
                    (setq i (1+ i)))
                   ;; Removed line — collect adjacent group
                   ((string-prefix-p "-" line)
                    (let (removed-lines added-lines)
                      ;; Collect all adjacent - lines
                      (while (and (< i n) (string-prefix-p "-" (nth i raw-lines)))
                        (push (nth i raw-lines) removed-lines)
                        (setq i (1+ i)))
                      (setq removed-lines (nreverse removed-lines))
                      ;; Collect any immediately following + lines
                      (while (and (< i n) (string-prefix-p "+" (nth i raw-lines)))
                        (push (nth i raw-lines) added-lines)
                        (setq i (1+ i)))
                      (setq added-lines (nreverse added-lines))
                      ;; If paired, do word-level refinement
                      (if (and removed-lines added-lines)
                          (claude-gravity--insert-refined-hunk-lines
                           removed-lines added-lines prefix)
                        ;; Unpaired — render directly
                        (dolist (rl removed-lines)
                          (when (< total-lines claude-gravity-diff-max-lines)
                            (insert prefix "  "
                                    (propertize rl 'face 'claude-gravity-diff-removed)
                                    "\n")
                            (setq total-lines (1+ total-lines))))
                        (dolist (al added-lines)
                          (when (< total-lines claude-gravity-diff-max-lines)
                            (insert prefix "  "
                                    (propertize al 'face 'claude-gravity-diff-added)
                                    "\n")
                            (setq total-lines (1+ total-lines)))))
                      (setq total-lines (+ total-lines
                                           (length removed-lines)
                                           (length added-lines)))))
                   ;; Added line (standalone, no preceding -)
                   ((string-prefix-p "+" line)
                    (insert prefix "  "
                            (propertize line 'face 'claude-gravity-diff-added)
                            "\n")
                    (setq total-lines (1+ total-lines))
                    (setq i (1+ i)))
                   ;; Unknown prefix — just insert
                   (t
                    (insert prefix "  " line "\n")
                    (setq total-lines (1+ total-lines))
                    (setq i (1+ i))))))))))
    (when (>= total-lines claude-gravity-diff-max-lines)
      (insert (propertize (format "%s  ... (truncated)\n" prefix)
                          'face 'claude-gravity-detail-label))))))


(defun claude-gravity--insert-refined-hunk-lines (removed-lines added-lines prefix)
  "Insert REMOVED-LINES and ADDED-LINES with word-level refinement.
Pairs lines 1:1 and runs word-diff on each pair.  PREFIX is indent string."
  ;; Join all removed into one text, all added into one text
  (let* ((old-text (mapconcat (lambda (l) (substring l 1)) removed-lines "\n"))
         (new-text (mapconcat (lambda (l) (substring l 1)) added-lines "\n"))
         (old-tokens (claude-gravity--word-tokenize old-text))
         (new-tokens (claude-gravity--word-tokenize new-text))
         (diff-ops (claude-gravity--lcs-diff old-tokens new-tokens))
         (diff-text (claude-gravity--propertize-diff diff-ops))
         (lines (split-string diff-text "\n")))
    (dolist (line lines)
      (insert prefix "  " line "\n"))))


(defun claude-gravity--tool-summary (name input)
  "Produce a short one-line summary for tool NAME with INPUT alist."
  (pcase name
    ("Bash"
     (let* ((cmd (or (alist-get 'command input) ""))
            (parts (split-string cmd " " t))
            (prog (file-name-nondirectory (or (car parts) "")))
            (args (string-join (cdr parts) " ")))
       (claude-gravity--truncate (concat prog " " args) 60)))
    ("Read"
     (claude-gravity--shorten-path (alist-get 'file_path input)))
    ((or "Edit" "Write")
     (claude-gravity--shorten-path (alist-get 'file_path input)))
    ((or "Grep" "Glob")
     (let ((pattern (alist-get 'pattern input)))
       (if pattern (format "\"%s\"" (claude-gravity--truncate pattern 40)) "")))
    ("AskUserQuestion"
     (let* ((questions (alist-get 'questions input))
            (first-q (and (vectorp questions) (> (length questions) 0)
                          (aref questions 0)))
            (q-text (and first-q (alist-get 'question first-q))))
       (if q-text (claude-gravity--truncate q-text 55) "")))
    (_
     (let ((first-val (cdar input)))
       (if (stringp first-val)
           (claude-gravity--truncate first-val 50)
         "")))))


(defun claude-gravity--tool-description (input)
  "Return the description field from INPUT if present."
  (alist-get 'description input))


(defun claude-gravity--tool-signature (name input)
  "Build the permission-format signature string for tool NAME with INPUT.
Returns a string like \"Bash(npm run build)\" or \"Edit(/path/to/file)\"."
  (pcase name
    ("Bash"
     (let ((cmd (replace-regexp-in-string "[\n\r\t]+" " "
                 (or (alist-get 'command input) ""))))
       (format "Bash(%s)" cmd)))
    ((or "Edit" "Write")
     (let ((path (or (alist-get 'file_path input) "")))
       (format "%s(%s)" name path)))
    ("Read"
     (let ((path (or (alist-get 'file_path input) "")))
       (format "Read(%s)" path)))
    ("WebFetch"
     (let* ((url (or (alist-get 'url input) ""))
            (host (when (string-match "https?://\\([^/]+\\)" url)
                    (match-string 1 url))))
       (if host
           (format "WebFetch(domain:%s)" host)
         (format "WebFetch(%s)" url))))
    ((or "Grep" "Glob")
     (let ((pattern (or (alist-get 'pattern input) "")))
       (format "%s(%s)" name pattern)))
    ("Task"
     (let ((subtype (or (alist-get 'subagent_type input) ""))
           (desc (replace-regexp-in-string "[\n\r\t]+" " "
                   (or (alist-get 'description input) ""))))
       (if (not (string-empty-p desc))
           (format "Task(%s · %s)" subtype desc)
         (format "Task(%s)" subtype))))
    ("AskUserQuestion"
     "AskUserQuestion")
    (_
     name)))


(defun claude-gravity--insert-tool-detail (name input result &optional status)
  "Insert expanded detail for tool NAME with INPUT, RESULT, and STATUS."
  (pcase name
    ("Bash"
     (let ((cmd (alist-get 'command input)))
       (when cmd
         (claude-gravity--insert-label-value "Command: " cmd))))
    ("Read"
     (let ((path (alist-get 'file_path input)))
       (when path
         (claude-gravity--insert-label-value "File: " path))))
    ((or "Edit" "Write")
     (let ((path (alist-get 'file_path input)))
       (when path
         (claude-gravity--insert-label-value "File: " path)))
     (when (equal name "Edit")
       (claude-gravity--insert-edit-diff input result status)))
    ((or "Grep" "Glob")
     (let ((pattern (alist-get 'pattern input))
           (path (alist-get 'path input)))
       (when pattern
         (claude-gravity--insert-label-value "Pattern: " pattern))
       (when path
         (claude-gravity--insert-label-value "Path: " path))))
    ("AskUserQuestion"
     (let* ((questions (alist-get 'questions input))
            (first-q (and (vectorp questions) (> (length questions) 0)
                          (aref questions 0)))
            (q-text (and first-q (alist-get 'question first-q)))
            (answer (claude-gravity--extract-ask-answer result)))
       (when q-text
         (claude-gravity--insert-label "Question: ")
         (claude-gravity--insert-wrapped q-text nil))
       (when answer
         (claude-gravity--insert-label "Answer: ")
         (claude-gravity--insert-wrapped answer nil 'claude-gravity-question)))))
  ;; Result section
  (when result
    ;; Normalize MCP-style vector results [((type . "text") (text . "..."))]
    (when (vectorp result)
      (let* ((first (and (> (length result) 0) (aref result 0)))
             (text (and (listp first) (alist-get 'text first))))
        (setq result (when text (list (cons 'stdout text))))))
    (let ((stdout (and (listp result) (alist-get 'stdout result)))
          (stderr (and (listp result) (alist-get 'stderr result)))
          (file-data (and (listp result) (alist-get 'file result)))
          (cont-prefix (claude-gravity--indent)))
      ;; Bash-style stdout
      (when (and stdout (not (string-empty-p stdout)))
        (let* ((lines (split-string stdout "\n" t))
               (nlines (length lines))
               (preview (seq-take lines 8)))
          (claude-gravity--insert-label (format "Output (%d lines):\n" nlines))
          (dolist (line preview)
            (insert cont-prefix (claude-gravity--truncate line 80) "\n"))
          (when (> nlines 8)
            (insert (propertize (concat cont-prefix "...\n") 'face 'claude-gravity-detail-label)))))
      ;; Read-style file content
      (when file-data
        (let* ((content (alist-get 'content file-data))
               (num-lines (alist-get 'numLines file-data))
               (total-lines (alist-get 'totalLines file-data)))
          (when content
            (let* ((lines (split-string content "\n"))
                   (preview (seq-take lines 6)))
              (claude-gravity--insert-label (format "Content (%d/%d lines):\n"
                                                    (or num-lines (length lines))
                                                    (or total-lines (length lines))))
              (dolist (line preview)
                (insert cont-prefix (claude-gravity--truncate line 80) "\n"))
              (when (> (length lines) 6)
                (insert (propertize (concat cont-prefix "...\n") 'face 'claude-gravity-detail-label)))))))
      ;; Stderr
      (when (and stderr (not (string-empty-p stderr)))
        (insert cont-prefix
                (propertize "Stderr:\n" 'face 'claude-gravity-stderr))
        (let ((stderr-preview (string-join (seq-take (split-string stderr "\n" t) 4) "\n")))
          (claude-gravity--insert-wrapped stderr-preview nil 'claude-gravity-stderr))))))


(defun claude-gravity--insert-tool-context (item)
  "Insert thinking and assistant text from ITEM as standalone interlaced lines.
Called before each tool item in the rendering loop."
  (let ((athink (alist-get 'assistant_thinking item))
        (atext (alist-get 'assistant_text item))
        (indent (or (* (claude-gravity--section-depth) claude-gravity--indent-step) 0)))
    ;; Trim leading/trailing whitespace
    (when athink (setq athink (string-trim athink)))
    (when atext (setq atext (string-trim atext)))
    ;; Thinking: show "┊ Thinking..." header, then content indented below
    (when (and athink (not (string-empty-p athink)))
      (let* ((margin (propertize (concat claude-gravity--margin-char " ")
                                'face claude-gravity--margin-face))
             (prefix (concat (make-string indent ?\s) margin)))
        (insert prefix (propertize "Thinking..." 'face 'claude-gravity-thinking) "\n")
        ;; Content below with plain indent (no ┊)
        (claude-gravity--insert-wrapped athink (+ indent 4) 'claude-gravity-thinking))
      (insert "\n"))
    ;; Assistant text: show with ┊ prefix on first line
    (when (and atext (not (string-empty-p atext)))
      (claude-gravity--insert-wrapped-with-margin atext nil 'claude-gravity-assistant-text)
      (insert "\n"))))


;;; Plan Revision Diff (git-gutter-style margin indicators)

(defun claude-gravity--plan-revision-diff (old-content new-content)
  "Compute line-level diff between OLD-CONTENT and NEW-CONTENT.
Returns list of (LINE-NUM . STATUS) where STATUS is `added', `modified',
or `deleted'.  Returns nil if OLD-CONTENT is nil or contents are identical."
  (when (and old-content (stringp old-content) (not (string= old-content new-content)))
    (let ((orig-file (make-temp-file "plan-rev-old"))
          (new-file (make-temp-file "plan-rev-new")))
      (unwind-protect
          (progn
            (with-temp-file orig-file (insert old-content))
            (with-temp-file new-file (insert new-content))
            (let ((diff-output
                   (with-temp-buffer
                     (call-process "diff" nil t nil "-u" orig-file new-file)
                     (buffer-string))))
              (claude-gravity--parse-unified-diff-for-margins diff-output)))
        (delete-file orig-file)
        (delete-file new-file)))))


(defun claude-gravity--parse-unified-diff-for-margins (diff-output)
  "Parse DIFF-OUTPUT (unified diff) into margin indicator data.
Returns list of (LINE-NUM . STATUS) for the new file, where STATUS is
`added', `modified', or `deleted'.

Approach: collect lines into groups.  An adjacent block of `-' lines
followed by `+' lines is a modification (the `+' lines are `modified').
A `+' block with no preceding `-' block is pure addition.  A `-' block
with no following `+' block is a deletion (marked at the next new-file
line)."
  (let ((diff-lines nil)
        (new-line 0))
    ;; Pass 1: parse into a flat list of (TYPE . NEW-LINE-NUM-OR-NIL)
    (with-temp-buffer
      (insert diff-output)
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line (buffer-substring-no-properties
                     (line-beginning-position) (line-end-position))))
          (cond
           ((string-match "^@@ -[0-9]+\\(?:,[0-9]+\\)? \\+\\([0-9]+\\)" line)
            (setq new-line (1- (string-to-number (match-string 1 line))))
            (push (cons 'hunk new-line) diff-lines))
           ((or (string-prefix-p "---" line) (string-prefix-p "+++" line)
                (string-prefix-p "diff " line) (string-prefix-p "index " line)))
           ((string-prefix-p "+" line)
            (setq new-line (1+ new-line))
            (push (cons 'add new-line) diff-lines))
           ((string-prefix-p "-" line)
            (push (cons 'del new-line) diff-lines))
           ((string-prefix-p " " line)
            (setq new-line (1+ new-line))
            (push (cons 'ctx new-line) diff-lines))))
        (forward-line 1)))
    (setq diff-lines (nreverse diff-lines))
    ;; Pass 2: walk groups and classify
    (let ((result nil)
          (lines diff-lines))
      (while lines
        (let ((entry (car lines)))
          (cond
           ;; Skip context and hunk headers
           ((memq (car entry) '(ctx hunk))
            (setq lines (cdr lines)))
           ;; Deletion group: collect consecutive del entries
           ((eq (car entry) 'del)
            (let ((del-count 0)
                  (del-pos (cdr entry)))
              (while (and lines (eq (car (car lines)) 'del))
                (setq del-count (1+ del-count))
                (setq del-pos (cdr (car lines)))
                (setq lines (cdr lines)))
              ;; Check if followed by add group → modification
              (if (and lines (eq (car (car lines)) 'add))
                  ;; Modification: mark the add lines as modified
                  (let ((add-count 0))
                    (while (and lines (eq (car (car lines)) 'add))
                      (push (cons (cdr (car lines)) 'modified) result)
                      (setq add-count (1+ add-count))
                      (setq lines (cdr lines)))
                    ;; If more lines were deleted than added, mark deletion
                    ;; at the last modified line position
                    (when (> del-count add-count)
                      (let ((last-mod-line (car (car result))))
                        (push (cons last-mod-line 'deleted) result))))
                ;; Pure deletion: mark at the next new-file line position
                ;; (del-pos is the new-file line *before* the deletion point)
                (push (cons (max 1 (1+ del-pos)) 'deleted) result))))
           ;; Pure addition group (no preceding del)
           ((eq (car entry) 'add)
            (while (and lines (eq (car (car lines)) 'add))
              (push (cons (cdr (car lines)) 'added) result)
              (setq lines (cdr lines))))
           ;; Anything else: skip
           (t (setq lines (cdr lines))))))
      ;; Deduplicate: prefer modified > added > deleted per line
      (let ((line-map (make-hash-table :test #'eql))
            (priority '((modified . 3) (added . 2) (deleted . 1))))
        (dolist (entry result)
          (let* ((ln (car entry))
                 (st (cdr entry))
                 (existing (gethash ln line-map))
                 (new-pri (alist-get st priority))
                 (old-pri (and existing (alist-get existing priority))))
            (when (or (not existing) (> (or new-pri 0) (or old-pri 0)))
              (puthash ln st line-map))))
        (let ((final nil))
          (maphash (lambda (k v) (push (cons k v) final)) line-map)
          (sort final (lambda (a b) (< (car a) (car b)))))))))


(defun claude-gravity--plan-review-apply-margin-indicators (diff-data)
  "Apply left-fringe overlays to current buffer based on DIFF-DATA.
DIFF-DATA is a list of (LINE-NUM . STATUS) from `claude-gravity--plan-revision-diff'."
  (let (overlays)
    (save-excursion
      (dolist (entry diff-data)
        (let* ((line-num (car entry))
               (status (cdr entry))
               (bitmap (pcase status
                         ('added 'claude-gravity-plan-added)
                         ('modified 'claude-gravity-plan-modified)
                         ('deleted 'claude-gravity-plan-deleted)))
               (face (pcase status
                       ('added 'claude-gravity-plan-margin-added)
                       ('modified 'claude-gravity-plan-margin-modified)
                       ('deleted 'claude-gravity-plan-margin-deleted))))
          (when (and bitmap face)
            (goto-char (point-min))
            (when (zerop (forward-line (1- line-num)))
              (let ((ov (make-overlay (line-beginning-position)
                                      (line-end-position) nil t nil)))
                (overlay-put ov 'before-string
                             (propertize "x" 'display
                                         `(left-fringe ,bitmap ,face)))
                (overlay-put ov 'claude-margin-status status)
                (overlay-put ov 'evaporate t)
                (push ov overlays)))))))
    (setq-local claude-gravity--plan-review-margin-overlays (nreverse overlays))))


(defun claude-gravity-plan-review-toggle-margins ()
  "Toggle visibility of plan revision margin indicators."
  (interactive)
  (if (null claude-gravity--plan-review-margin-overlays)
      (claude-gravity--log 'debug "No revision margin indicators in this buffer")
    (let* ((first-ov (car claude-gravity--plan-review-margin-overlays))
           (visible (overlay-get first-ov 'before-string)))
      (dolist (ov claude-gravity--plan-review-margin-overlays)
        (if visible
            (overlay-put ov 'before-string nil)
          (let* ((status (overlay-get ov 'claude-margin-status))
                 (bitmap (pcase status
                           ('added 'claude-gravity-plan-added)
                           ('modified 'claude-gravity-plan-modified)
                           ('deleted 'claude-gravity-plan-deleted)))
                 (face (pcase status
                         ('added 'claude-gravity-plan-margin-added)
                         ('modified 'claude-gravity-plan-margin-modified)
                         ('deleted 'claude-gravity-plan-margin-deleted))))
            (overlay-put ov 'before-string
                         (propertize "x" 'display
                                     `(left-fringe ,bitmap ,face))))))
      (claude-gravity--log 'debug "Plan margin indicators %s" (if visible "hidden" "shown")))))

(provide 'claude-gravity-diff)
;;; claude-gravity-diff.el ends here