;;; claude-gravity-text.el --- Text rendering utilities for Claude Gravity  -*- lexical-binding: t; -*-

;;; Code:

(require 'claude-gravity-core)
(require 'claude-gravity-faces)
(require 'claude-gravity-session)


;;; Divider helpers

(defun claude-gravity--section-divider (title)
  "Return a section divider string with TITLE embedded.
Produces: ── Title ──────────────────────
Use as the argument to `magit-insert-heading'."
  (let* ((prefix "── ")
         (suffix " ")
         (label-len (+ (length prefix) (length title) (length suffix)))
         (width (max 40 (- (or (window-width) 80) 2)))
         (remaining (max 3 (- width label-len)))
         (line (make-string remaining ?─)))
    (concat (propertize prefix 'face 'claude-gravity-divider)
            (propertize title 'face 'claude-gravity-section-heading)
            (propertize (concat suffix line) 'face 'claude-gravity-divider))))


(defun claude-gravity--turn-separator ()
  "Insert a thin dashed separator between turns."
  (let* ((indent (claude-gravity--indent))
         (width (max 20 (- (or (window-width) 80) (length indent) 4)))
         (line (make-string width ?╌)))
    (insert indent (propertize line 'face 'claude-gravity-divider) "\n")))


(defun claude-gravity--table-line-p (line)
  "Return non-nil if LINE looks like a markdown table row."
  (string-match-p "^\\s-*|" line))


(defun claude-gravity--box-table-line-p (line)
  "Return non-nil if LINE is a rendered box-drawing table line."
  (string-match-p "^[┌├└│─┬┼┴┤┐┘ ]" line))


(defun claude-gravity--render-markdown-table (table-lines)
  "Render TABLE-LINES (list of markdown pipe-delimited strings) as a box-drawn table."
  (let* ((data-rows
          (mapcar (lambda (line)
                    (let ((cells (split-string
                                 (string-trim line "\\s-*|" "|\\s-*") "|")))
                      (mapcar #'string-trim cells)))
                  (seq-remove (lambda (l) (string-match-p "^\\s-*|[-:|]+|" l))
                              table-lines)))
         (ncols (if data-rows (length (car data-rows)) 0))
         (widths (let ((ws (make-list ncols 0)))
                   (dolist (row data-rows ws)
                     (dotimes (i (min ncols (length row)))
                       (setf (nth i ws)
                             (max (nth i ws) (length (nth i row))))))))
         (make-sep (lambda (left mid right)
                     (concat left
                             (mapconcat (lambda (w) (make-string (+ w 2) ?─))
                                        widths mid)
                             right)))
         (fmt-row (lambda (row)
                    (concat "│"
                            (mapconcat
                             (lambda (pair)
                               (let ((cell (car pair)) (w (cdr pair)))
                                 (concat " " cell
                                         (make-string (max 0 (- w (length cell))) ?\s)
                                         " ")))
                             (cl-mapcar #'cons row widths) "│")
                            "│"))))
    (when (and data-rows (> ncols 0))
      (concat (funcall make-sep "┌" "┬" "┐") "\n"
              (funcall fmt-row (car data-rows)) "\n"
              (funcall make-sep "├" "┼" "┤") "\n"
              (mapconcat fmt-row (cdr data-rows) "\n")
              (when (cdr data-rows) "\n")
              (funcall make-sep "└" "┴" "┘")))))


(defun claude-gravity--render-tables-in-text (text)
  "Replace markdown tables in TEXT with box-drawn rendered tables."
  (let ((lines (split-string text "\n"))
        result
        table-acc)
    (dolist (line lines)
      (if (claude-gravity--table-line-p line)
          (push line table-acc)
        ;; Flush accumulated table
        (when table-acc
          (let ((rendered (claude-gravity--render-markdown-table (nreverse table-acc))))
            (if rendered
                (push rendered result)
              ;; Fallback: emit raw lines
              (dolist (tl (nreverse table-acc))
                (push tl result))))
          (setq table-acc nil))
        (push line result)))
    ;; Flush trailing table
    (when table-acc
      (let ((rendered (claude-gravity--render-markdown-table (nreverse table-acc))))
        (if rendered
            (push rendered result)
          (dolist (tl (nreverse table-acc))
            (push tl result)))))
    (mapconcat #'identity (nreverse result) "\n")))


(defvar claude-gravity--fontify-cache (make-hash-table :test 'equal :size 256)
  "Cache of fontified markdown strings.  Key is raw text, value is fontified text.")

(defvar claude-gravity--md-buffer nil
  "Persistent buffer with `markdown-mode' for fontification reuse.")

(defun claude-gravity--get-md-buffer ()
  "Return a persistent markdown-mode buffer for fontification."
  (if (and claude-gravity--md-buffer (buffer-live-p claude-gravity--md-buffer))
      claude-gravity--md-buffer
    (setq claude-gravity--md-buffer
          (with-current-buffer (get-buffer-create " *claude-gravity-md*")
            (when (fboundp 'markdown-mode) (markdown-mode))
            (current-buffer)))))

(defvar claude-gravity--wrap-cache (make-hash-table :test 'equal :size 256)
  "Cache of fill-region results.  Key is (text fill-column prefix), value is rendered string.")

(defun claude-gravity--wrap-cache-clear ()
  "Clear the fill-region wrap cache (e.g. on window resize)."
  (clrhash claude-gravity--wrap-cache))

(defun claude-gravity--fontify-cache-clear ()
  "Clear the markdown fontification cache."
  (clrhash claude-gravity--fontify-cache))

(defun claude-gravity--fontify-markdown (text)
  "Return TEXT with markdown fontification, markup hiding, and table rendering.
Renders markdown tables as box-drawn tables.
Uses `markdown-mode' if available for inline markup; returns TEXT with
tables rendered regardless.  Results are cached to avoid repeated
`markdown-mode' font-lock calls."
  (let ((text (claude-gravity--render-tables-in-text text)))
    (or (gethash text claude-gravity--fontify-cache)
        (let ((result
               (if (fboundp 'markdown-mode)
                   (with-current-buffer (claude-gravity--get-md-buffer)
                     (let ((inhibit-read-only t))
                       (erase-buffer)
                       (insert text)
                       (let ((markdown-hide-markup t))
                         (font-lock-ensure))
                       (buffer-string)))
                 text)))
          ;; Evict cache when too large
          (when (> (hash-table-count claude-gravity--fontify-cache) 512)
            (clrhash claude-gravity--fontify-cache))
          (puthash text result claude-gravity--fontify-cache)
          result))))


(defun claude-gravity--insert-wrapped-with-margin (text indent-or-nil face)
  "Insert TEXT with word-wrap and a ▎ margin indicator.
INDENT-OR-NIL and FACE work like `claude-gravity--insert-wrapped'.
The margin character inherits FACE so its color matches the content type."
  (when (and text (not (string-empty-p text)))
    (let* ((text (claude-gravity--fontify-markdown text))
           (indent (or indent-or-nil
                       (* (claude-gravity--section-depth) claude-gravity--indent-step)))
           (margin (propertize (concat claude-gravity--margin-char " ")
                              'face (or face claude-gravity--margin-face)))
           (prefix (concat (make-string indent ?\s) margin))
           (fc (max 40 (- (or (window-width) 80) 2)))
           (cache-key (list text fc prefix face))
           (cached (gethash cache-key claude-gravity--wrap-cache)))
      (if cached
          (insert cached)
        (let ((rendered
               (with-temp-buffer
                 (let ((fill-column fc)
                       (fill-prefix prefix)
                       (plain-prefix (make-string (+ indent 2) ?\s)))
                   (dolist (para (split-string text "\n"))
                     (let ((para-start (point))
                           (is-box (claude-gravity--box-table-line-p para)))
                       (insert (if is-box plain-prefix prefix) para "\n")
                       (when (and (not is-box)
                                  (> (length para) (- fc indent 2))
                                  (not (claude-gravity--table-line-p para)))
                         (fill-region para-start (point))))))
                 ;; Face is applied only to the ▎ margin char (via propertize above),
                ;; not to the text body — keeps content in default color.
                 (buffer-string))))
          (when (> (hash-table-count claude-gravity--wrap-cache) 512)
            (clrhash claude-gravity--wrap-cache))
          (puthash cache-key rendered claude-gravity--wrap-cache)
          (insert rendered))))))


(defun claude-gravity--split-margin-text (text face)
  "Split TEXT into first wrapped margin line and the rest.
Returns (FIRST-LINE . REST-STRING) where FIRST-LINE has ▎ prefix
and FACE applied.  REST-STRING contains remaining lines or nil.
Uses current section depth for indentation."
  (when (and text (stringp text) (not (string-empty-p text)))
    (let* ((text (claude-gravity--fontify-markdown text))
           (indent (* (claude-gravity--section-depth) claude-gravity--indent-step))
           (margin (propertize (concat claude-gravity--margin-char " ")
                              'face (or face claude-gravity--margin-face)))
           (prefix (concat (make-string indent ?\s) margin))
           (fc (max 40 (- (or (window-width) 80) 2)))
           (cache-key (list 'split text fc prefix face))
           (cached (gethash cache-key claude-gravity--wrap-cache)))
      (or cached
          (let* ((rendered
                  (with-temp-buffer
                    (let ((fill-column fc)
                          (fill-prefix prefix))
                      (dolist (para (split-string text "\n"))
                        (let ((para-start (point)))
                          (insert prefix para "\n")
                          (when (and (> (length para) (- fc indent 2))
                                     (not (claude-gravity--table-line-p para)))
                            (fill-region para-start (point))))))
                    (when face
                      (add-face-text-property (point-min) (point-max) face))
                    (buffer-string)))
                 (result
                  (unless (string-empty-p rendered)
                    (let ((nl-pos (string-match "\n" rendered)))
                      (if nl-pos
                          (let ((first (substring rendered 0 nl-pos))
                                (rest (substring rendered (1+ nl-pos))))
                            (cons first (if (string-empty-p rest) nil rest)))
                        (cons rendered nil))))))
            (when (> (hash-table-count claude-gravity--wrap-cache) 512)
              (clrhash claude-gravity--wrap-cache))
            (puthash cache-key result claude-gravity--wrap-cache)
            result)))))


(defun claude-gravity--insert-wrapped (text indent-or-nil &optional face)
  "Insert TEXT with word-wrap, indented by INDENT-OR-NIL spaces.
When INDENT-OR-NIL is nil, uses the current section depth for indentation.
Each paragraph is filled to fit the window width.  Optional FACE
is applied to the inserted text."
  (when (and text (not (string-empty-p text)))
    (let* ((indent (or indent-or-nil
                       (* (claude-gravity--section-depth) claude-gravity--indent-step)))
           (prefix (make-string indent ?\s))
           (start (point))
           (fill-column (max 40 (- (or (window-width) 80) 2)))
           (fill-prefix prefix))
      (dolist (para (split-string text "\n"))
        (let ((para-start (point)))
          (insert prefix para "\n")
          (when (and (> (length para) (- fill-column indent))
                     (not (claude-gravity--table-line-p para)))
            (fill-region para-start (point)))))
      (when face
        (add-face-text-property start (point) face)))))


(defun claude-gravity--insert-label (text &optional indent)
  "Insert TEXT as a detail label with INDENT spaces (default depth-based)."
  (insert (if indent
              (make-string indent ?\s)
            (claude-gravity--indent))
          (propertize text 'face 'claude-gravity-detail-label)))


(defun claude-gravity--shorten-path (path)
  "Shorten PATH to project-relative or basename."
  (if (null path) ""
    (let ((name (file-name-nondirectory path)))
      (if (string-empty-p name) path name))))


(defun claude-gravity--insert-label-value (label value &optional face)
  "Insert LABEL followed by VALUE on the same line.
LABEL gets detail-label face, VALUE gets optional FACE."
  (insert (claude-gravity--indent)
          (propertize label 'face 'claude-gravity-detail-label)
          (if face (propertize value face) value)
          "\n"))


;;; Plan display

(defun claude-gravity--update-plan (session content)
  "Update plan for SESSION from CONTENT string."
  (plist-put session :plan (list :content content))
  (claude-gravity--show-plan-buffer session))


(defun claude-gravity--show-plan-buffer (session)
  "Display the plan for SESSION in a dedicated buffer."
  (let ((plan (plist-get session :plan)))
    (when plan
      (let* ((label (claude-gravity--session-label session))
             (buf (get-buffer-create (format "*Claude Plan: %s*" label)))
             (content (plist-get plan :content)))
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert content)
            (goto-char (point-min)))
          (when (fboundp 'markdown-mode)
            (markdown-mode))
          (visual-line-mode 1)
          (setq buffer-read-only t)
          (set-buffer-modified-p nil))
        (pop-to-buffer buf)))))


(defun claude-gravity-show-plan ()
  "Show the plan for the current session."
  (interactive)
  (let* ((sid (or claude-gravity--buffer-session-id
                  (claude-gravity--current-overview-session-id)))
         (session (when sid (claude-gravity--get-session sid))))
    (if (and session (plist-get session :plan))
        (claude-gravity--show-plan-buffer session)
      (claude-gravity--log 'debug "No plan available"))))


(defun claude-gravity--current-overview-session-id ()
  "Return session-id at point in the overview buffer, or nil."
  (let ((section (magit-current-section)))
    (when (and section (eq (oref section type) 'session-entry))
      (oref section value))))


(defun claude-gravity--plan-preview-lines (content n)
  "Return first N non-empty lines from plan CONTENT."
  (let ((lines (split-string content "\n"))
        result)
    (dolist (line lines)
      (when (and (< (length result) n)
                 (not (string-empty-p (string-trim line))))
        (push line result)))
    (nreverse result)))


(defun claude-gravity--format-allowed-prompts (prompts)
  "Format PROMPTS list as \"Tool(description), ...\" string."
  (mapconcat (lambda (p)
               (format "%s(%s)"
                       (or (alist-get 'tool p) "?")
                       (or (alist-get 'prompt p) "?")))
             prompts ", "))


(defun claude-gravity-open-plan-file ()
  "Open the plan file for the current session."
  (interactive)
  (let* ((sid (or claude-gravity--buffer-session-id
                  (claude-gravity--current-overview-session-id)))
         (session (when sid (claude-gravity--get-session sid)))
         (plan (when session (plist-get session :plan)))
         (fpath (when plan (plist-get plan :file-path))))
    (cond
     ((and fpath (file-exists-p fpath)) (find-file fpath))
     (fpath (claude-gravity--log 'debug "Plan file not found: %s" fpath))
     (t (claude-gravity--log 'debug "No plan file path available")))))


(defun claude-gravity-insert-plan (session)
  "Insert plan section for SESSION with inline preview."
  (let ((plan (plist-get session :plan)))
    (when plan
      (let* ((content (plist-get plan :content))
             (file-path (plist-get plan :file-path))
             (allowed-prompts (plist-get plan :allowed-prompts))
             (preview-lines (claude-gravity--plan-preview-lines content 8))
             (all-lines (split-string content "\n" t "[ \t]"))
             (truncated (> (length all-lines) (length preview-lines))))
        (magit-insert-section (plan nil t)
          (magit-insert-heading
            (claude-gravity--section-divider "Plan"))
          (claude-gravity--insert-wrapped
           (string-join preview-lines "\n") nil)
          (when truncated
            (insert (propertize (concat (claude-gravity--indent) "...\n")
                                'face 'claude-gravity-detail-label)))
          (when allowed-prompts
            (claude-gravity--insert-label "Permissions: ")
            (claude-gravity--insert-wrapped
             (claude-gravity--format-allowed-prompts allowed-prompts) nil))
          (when file-path
            (claude-gravity--insert-label "File: ")
            (claude-gravity--insert-wrapped
             (concat file-path "  " (propertize "(F to open)" 'face 'claude-gravity-detail-label))
             nil))
          (insert (propertize (concat (claude-gravity--indent) "P to view full plan\n")
                              'face 'claude-gravity-detail-label))
          (insert "\n"))))))


(defun claude-gravity-insert-streaming-text (session)
  "Insert live streaming text section for SESSION.
Shows assistant text as it generates from the JSON-output adapter.
Only visible when :streaming-text is non-nil (during active generation)."
  (let ((text (plist-get session :streaming-text)))
    (when (and text (stringp text) (not (string-empty-p text)))
      (magit-insert-section (streaming-text nil t)
        (magit-insert-heading
          (concat
           (propertize ">>> " 'face 'claude-gravity-status-responding)
           (propertize "Claude is responding..." 'face 'claude-gravity-assistant-text)))
        (claude-gravity--insert-wrapped-with-margin text nil 'claude-gravity-assistant-text)
        (insert "\n")))))


(defun claude-gravity--format-duration (seconds)
  "Format SECONDS as a compact duration string like 12.3s or 1m23s."
  (cond
   ((< seconds 60) (format "%.1fs" seconds))
   ((< seconds 3600) (format "%dm%02ds" (truncate (/ seconds 60))
                              (truncate (mod seconds 60))))
   (t (format "%dh%02dm" (truncate (/ seconds 3600))
              (truncate (/ (mod seconds 3600) 60))))))


(defun claude-gravity--margins-to-gutter ()
  "Move inline ▎ indicators from buffer text into the left display margin.
Post-processing step called after rendering.  Finds every ▎ character,
preserves its face, and adds a `display' text property that renders
it in the left margin instead of inline.  Requires `left-margin-width'
to be set on the buffer (and window margins updated)."
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "▎" nil t)
      (let* ((start (match-beginning 0))
             (end (match-end 0))
             (face (get-text-property start 'face)))
        (put-text-property start end 'display
          `((margin left-margin)
            ,(propertize "▎" 'face (or face 'claude-gravity-margin-indicator))))))))

(provide 'claude-gravity-text)
;;; claude-gravity-text.el ends here