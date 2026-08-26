; claude-gravity-md-render.el --- Markdown rendering engine  -*- lexical-binding: t; -*-
;; ⚠️ VIBECODED EXPERIMENT — This code was generated in a single AI-assisted session.
;; It may contain bugs, design issues, or unexpected behavior. Use with caution.

(require 'claude-gravity-core)
(require 'claude-gravity-faces)
(require 'claude-gravity-session)
(require 'cl-lib)

;;;; Tables

(defun claude-gravity--table-line-p (line)
  "Return non-nil if LINE looks like a markdown table row."
  (string-match-p "^\\s-*|" line))

(defun claude-gravity--box-table-line-p (line)
  "Return non-nil if LINE is a rendered box-drawing table line."
  (string-match-p "^[┌├└│─┬┼┴┤┐┘ ]" line))

(defun claude-gravity--render-markdown-table (table-lines)
  "Render TABLE-LINES as box-drawn ASCII table."
  (let* ((data-rows
          (mapcar
           (lambda (line)
             (let ((cells (split-string
                           (string-trim line "\\s-*|" "|\\s-*") "|")))
               (mapcar #'string-trim cells)))
           (seq-remove
            (lambda (l) (string-match-p "^\\s-*|[ \t-:|]+|" l))
            table-lines)))
         (ncols (if data-rows (length (car data-rows)) 0)))
    (when (and data-rows (> ncols 0))
      (let* ((display-width
              (lambda (cell)
                (length (replace-regexp-in-string
                         "\\*\\*\\(.*?\\)\\*\\*\\|\\*\\(.*?\\)\\*\\|`\\(.*?\\)`\\|_\\(.*?\\)_"
                         (lambda (m)
                           (or (match-string 1 m) (match-string 2 m)
                               (match-string 3 m) (match-string 4 m) ""))
                         cell))))
             (widths
              (let ((ws (make-list ncols 0)))
                (dolist (row data-rows ws)
                  (dotimes (i (min ncols (length row)))
                    (setf (nth i ws)
                          (max (nth i ws) (funcall display-width (nth i row))))))))
             (available (- (or (window-width) 80) 4))
             (overhead (+ ncols 1 (* ncols 2)))
             (content-budget (- available overhead))
             (content-total (apply '+ widths))
             (widths
              (if (or (<= content-total content-budget) (<= content-budget 0))
                  widths
                (let ((min-col 6))
                  (mapcar (lambda (w)
                            (max min-col (min w (/ (* w content-budget) content-total))))
                          widths))))
             (strip-markup
              (lambda (cell)
                (replace-regexp-in-string
                 "\\*\\*\\(.*?\\)\\*\\*\\|\\*\\(.*?\\)\\*\\|`\\(.*?\\)`\\|_\\(.*?\\)_"
                 (lambda (m)
                   (or (match-string 1 m) (match-string 2 m)
                       (match-string 3 m) (match-string 4 m) ""))
                 cell)))
             (wrap-cell
              (lambda (cell w)
                (let ((dw (funcall display-width cell)))
                  (if (<= dw w)
                      (list cell)
                    (let* ((stripped (funcall strip-markup cell))
                           (words (split-string stripped " +" t))
                           (lines nil)
                           (current ""))
                      (dolist (word words)
                        (let ((sep-len (if (string-empty-p current) 0 1)))
                          (cond
                           ((<= (+ (length current) sep-len (length word)) w)
                            (setq current (if (string-empty-p current) word
                                            (concat current " " word))))
                           ((> (length word) w)
                            (unless (string-empty-p current)
                              (push current lines)
                              (setq current ""))
                            (let ((s word))
                              (while (> (length s) w)
                                (push (substring s 0 w) lines)
                                (setq s (substring s w)))
                              (setq current s)))
                           (t
                            (push current lines)
                            (setq current word)))))
                      (unless (string-empty-p current)
                        (push current lines))
                      (or (nreverse lines) (list "")))))))
             (make-sep
              (lambda (left mid right)
                (concat left
                        (mapconcat (lambda (w) (make-string (+ w 2) ?─)) widths mid)
                        right)))
             (fmt-row
              (lambda (row)
                (let* ((wrapped (cl-mapcar (lambda (cell w) (funcall wrap-cell cell w))
                                           row widths))
                       (row-height (apply #'max 1 (mapcar #'length wrapped))))
                  (mapconcat
                   (lambda (line-idx)
                     (concat "│"
                             (mapconcat
                              (lambda (pair)
                                (let* ((lines (car pair))
                                       (w (cdr pair))
                                       (line (or (nth line-idx lines) "")))
                                  (concat " " line
                                          (make-string
                                           (max 0 (- w (funcall display-width line)))
                                           ?\s)
                                          " ")))
                              (cl-mapcar #'cons wrapped widths) "│")
                             "│"))
                   (number-sequence 0 (1- row-height))
                   "\n")))))
        (concat (funcall make-sep "┌" "┬" "┐") "\n"
                (funcall fmt-row (car data-rows)) "\n"
                (funcall make-sep "├" "┼" "┤") "\n"
                (mapconcat fmt-row (cdr data-rows) "\n")
                (when (cdr data-rows) "\n")
                (funcall make-sep "└" "┴" "┘"))))))

(defun claude-gravity--render-tables-in-text (text)
  "Replace markdown tables in TEXT with box-drawn rendered tables."
  (let ((lines (split-string text "\n"))
        result table-acc)
    (dolist (line lines)
      (if (claude-gravity--table-line-p line)
          (push line table-acc)
        (when table-acc
          (let ((rendered (claude-gravity--render-markdown-table (nreverse table-acc))))
            (push (or rendered (string-join (nreverse table-acc) "\n")) result))
          (setq table-acc nil))
        (push line result)))
    (when table-acc
      (let ((rendered (claude-gravity--render-markdown-table (nreverse table-acc))))
        (push (or rendered (string-join (nreverse table-acc) "\n")) result)))
    (string-join (nreverse result) "\n")))

;;;; Edit-buffer table rendering

;; Parsing is delegated to markdown-mode throughout: it is escape- and
;; code-span-aware, honours the alignment row, and measures width with
;; hidden markup removed.

(defvar claude-gravity--md-table-min-column 6
  "Minimum display width of a rendered table column.")

(defun claude-gravity--md-split-row (line)
  "Split propertized table LINE into cells, keeping text properties.
Mirrors `markdown--table-line-to-columns', which discards them: bars
that are escaped, inside inline code, or part of a wiki link do not
split a cell."
  (with-temp-buffer
    (insert line)
    (goto-char (point-min))
    (let ((cur (point)) cells)
      (while (re-search-forward "\\s-*\\(|\\)\\s-*" nil t)
        (let ((bar (match-beginning 1)))
          (cond
           ((markdown--first-column-p bar)
            (setq cur (match-end 0)))
           ((eq (char-before bar) ?\\)
            (goto-char (match-end 1)))
           ((markdown--face-p bar '(markdown-inline-code-face))
            (goto-char (match-end 1)))
           ((markdown--thing-at-wiki-link bar))
           (t
            (push (buffer-substring cur (match-beginning 0)) cells)
            (setq cur (match-end 0))))))
      (when (< cur (point-max))
        (push (buffer-substring cur (point-max)) cells))
      (nreverse cells))))

(defun claude-gravity--md-table-cells (line)
  "Split propertized table LINE into trimmed cell strings.
Splitting happens before hidden markup is removed, so an escaped bar
still reads as an escape; widths then match what is displayed."
  (mapcar (lambda (cell)
            (string-trim (markdown--remove-invisible-markup cell)))
          (claude-gravity--md-split-row line)))

(defun claude-gravity--md-table-fit-widths (widths budget)
  "Cap WIDTHS so their sum fits BUDGET, shrinking the widest columns first.
Columns narrower than the cap keep their natural width."
  (if (or (<= budget 0) (<= (apply #'+ widths) budget))
      widths
    (let ((lo claude-gravity--md-table-min-column)
          (hi (apply #'max widths))
          (cap claude-gravity--md-table-min-column))
      (while (<= lo hi)
        (let* ((mid (/ (+ lo hi) 2))
               (sum (apply #'+ (mapcar (lambda (w) (min w mid)) widths))))
          (if (<= sum budget)
              (setq cap mid lo (1+ mid))
            (setq hi (1- mid)))))
      (mapcar (lambda (w) (min w cap)) widths))))

(defun claude-gravity--md-wrap-cell (cell width)
  "Wrap CELL to WIDTH columns, returning a list of display lines."
  (if (<= (string-width cell) width)
      (list cell)
    (let (lines (current ""))
      (dolist (word (split-string cell "[ \t]+" t))
        (let ((candidate (if (string-empty-p current) word
                           (concat current " " word))))
          (cond
           ((<= (string-width candidate) width)
            (setq current candidate))
           ((> (string-width word) width)
            (unless (string-empty-p current)
              (push current lines)
              (setq current ""))
            (let ((rest word))
              (while (> (string-width rest) width)
                (let ((head (truncate-string-to-width rest width)))
                  (push head lines)
                  (setq rest (substring rest (length head)))))
              (setq current rest)))
           (t
            (push current lines)
            (setq current word)))))
      (unless (string-empty-p current)
        (push current lines))
      (or (nreverse lines) (list "")))))

(defun claude-gravity--md-pad-cell (text width align)
  "Pad TEXT to WIDTH columns according to ALIGN (`l', `r', `c' or `d')."
  (let ((pad (max 0 (- width (string-width text)))))
    (pcase align
      ('r (concat (make-string pad ?\s) text))
      ('c (let ((left (/ pad 2)))
            (concat (make-string left ?\s) text
                    (make-string (- pad left) ?\s))))
      (_ (concat text (make-string pad ?\s))))))

(defun claude-gravity--md-table-rule (widths left mid right)
  "Return a horizontal rule for WIDTHS using LEFT, MID and RIGHT corners."
  (concat left
          (mapconcat (lambda (w) (make-string (+ w 2) ?─)) widths mid)
          right))

(defun claude-gravity--md-render-width ()
  "Return the column budget available for a rendered table."
  (let ((win (get-buffer-window (current-buffer) t)))
    (max 24 (- (if win (window-body-width win) 80) 2))))

(defun claude-gravity--md-format-row (row widths aligns ncols)
  "Format ROW as one or more box lines, wrapping cells to WIDTHS."
  (let* ((cells (cl-loop for i from 0 below ncols
                         collect (claude-gravity--md-wrap-cell
                                  (or (nth i row) "") (nth i widths))))
         (height (apply #'max 1 (mapcar #'length cells))))
    (mapconcat
     (lambda (li)
       (concat "│"
               (mapconcat
                (lambda (i)
                  (concat " "
                          (claude-gravity--md-pad-cell
                           (or (nth li (nth i cells)) "")
                           (nth i widths) (nth i aligns))
                          " "))
                (number-sequence 0 (1- ncols)) "│")
               "│"))
     (number-sequence 0 (1- height)) "\n")))

(defun claude-gravity--md-parse-table (table-lines)
  "Parse TABLE-LINES into (ITEMS . FMTSPEC).
Each element of ITEMS is either the symbol `rule' or a list of cells."
  (let (fmtspec items)
    (dolist (line table-lines)
      (if (markdown--is-delimiter-row line)
          (progn (setq fmtspec (or fmtspec line))
                 (push 'rule items))
        (push (claude-gravity--md-table-cells line) items)))
    (cons (nreverse items) fmtspec)))

(defun claude-gravity--md-build-table (items fmtspec width)
  "Build one display string per element of ITEMS, capped to WIDTH columns.
The top and bottom rules are attached to the first and last lines so
that every source line keeps its own display string and stays
individually navigable."
  (let* ((rows (seq-remove (lambda (x) (eq x 'rule)) items))
         (ncols (if rows (apply #'max (mapcar #'length rows)) 0)))
    (when (> ncols 0)
      (let* ((aligns (let ((a (markdown-table-colfmt fmtspec)))
                       (append a (make-list (max 0 (- ncols (length a))) 'd))))
             (widths (let ((ws (make-list ncols 1)))
                       (dolist (row rows ws)
                         (dotimes (i ncols)
                           (setf (nth i ws)
                                 (max (nth i ws)
                                      (string-width (or (nth i row) ""))))))))
             (widths (claude-gravity--md-table-fit-widths
                      widths (- width (1+ ncols) (* 2 ncols))))
             (top (claude-gravity--md-table-rule widths "┌" "┬" "┐"))
             (mid (claude-gravity--md-table-rule widths "├" "┼" "┤"))
             (bot (claude-gravity--md-table-rule widths "└" "┴" "┘"))
             (last (1- (length items)))
             (idx -1))
        (mapcar
         (lambda (item)
           (setq idx (1+ idx))
           (concat (when (= idx 0) (concat top "\n"))
                   (if (eq item 'rule)
                       mid
                     (claude-gravity--md-format-row item widths aligns ncols))
                   (when (= idx last) (concat "\n" bot))))
         items)))))

(defvar claude-gravity--md-table-cache (make-hash-table :test 'equal :size 128)
  "Rendered table lines, keyed by width and parsed cell content.
Keying on parsed content rather than raw text means a table first seen
before `markdown-hide-markup' properties were applied re-renders once
they are, instead of caching the unstripped version forever.")

(defun claude-gravity--md-buffer-lines (beg end)
  "Return the propertized buffer lines between BEG and END."
  (save-excursion
    (goto-char beg)
    (let (acc)
      (while (< (point) end)
        (push (buffer-substring (line-beginning-position) (line-end-position)) acc)
        (forward-line 1))
      (nreverse acc))))

(defun claude-gravity--md-table-display-lines (beg end width)
  "Return per-line display strings for the table between BEG and END, or nil.
The cache is consulted before parsing, which is the expensive step.
HIDDEN is part of the key so a table first rendered before font-lock
applied its invisibility properties re-renders once they arrive."
  (let* ((raw (buffer-substring-no-properties beg end))
         (hidden (and (text-property-any beg end 'invisible 'markdown-markup) t))
         (key (list width hidden raw))
         (hit (gethash key claude-gravity--md-table-cache)))
    (cond
     ((eq hit 'none) nil)
     (hit hit)
     (t (let* ((parsed (claude-gravity--md-parse-table
                        (claude-gravity--md-buffer-lines beg end)))
               (built (claude-gravity--md-build-table
                       (car parsed) (cdr parsed) width)))
          (when (> (hash-table-count claude-gravity--md-table-cache) 256)
            (clrhash claude-gravity--md-table-cache))
          (puthash key (or built 'none) claude-gravity--md-table-cache)
          built)))))

(defun claude-gravity--md-table-bounds ()
  "Return (BEG . END) of the whole table at point, or nil.
Resolving the real start matters: font-lock hands out chunks, and a
chunk beginning mid-table would otherwise render a fragment."
  (when (markdown-table-at-point-p)
    (save-excursion
      (while (and (not (bobp))
                  (save-excursion (forward-line -1) (markdown-table-at-point-p)))
        (forward-line -1))
      (let ((beg (line-beginning-position)))
        (while (and (not (eobp)) (markdown-table-at-point-p))
          (forward-line 1))
        (cons beg (point))))))

;;;; Mermaid

(defvar claude-gravity--mermaid-cache
  (make-hash-table :test 'equal :size 128))

(defvar claude-gravity--mermaid-render-timeout 5)
(defvar claude-gravity--mermaid-render-port 9876)

(defun claude-gravity--mermaid-block-at-point-p ()
  "Return (BEG . END) if point is inside a mermaid code block."
  (save-excursion
    (save-match-data
      (let ((case-fold-search t))
        (when (re-search-backward "^\\s-*```\\s-*mermaid" nil t)
          (let ((beg (match-end 0)))
            (when (re-search-forward "^\\s-*```" nil t)
              (cons beg (match-beginning 0)))))))))

(defun claude-gravity--mermaid-extract-source (beg end)
  "Extract mermaid source from BEG to END, excluding fence markers."
  (string-trim (buffer-substring-no-properties beg end)))

(cl-defun claude-gravity--render-mermaid-to-ascii
  (source &key (use-ascii nil) (timeout claude-gravity--mermaid-render-timeout))
  "Render SOURCE mermaid diagram to ASCII art."
  (let* ((cache-key (list source use-ascii))
         (cached (gethash cache-key claude-gravity--mermaid-cache)))
    (if cached
        cached
      (let* ((result (claude-gravity--render-mermaid-via-rpc source
                                                             :use-ascii use-ascii
                                                             :timeout timeout))
             (final-result (if (string-match-p "\\`\\[Mermaid:" result)
                               result
                             result)))
        (when (> (hash-table-count claude-gravity--mermaid-cache) 256)
          (clrhash claude-gravity--mermaid-cache))
        (puthash cache-key final-result claude-gravity--mermaid-cache)
        final-result))))

(cl-defun claude-gravity--render-mermaid-via-rpc
  (source &key (use-ascii nil) (timeout 5))
  "Call gravity-server JSON-RPC to render SOURCE mermaid diagram."
  (claude-gravity--log 'debug "Rendering mermaid diagram (len=%d)" (length source))
  (let* ((json-payload
          (json-serialize
           `(:jsonrpc "2.0" :id 1 :method "renderMermaid"
             :params (:source ,source
                      :options (:useAscii ,(if use-ascii t :false))))))
         (result nil))
    (condition-case ex
        (let ((proc (make-network-process
                     :name "gravity-mermaid"
                     :host "127.0.0.1"
                     :service claude-gravity--mermaid-render-port
                     :family 'ipv4
                     :filter (lambda (_proc output)
                               (setq result (concat result output))))))
          (set-process-query-on-exit-flag proc nil)
          (process-send-string proc (concat json-payload "\n"))
          (let ((start (float-time))
                (check-interval 0.05))
            (while (and (< (- (float-time) start) timeout)
                        (process-live-p proc)
                        (not (and result
                                  (string-suffix-p "\n" result))))
              (sleep-for check-interval))
            (when (process-live-p proc)
              (delete-process proc)))
          (if (null result)
              (format "[Mermaid render error: No response from gravity-server (port %d)]"
                      claude-gravity--mermaid-render-port)
            (let* ((parsed (json-parse-string result :object-type 'alist))
                   (err (alist-get 'error parsed))
                   (res (alist-get 'result parsed)))
              (cond
               (err (format "[Mermaid render error: %s]"
                            (or (alist-get 'message err) err)))
               ((stringp res) res)
               (t "[Mermaid render error: Failed to parse RPC response]")))))
      (error
       (format "[Mermaid render error: %s]" (error-message-string ex))))))

(cl-defun claude-gravity--render-mermaid-blocks-in-text
  (text &key (use-ascii nil) (render nil))
  "Replace mermaid code blocks in TEXT with ASCII rendering."
  (if (not render)
      text
    (replace-regexp-in-string
     "\\(`\\{3\\}\\s*mermaid\\n\\([\\s\\S]*?\\)\\n\\s*`\\{3\\}\\)"
     (lambda (match)
       (let* ((source (string-trim (match-string 2 match)))
              (ascii (claude-gravity--render-mermaid-to-ascii source :use-ascii use-ascii)))
         (concat "\n" ascii "\n")))
     text 'literal)))

;;;; Fontification for markdown-mode

(defvar claude-gravity--markdown-render-mode nil)

(defun claude-gravity--md-fontify-mermaid-block (limit)
  "Font-lock function: render mermaid code blocks from point to LIMIT."
  (when (re-search-forward "```mermaid" limit t)
    (let ((fence-start (match-beginning 0)))
      (when (re-search-forward "^\\s-*```" limit t)
        (let* ((fence-end (match-end 0))
               (source-beg (save-excursion (goto-char fence-start) (forward-line) (point)))
               (source-end (match-beginning 0))
               (source (claude-gravity--mermaid-extract-source source-beg source-end)))
          (font-lock-append-text-property fence-start fence-end 'face 'markdown-markup-face)
          (when (and markdown-hide-markup claude-gravity--markdown-render-mode (not (string-empty-p source)))
            (let* ((ascii (claude-gravity--render-mermaid-to-ascii source))
                   (display-str (if (string-match-p "\\`\\[Mermaid:" ascii)
                                    (concat "\n" source "\n")
                                  (concat "\n" ascii "\n"))))
              (add-text-properties fence-start fence-end `(display ,display-str))))
          t)))))

(defun claude-gravity--md-fontify-rendered-table (limit)
  "Font-lock matcher: render the markdown table at point, up to LIMIT."
  (when (re-search-forward "|" limit t)
    (let ((bounds (claude-gravity--md-table-bounds)))
      (if (null bounds)
          (progn (forward-line 1) t)
        (let ((beg (car bounds))
              (end (cdr bounds)))
          (font-lock-append-text-property beg end 'face 'markdown-table-face)
          (when (and markdown-hide-markup claude-gravity--markdown-render-mode)
            (let ((rendered (claude-gravity--md-table-display-lines
                             beg end (claude-gravity--md-render-width))))
              (when rendered
                (save-excursion
                  (goto-char beg)
                  (dolist (str rendered)
                    (when (and str (< (point) end))
                      (add-text-properties (line-beginning-position)
                                           (line-end-position)
                                           `(display ,str)))
                    (forward-line 1))))))
          (goto-char end)
          t)))))

(defun claude-gravity-setup-md-render ()
  "Enable gravity markdown rendering extensions for markdown-mode."
  (setq claude-gravity--markdown-render-mode t)
  (font-lock-add-keywords
   nil
   '(("^\\s-*```mermaid\\b" (0 'markdown-markup-face 'prepend))
     (claude-gravity--md-fontify-mermaid-block)
     (claude-gravity--md-fontify-rendered-table))
   'append)
  (add-hook 'window-size-change-functions
            #'claude-gravity--md-schedule-resize-check)
  (claude-gravity--log 'debug "gravity md render enabled"))

(defun claude-gravity-teardown-md-render ()
  "Disable gravity markdown rendering extensions."
  (setq claude-gravity--markdown-render-mode nil)
  (font-lock-remove-keywords
   nil
   '(("^\\s-*```mermaid\\b" (0 'markdown-markup-face 'prepend))
     (claude-gravity--md-fontify-mermaid-block)
     (claude-gravity--md-fontify-rendered-table)))
  (clrhash claude-gravity--mermaid-cache)
  (claude-gravity--log 'debug "gravity md render disabled"))

(add-hook 'markdown-mode-hook #'claude-gravity-setup-md-render)

;;;; Window resize -> re-render tables

(defvar-local claude-gravity--md-last-render-width nil
  "Window width used the last time this buffer was font-locked.")

(defvar claude-gravity--md-resize-timer nil
  "Pending idle timer coalescing window-size-change events.")

(defun claude-gravity--md-schedule-resize-check (&rest _)
  "Debounce window-size-change events into a single idle-time check."
  (when (timerp claude-gravity--md-resize-timer)
    (cancel-timer claude-gravity--md-resize-timer))
  (setq claude-gravity--md-resize-timer
        (run-with-idle-timer 0.2 nil #'claude-gravity--md-process-resize)))

(defun claude-gravity--md-process-resize ()
  "Re-render tables in markdown buffers whose window changed width."
  (setq claude-gravity--md-resize-timer nil)
  (walk-windows
   (lambda (win)
     (let ((buf (window-buffer win)))
       (when (buffer-live-p buf)
         (with-current-buffer buf
           (when (and claude-gravity--markdown-render-mode
                      (derived-mode-p 'markdown-mode))
             (let ((new-width (window-body-width win)))
               (unless (eql new-width claude-gravity--md-last-render-width)
                 (setq claude-gravity--md-last-render-width new-width)
                 (font-lock-flush))))))))
   nil 'visible))

;;;; Cache management

(defvar claude-gravity--fontify-cache (make-hash-table :test 'equal :size 256))
(defvar claude-gravity--wrap-cache (make-hash-table :test 'equal :size 256))

(defun claude-gravity-md-render-clear-cache ()
  "Clear all rendering caches."
  (clrhash claude-gravity--mermaid-cache)
  (clrhash claude-gravity--fontify-cache)
  (clrhash claude-gravity--wrap-cache)
  (claude-gravity--log 'debug "MD render caches cleared"))

;;;; Fontification for gravity buffers

(defvar claude-gravity--md-buffer nil)

(defun claude-gravity--get-md-buffer ()
  "Return a persistent markdown-mode buffer for fontification."
  (if (and claude-gravity--md-buffer (buffer-live-p claude-gravity--md-buffer))
      claude-gravity--md-buffer
    (setq claude-gravity--md-buffer
          (with-current-buffer (get-buffer-create " *claude-gravity-md*")
            (when (fboundp 'markdown-mode) (markdown-mode))
            (current-buffer)))))

(defun claude-gravity--fontify-markdown (text)
  "Return TEXT with markdown fontification and table rendering."
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
          (when (> (hash-table-count claude-gravity--fontify-cache) 512)
            (clrhash claude-gravity--fontify-cache))
          (puthash text result claude-gravity--fontify-cache)
          result))))

(provide 'claude-gravity-md-render)
;;; claude-gravity-md-render.el ends here
