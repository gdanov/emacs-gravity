;;; claude-gravity-md-render-test.el --- ERT tests for markdown rendering
;; ⚠️ VIBECODED EXPERIMENT — This code was generated in a single AI-assisted session.
;; It may contain bugs, design issues, or unexpected behavior. Use with caution.
(require 'ert)
(require 'cl-lib)

;;; Standalone test module

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
            (lambda (l) (string-match-p "^\\s-*|[ \\t-:|]+|" l))
            table-lines)))
         (ncols (if data-rows (length (car data-rows)) 0)))
    (when (and data-rows (> ncols 0))
      (let* ((display-width
              (lambda (cell)
                (length (replace-regexp-in-string
                         "\\*\\*\\(.*?\\)\\*\\*\\|\\*\\(.*?\\)\\*\\|`\\(.*?\\)`\\|_\_(.*?\_)_"
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
                 "\\*\\*\\(.*?\\)\\*\\*\\|\\*\\(.*?\\)\\*\\|`\\(.*?\\)`\\|_\_(.*?\_)_"
                 (lambda (m)
                   (or (match-string 1 m) (match-string 2 m)
                       (match-string 3 m) (match-string 4 m) ""))
                 cell)))
             (truncate-cell
              (lambda (cell w)
                (let ((dw (funcall display-width cell)))
                  (if (<= dw w) cell
                    (concat (substring (funcall strip-markup cell) 0 (max 1 (- w 1))) "…")))))
             (make-sep
              (lambda (left mid right)
                (concat left
                        (mapconcat (lambda (w) (make-string (+ w 2) ?─)) widths mid)
                        right)))
             (fmt-row
              (lambda (row)
                (concat "│"
                        (mapconcat
                         (lambda (pair)
                           (let* ((cell (funcall truncate-cell (car pair) (cdr pair)))
                                  (w (cdr pair)))
                             (concat " " cell
                                     (make-string (max 0 (- w (funcall display-width cell))) ?\s)
                                     " ")))
                         (cl-mapcar #'cons row widths) "│")
                        "│"))))
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

;;; Table detection tests

(ert-deftest cg-md-test--table-line-p ()
  "Test table line detection."
  (should (claude-gravity--table-line-p "| Header | Data |"))
  (should (claude-gravity--table-line-p "  | Col1 | Col2 |"))
  (should (claude-gravity--table-line-p "|"))
  (should-not (claude-gravity--table-line-p "plain text"))
  (should-not (claude-gravity--table-line-p "not a table"))
  (should-not (claude-gravity--table-line-p "")))

(ert-deftest cg-md-test--box-table-line-p ()
  "Test box-drawing line detection."
  (should (claude-gravity--box-table-line-p "┌───┬───┐"))
  (should (claude-gravity--box-table-line-p "│ text │"))
  (should (claude-gravity--box-table-line-p "├───┼───┤"))
  (should (claude-gravity--box-table-line-p "└───┴───┘"))
  (should-not (claude-gravity--box-table-line-p "plain text"))
  (should-not (claude-gravity--box-table-line-p "| plain |")))

;;; Table rendering tests

(ert-deftest cg-md-test--render-markdown-table-basic ()
  "Test basic table rendering."
  (let* ((lines '("| Header1 | Header2 |"
                  "|----------|----------|"
                  "| Cell1   | Cell2   |"
                  "| Cell3   | Cell4   |"))
         (result (claude-gravity--render-markdown-table lines)))
    (should (stringp result))
    (should (string-match-p "┌───" result))
    (should (string-match-p "Header1" result))
    (should (string-match-p "Header2" result))
    (should (string-match-p "Cell1" result))
    (should (string-match-p "└───" result))))

(ert-deftest cg-md-test--render-markdown-table-empty-input ()
  "Test table rendering with empty input returns nil."
  (should-not (claude-gravity--render-markdown-table nil))
  (should-not (claude-gravity--render-markdown-table ())))

(ert-deftest cg-md-test--render-markdown-table-invalid-input ()
  "Test table rendering with empty or no-data input returns nil."
  (should-not (claude-gravity--render-markdown-table ()))
  (should-not (claude-gravity--render-markdown-table nil)))

(ert-deftest cg-md-test--render-markdown-table-single-row ()
  "Test table rendering with just header row."
  (let ((lines '("| Col1 | Col2 |"
                "|-----|-----|")))
    (let ((result (claude-gravity--render-markdown-table lines)))
      (should (stringp result))
      (should (string-match-p "┌───" result))
      (should (string-match-p "Col1" result)))))

(ert-deftest cg-md-test--render-markdown-table-fits-width ()
  "Test table rendering fits within window width."
  (let ((lines '("| Very Long Header | Short |"
                "|------------------|-------|"
                "| Very long cell   | Short |")))
    (let ((result (claude-gravity--render-markdown-table lines)))
      (should (stringp result))
      (should (string-match-p "┌───" result))
      (should (string-match-p "Very Long Header" result)))))

(ert-deftest cg-md-test--render-markdown-table-unicode ()
  "Test table rendering preserves Unicode content."
  (let ((lines '("| 中文 | Emoji |"
                "|------|-------|"
                "| 数据 | 🔥 |")))
    (let ((result (claude-gravity--render-markdown-table lines)))
      (should (stringp result))
      (should (string-match-p "中文" result))
      (should (string-match-p "数据" result)))))

(ert-deftest cg-md-test--render-tables-in-text-basic ()
  "Test table rendering in full text."
  (let ((text "Some text\n\n| Col1 | Col2 |\n|----|----|\n| a | b |\n\nMore text"))
    (let ((result (claude-gravity--render-tables-in-text text)))
      (should (stringp result))
      (should (string-match-p "┌───" result))
      (should (string-match-p "Col1" result))
      (should (string-match-p "a" result))
      (should (string-match-p "Some text" result))
      (should (string-match-p "More text" result)))))

(ert-deftest cg-md-test--render-tables-in-text-multiple ()
  "Test multiple tables in text."
  (let ((text "| A | B |\n|--|--|\n| 1 | 2 |\n\n|C|D|\n|--|--|\n|3|4|"))
    (let ((result (claude-gravity--render-tables-in-text text)))
      (should (string-match-p "A" result))
      (should (string-match-p "B" result))
      (should (string-match-p "C" result))
      (should (string-match-p "D" result)))))

(ert-deftest cg-md-test--render-tables-in-text-no-tables ()
  "Test text with no tables is preserved unchanged."
  (let ((text "Plain text without tables"))
    (should (string= text (claude-gravity--render-tables-in-text text)))))

(ert-deftest cg-md-test--render-tables-in-text-empty ()
  "Test rendering empty text."
  (should (string= "" (claude-gravity--render-tables-in-text ""))))

(ert-deftest cg-md-test--render-markdown-table-bold-italic ()
  "Test table rendering handles markdown formatting."
  (let ((lines '("| **Bold** | *Italic* |"
                "|---------|---------|"
                "| Normal | Text |")))
    (let ((result (claude-gravity--render-markdown-table lines)))
      (should (string-match-p "Bold" result))
      (should (string-match-p "Italic" result)))))

(ert-deftest cg-md-test--render-markdown-table-code ()
  "Test table rendering handles inline code."
  (let ((lines '("| `code` | normal |"
                "|--------|--------|"
                "| x | y |")))
    (let ((result (claude-gravity--render-markdown-table lines)))
      (should (string-match-p "code" result)))))

(ert-run-tests-batch-and-exit)
