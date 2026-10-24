;;; claude-gravity-md-table-test.el --- ERT tests for rendered markdown tables  -*- lexical-binding: t; -*-

(require 'ert)
(require 'markdown-mode)
(require 'claude-gravity-md-render)

(defun cg-md-table-test--display-strings ()
  "Return the `display' strings on table rows of the current buffer."
  (save-excursion
    (goto-char (point-min))
    (let (out)
      (while (not (eobp))
        (let ((d (get-text-property (point) 'display)))
          (when d (push d out)))
        (forward-line 1))
      (nreverse out))))

(ert-deftest cg-md-table-test--region-ending-mid-table-hides-link-markup ()
  "A font-lock region ending mid-table still hides link markup in later rows."
  (with-temp-buffer
    (let ((markdown-hide-markup t)
          (claude-gravity--markdown-render-mode t))
      (insert "Intro.\n\n| Id | Note |\n|----|------|\n")
      (dotimes (i 6)
        (insert (format "| R%d | see [D%d](#d%d-anchor) |\n" i i i)))
      (gfm-mode)
      (font-lock-set-defaults)
      (clrhash claude-gravity--md-table-cache)
      (let ((mid (save-excursion
                   (goto-char (point-min))
                   (search-forward "| R1")
                   (line-end-position))))
        (font-lock-fontify-region (point-min) mid))
      (let ((rendered (cg-md-table-test--display-strings)))
        (should (= (length rendered) 8))
        (should-not (seq-some (lambda (s) (string-match-p "](#" s)) rendered))))))

(provide 'claude-gravity-md-table-test)
;;; claude-gravity-md-table-test.el ends here
