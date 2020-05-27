(require 'gudder)

(defvar *pdb-breakpoints* '()
  "Stores all currently active breakpoints for the pdb session.
Breakpoints are stored in the following format:
(filepath . line-number)")

(gudder-breakpoint-clearer 'pdb *pdb-breakpoints*)

(defun pdb-breakpoint-output-parser (output-str)
  (if (s-matches-p "^Breakpoint\\|^Deleted breakpoint" output-str)
      (let ((infos '())
            (output-lines (s-split "\n" output-str)))
        (dolist (line output-lines infos)
          (when (s-matches-p "^Breakpoint\\|^Deleted breakpoint" line)
            (let ((breakpoint-info (s-split ":" (car (last (s-split " " line))))))
              (push (list (if (s-matches-p "^Breakpoint" output-str)
                              'add
                            'remove)
                          (car breakpoint-info)
                          (string-to-number (cadr breakpoint-info)))
                    infos))))
        infos)
    '((nil nil nil))))

(gudder-breakpoint-listener 'pdb *pdb-breakpoints* pdb-breakpoint-output-parser)

(gudder-toggle-breakpoint 'pdb 'python-mode *pdb-breakpoints*)

(provide 'gudder-pdb)
;;; gudder-pdb.el ends here
