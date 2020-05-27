(add-to-list 'load-path ".")
(require 'cc-mode)
(require 'array)
(require 'gudder)

(defconst jdb-prompt-regex "\n[^=]+\\[[[:digit:]]+\\] "
  "Regex to match the jdb prompt.")

(defvar *jdb-breakpoints* '()
  "Stores all currently active breakpoints for the jdb session.
Breakpoints are stored in the following format:
`(filepath . line-number)'")

(defvar *overlays-drawn* nil
  "A flag to say whether or not the debug overlays have been drawn.")

(gudder-toggle-breakpoint 'jdb 'java-mode *jdb-breakpoints*)

(defun gudder-jdb ()
  (interactive)
  (gudder-breakpoint-clearer 'jdb *jdb-breakpoints*)
  (gudder-breakpoint-listener 'jdb *jdb-breakpoints* jdb-breakpoint-output-parser)
  (gudder-bind-keys 'java-mode)
  (add-hook 'comint-output-filter-functions #'jdb-overlay-listener-filter-function)
  (call-interactively #'cd)
  (when (running-gud)
    (kill-process (running-gud)))
  (save-window-excursion
    (call-interactively #'jdb))
  ;; clear debug values
  (dolist (buffer (buffer-list))
    (gudder-clear-debug-values buffer))
  ;; reset breakpoints
  (let ((breakpoints *jdb-breakpoints*))
    (setq *jdb-breakpoints* '())
    (dolist (breakpoint breakpoints)
      (let ((filename (file-name-nondirectory (car breakpoint)))
            (line-number (cdr breakpoint)))
        (sync-process-send-string (running-gud) (concat "stop at "
                                                        (gud-find-class filename line-number) ":"
                                                        (number-to-string line-number) "\n")
                                  jdb-prompt-regex 1)))))

(defun jdb-find-function (filename function-name)
  "Return the line number where FUNCTION-NAME appears in FILENAME.
Return nil if the file or function doesn't exist."
  (when (file-exists-p filename)
    (with-temp-buffer
      (insert-file-contents-literally filename)
      (let ((line-number nil))
        (while (< (point) (point-max))
          (when (looking-at (concat "^\s*\\(public\\|private\\|protected\\)?\s*\\(static\\)?\s*[a-zA-Z0-9_]+\s+"
                                    function-name "\s*("))
            (setq line-number (1+ (current-line))))
          (forward-line))
        line-number))))

(defun jdb-breakpoint-output-parser (output-str)
  (if (or (s-matches-p "\\(^Deferring\s\\|Set\s\\)breakpoint" output-str)
          (s-matches-p "\\(^Removed:\sbreakpoint\\|^Not found: breakpoint\\)" output-str))
      ;; add or remove breakpoint
      (list
       (append
        (list (if (s-matches-p "\\(^Deferring\s\\|Set\s\\)breakpoint" output-str)
                  'add
                'remove))
        (let ((breakpoint-str (car (last (s-split "\s" (car (s-split "\n" output-str t)))))))
          (if (s-matches-p ":" breakpoint-str)
              ;; line number breakpoint
              (let* ((breakpoint-separated (s-split ":" breakpoint-str))
                     (filepath (expand-file-name (gudder-find-source (replace-regexp-in-string "\\." "/" (car breakpoint-separated))
                                                   ".java"
                                                   gud-jdb-sourcepath)))
                     (line-number (string-to-number (replace-regexp-in-string "\\." "" (cadr breakpoint-separated)))))
                (list filepath line-number))
            ;; function breakpoint
            (let* ((breakpoint-separated (s-split "\\." breakpoint-str t))
                   (filepath (gudder-find-source (-reduce (lambda (path dirname)
                                                            (concat (file-name-as-directory path)  dirname))
                                                          (without-last breakpoint-separated))
                                                 ".java"
                                                 gud-jdb-sourcepath))
                   (line-number (jdb-find-function filepath
                                                   (car (last breakpoint-separated)))))
              (list filepath line-number))))))
    '(nil nil nil)))

(cl-defun gud-find-class (f _line &optional (paths (or gud-jdb-sourcepath '("."))))
  "Return the Java fully qualified classname of source file F.
This function uses `gud-jdb-sourcepath' to find F."
  (defun gud-find-class-helper (f path partial-path)
    (if (or (null path)
            (string= f (file-name-nondirectory partial-path)))
        (s-replace-regexp "/\\|\\\\" "." (file-name-sans-extension partial-path)) ;; convert filepath to java classpath
      (when (file-directory-p path)
        (let* ((files (cddr (directory-files path :full))))
          (eval (cons 'or (mapcar (lambda (file)
                                    `(gud-find-class-helper
                                      ,f ,file
                                      ,(concat (if (string= partial-path "")
                                                   ""
                                                 (file-name-as-directory partial-path))
                                               (file-name-nondirectory file))))
                                  files)))))))
  (eval (cons 'or (mapcar (lambda (path)
                            `(gud-find-class-helper ,(file-name-nondirectory f) ,path ""))
                          (mapcar #'file-name-as-directory paths)))))

(defun jdb-overlay-listener-filter-function (output-str)
  (run-with-timer .0005 nil #'jdb-overlay-listener output-str)
  output-str)

(defun jdb-overlay-listener (output-str)
  (when (and output-str (s-index-of ", line=" output-str))
    (let ((filepath (buffer-file-name (marker-buffer gud-overlay-arrow-position)))
          (line-number (with-current-buffer (marker-buffer gud-overlay-arrow-position)
            (goto-char gud-overlay-arrow-position)
            (+ 1 (current-line))))
          (header-line (save-excursion
            (cl-loop (forward-line -1)
              (let ((start (progn (back-to-indentation) (point)))
                    (end (progn (goto-last-non-comment) (point))))
                (cond
                 ((progn (beginning-of-line)
                         (looking-at "^\s*\\(public\\|private\\|protected\\)?\s*\\(static\\)?\s*[a-zA-Z0-9_]+\s+[a-zA-Z0-9_]+\s*("))
                  (cl-return (+ 1 (current-line))))
                 ((gudder-debug-overlays (current-buffer) start (+ 1 end))
                  (cl-return nil))
                 ((= (current-line) 0)
                  (cl-return nil)))))))
          (rest-of-function-call-lines (save-excursion
            (cl-loop while (not (looking-at ".*;.*"))
                     do (forward-line 1)
                     collect (1+ (current-line))))))
      (save-window-excursion
        (with-current-buffer (find-file filepath)
          (save-excursion
            (dolist (line (-non-nil (append (list line-number header-line) rest-of-function-call-lines)))
              (goto-char (point-min))
              (forward-line (1- line))
              (let* ((start (save-excursion (back-to-indentation) (point)))
                     (end (save-excursion (goto-last-non-comment) (point)))
                     (idents (defpipeline idents
                       (buffer-substring-no-properties start end)
                       (s-split "" idents :omit-nulls)
                       (gudder-jdb-group-idents idents)
                       (s-join "" idents)
                       (without-embeded-strings idents)
                       (s-split " " idents :omit-nulls)
                       (-filter (lambda (ident) ;remove Boolean literals and numbers
                                  (not (s-matches-p "^[[:digit:].]$\\valuetrue\\valuefalse" ident)))
                                idents)
                       (mapcar (lambda (ident)
                                 (cond
                                  ((or (s-matches-p "\\[[^]]*$" ident)
                                       (s-matches-p "\\[.*(.*).*\\]" ident))
                                   (substring ident 0 (s-index-of "[" ident)))
                                  ((s-matches-p "[(]" ident)
                                   (defpipeline * ident
                                     (substring * 0 (s-index-of "(" *))
                                     (substring * 0 (-find-last-index (lambda (char)
                                                                        (char-equal char ?.))
                                                                      (string-to-list *)))))
                                  (:else ident)))
                               idents)
                       (-uniq idents)))
                     (debug-values ""))
                (gudder-clear-debug-values (current-buffer) start (+ 1 end))
                (dolist (pair (gudder-jdb-idents-values idents) debug-values)
                  (when pair
                    (setq debug-values (concat debug-values (car pair) ": " (if (<= (length (cadr pair)) gudder-max-debug-value-length)
                                                                                (cadr pair)
                                                                              (concat (substring (cadr pair) 0 gudder-max-debug-value-length) "...")) "  "))))
                (when (not (string= debug-values ""))
                  (add-debug-info-here (concat " " debug-values " "))))))))))
  (setq *overlays-drawn* t))

(cl-defun gudder-jdb-group-idents (charlst &optional (in-array 0))
  (cond
   ((null charlst) nil)
   ((or (and (= 0 in-array)
             (s-matches-p "[[:alpha:][:digit:].\"']" (car charlst)))
        (and (/= 0 in-array)
             (s-matches-p "[[:alpha:][:digit:].\"'-/+*%^&value<>?!@~\s]" (car charlst))))
    (append (list (if (s-matches-p "\s" (car charlst))
                      ""
                    (car charlst)))
            (gudder-jdb-group-idents (cdr charlst) in-array)))
   ((string= "[" (car charlst))
    (append (list (car charlst))
            (gudder-jdb-group-idents (cdr charlst) (1+ in-array))))
   ((string= "]" (car charlst))
    (append (list (car charlst))
            (gudder-jdb-group-idents (cdr charlst) (1- in-array))))
   ((s-matches-p "(" (car charlst))
    (append (list (car charlst))
            '(" ")
            (gudder-jdb-group-idents (cdr charlst) in-array)))
   (:else
    (append '(" ")
            (gudder-jdb-group-idents (cdr charlst) in-array)))))

(defun gudder-jdb-idents-values (idents)
  (if (not (running-gud))
      nil
    (-non-nil
     (mapcar (lambda (ident)
              (let ((value (jdb-value ident)))
                (cond
                 ((s-matches-p "null" value) nil) ;null value
                 ((s-matches-p "\\[[[:digit:]]+\\]" value) ;array
                  (let ((length (string-to-number (jdb-value (concat ident ".length")))))
                    (list ident (gudder-jdb-array-value ident length))))
                 (:else                 ;some primitive or has a toString()
                  (list ident value)))))
             idents))))

(defun jdb-value (ident)
  (defpipeline value ident
    (sync-process-send-string (running-gud)
                              (concat "print " value "\n") jdb-prompt-regex 1)
    (s-split "\n" value)
    (without-last value)
    (last value)
    (car value)
    (if (and value (s-index-of (concat ident " = ") value))
        (substring value (+ (s-index-of (concat ident " = ") value)
                            (length ident)
                            3))
      "null")))

(defun gudder-jdb-array-value (name length)
  (defpipeline * length
    (let ((idents '()))
      (dotimes (i * idents)
        (push (concat name "[" (number-to-string i) "]") idents)))
    (reverse *)
    (gudder-jdb-idents-values *)
    (mapcar (lambda (pair)
              (cadr pair)) *)
    (s-join ", " *)
    (concat "{" * "}")))

(provide 'gudder-jdb)
;;; gudder-jdb.el ends here
