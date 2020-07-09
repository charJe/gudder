(require 'cc-mode) ;for java-mode stuff
(require 'array)
(require 'gudder)

(defconst gudder:jdb-prompt-regexp "^> \\|^[^ ]+\\[[0-9]+\\] "
  "Regexp for detecting the jdb prompt")

(defconst gudder:jdb-step-complete-regexp "Breakpoint hit:\\|Step completed:"
  "Regexp for detecting that a step or continut has been completed.")

;;;###autoload
(defun gudder:jdb (directory command-line)
  "Call `jdb' the Gudder way.
-sourcepath and -classpath should have white space between them and their path
lists (: delimited) just like normal."
  (interactive
   (list (expand-file-name (read-directory-name "Run debugger from: ")) ;directory
         nil))
  (setq command-line
        (gud-query-cmdline 'jdb
                           (concat "-launch -sourcepath " directory " -classpath " directory " ")))
  (gudder:bind-keys java-mode)
  (gudder:breakpoint-setup jdb gudder:jdb-breakpoint-handler)
  (gudder:debug-info-setup jdb gudder:jdb-overlay-subhandler
                           gudder:jdb-step-complete-regexp)
  (save-window-excursion
    ;; kill existing process
    (let ((gud (gudder:running-gud)))
      (when gud (kill-buffer (process-buffer gud))))
    ;; start gudebugger
    (with-current-buffer (find-file directory)
    (jdb (replace-regexp-in-string "-sourcepath[[:space:]]+" "-sourcepath"
                                   (replace-regexp-in-string "-classpath[[:space:]]+" "-classpath"
                                                             command-line)))
    )))
;; Reusing default handlers. Trigger can be testing like
;; (jdb-invalidate-breakpoints 'update)
(def-gdb-auto-update-trigger
  jdb-invalidate-breakpoints "clear"
  'gdb-breakpoints-list-handler
  '(start update))
(def-gdb-auto-update-trigger
  jdb-invalidate-threads "threads"
  'gdb-thread-list-handler
  '(start update update-threads))
(def-gdb-auto-update-trigger
  jdb-invalidate-frames "where"
  'gdb-stack-list-frames-handler
  '(start update))
(def-gdb-auto-update-trigger
  jdb-invalidate-locals "locals"
  'gdb-locals-handler
  '(start update))

(defun gudder:jdb-breakpoint-handler (output-str)
  "See `gudder:breakpoint-setup'."
  (mapcar
   (lambda (line)
     (if (or (s-matches-p "\\(Deferring\s\\|Set\s\\)breakpoint" output-str)
             (s-matches-p "\\(Removed:\sbreakpoint\\|Not found: breakpoint\\)" output-str))
         ;; add or remove breakpoint
         (cons
          (if (s-matches-p "\\(Deferring\s\\|Set\s\\)breakpoint" line)
              'add
            'remove)
          (let ((breakpoint-str (car (last (s-split "\s" line :omit-nulls)))))
            (when (s-matches-p ":" breakpoint-str)
              (let* ((breakpoint-separated (s-split ":" breakpoint-str))
                     (filepath (defpipeline path breakpoint-separated
                                 (car path)
                                 (replace-regexp-in-string "\\." "/" path)
                                 (gud-jdb-find-source-using-classpath path)
                                 (expand-file-name path)))
                     (line-number (string-to-number
                                   (replace-regexp-in-string
                                    "\\." "" (cadr breakpoint-separated)))))
                (list filepath line-number)))))
       (list nil nil nil)))
     (s-split "\n" output-str :omit-nulls)))

(defun gudder:jdb-overlay-subhandler (filepath line-number)
  "See `gudder:debug-info-setup'."
  (with-current-buffer (find-file filepath)
    (goto-char (point-min))
    (forward-line (- line-number 1))
    (let ((header-line (save-excursion
                         (cl-loop
                          do (progn
                               (forward-line -1)
                               (let ((start (progn (back-to-indentation) (point)))
                                     (end (progn (goto-last-non-comment) (point))))
                                 (cond
                                  ((progn (beginning-of-line)
                                          (looking-at "^\s*\\(public\\|private\\|protected\\)?\s*\\(static\\)?\s*[a-zA-Z0-9_]+\s+[a-zA-Z0-9_]+\s*("))
                                   (cl-return (+ (current-line) 1)))
                                  ((gudder:debug-overlays (current-buffer) start (+ 1 end))
                                   (cl-return nil))
                                  ((= (current-line) 0)
                                   (cl-return nil))))))))
          (rest-of-function-call-lines
           (save-excursion (cl-loop
                            while (not (looking-at ".*[;{}].*"))
                            do (forward-line 1)
                            collect (+ (current-line) 1)))))
      (mapcar
       (lambda (line)
         (goto-char (point-min))
         (forward-line (- line 1))
         (let* ((start (save-excursion (back-to-indentation) (point)))
                (end (save-excursion (goto-last-non-comment) (point)))
                (idents (defpipeline idents
                          (buffer-substring-no-properties start end)
                          (without-embeded-strings idents)
                          (s-replace-regexp "[.]?[^[:space:].]+(\\|[()!%^&*|,=+;<>?:/{}-]\\|\\]\\|\\[" " "
                                            idents) ;it is important that the - is last in the regexp group [...-]
                          (s-split " " idents :omit-nulls)
                          (-filter (lambda (ident) ;remove Boolean literals and numbers
                                     (not (s-matches-p "^[[:digit:].]+$\\|true\\|false" ident)))
                                   idents)
                          (mapcar (lambda (ident)
                                    (cond
                                     ((or (s-matches-p "\\[[^]]*$" ident)
                                          (s-matches-p "\\[.*(.*).*\\]" ident))
                                      (substring ident 0 (s-index-of "[" ident)))
                                     ((s-matches-p "[(]" ident)
                                      (defpipeline ident
                                        (substring ident 0 (s-index-of "(" ident))
                                        (substring ident 0 (-find-last-index
                                                            (lambda (char)
                                                              (char-equal char ?.))
                                                            (string-to-list ident)))))
                                     (:else ident)))
                                  idents)
                          (-uniq idents)))
                (debug-values ""))
           (gudder:clear-debug-values (current-buffer) start (+ 1 end))
           (cons line (gudder:jdb-idents-values idents))))
       (-non-nil (append (list line-number header-line) rest-of-function-call-lines))))))

(defun gudder:jdb-find-function (filename function-name)
  "Return the line number where FUNCTION-NAME appears in FILENAME.
Return nil if the file or function doesn't exist."
  (when (file-exists-p filename)
    (with-temp-buffer
      (insert-file-contents-literally filename)
      (let ((line-number nil))
        (while (< (point) (point-max))
          (when (looking-at (concat "^\s*\\(public\\|private\\|protected\\)?\s*\\(static\\)?\s*[a-zA-Z0-9_]+\s+"
                                    function-name "\s*("))
            (setq line-number (+ (current-line) 1)))
          (forward-line))
        line-number))))

(defun gudder:jdb-idents-values (idents)
  "Return a list of list-pairs like so
'((ident1 value1) (ident2 value2) (ident3 value3))'"
  (when (gudder:running-gud)
    (-non-nil
     (mapcar
      (lambda (ident)
        (let ((value (or (gudder:jdb-value ident)
                         (gudder:jdb-value (concat "this." ident)))))
          (cond
           ((null value) nil)
           ((s-matches-p "null" value) nil) ;null value
           ((s-matches-p "\\[[[:digit:]]+\\]" value) ;array
            (let ((length (string-to-number
                           (or (gudder:jdb-value (concat ident ".length"))
                               "0"))))
              (list ident (gudder:jdb-array-value ident length))))
           (:else           ;some primitive or has a toString()
            (list ident value)))))
      idents))))

(defun gudder:jdb-value (ident)
  (let ((gud (gudder:running-gud)))
    (when gud
      (let ((output (sync-process-send-string gud
                                             (concat "print " ident "\n") gudder:jdb-prompt-regexp)))
        (when output
            (let* ((start-index (s-index-of (concat ident " = ") output))
                   (value (when start-index
                            (substring output (+ (or start-index 0) (length ident) 3)
                                       (or (s-next-index "\n" output start-index) (length output))))))
              (if (and value (string= value "null"))
                  nil
                value)))))))

(defun gudder:jdb-array-value (name length)
  (defpipeline values length
    (let ((idents '()))
      (dotimes (i values idents)
        (push (concat name "[" (number-to-string i) "]") idents)))
    (reverse values)
    (gudder:jdb-idents-values values)
    (mapcar (lambda (pair)
              (cadr pair))
            values)
    (s-join ", " values)
    (concat "{" values "}")))

(provide 'gudder-jdb)
;;; gudder-jdb.el ends here
