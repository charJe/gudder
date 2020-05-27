(require 'gdb-mi)
(require 'gud)
(require 'dash)
(require 'gudder-util)

(defface gudder-debug-value-face '((t :inherit shadow
                                      ;; :foreground "#00ff00"
                                      ;; :background "#000000"
                                      :weight bold))
  "Face to showdebug values. Is a bold `shadow' by defaut.")

(defcustom gudder-break-bind "<C-f6>"
  "Keybinding to toggle a breakpoint on the current line.
It will be put through `kbd'.")

(defcustom gudder-cont-bind "<f6>"
  "Keybinding to continue to the next breakpoint, end of the program, or error.
It will be put through `kbd'.")

(defcustom gudder-next-bind "<f7>"
  "Keybinding to step to the next line.
It will be put through `kbd'.")

(defcustom gudder-step-into-bind "<C-f7>"
  "Keybinding to step into a subroutine.
It will be put through `kbd'.")

(defcustom gudder-step-out-bind "<f8>"
  "Keybinding to step out of a subroutine.
It will be put through `kbd'.")

(defcustom gudder-max-debug-value-length 50
  "The maximum length of debug values before being truncated by ...")
(defmacro gudder-bind-keys (mm)
  "Bind the keybindings for gudder for MM mode. 
This doesn't bind gudder-cont-bind; instead use `gudder-toggle-breakpoint'."
  `(progn
    (define-key ,(intern (concat (symbol-name (eval mm)) "-map")) (kbd gudder-next-bind) #'gud-next)
    (define-key ,(intern (concat (symbol-name (eval mm)) "-map")) (kbd gudder-step-into-bind) #'gud-step)
    (define-key ,(intern (concat (symbol-name (eval mm)) "-map")) (kbd gudder-step-out-bind) #'gud-finish)
    (define-key ,(intern (concat (symbol-name (eval mm)) "-map")) (kbd gudder-cont-bind) #'gud-cont)))

(defmacro gudder-breakpoint-clearer (db *breakpoints*)
  "Create a function that will clear all visual breakpoints for DB-mode.
DB is the name of the cli debugger.  This function is named
`DB-clear-all-visual-breakpoints'.
*BREAKPOINTS* is a mutable variable in the in the form:
`'((filename . line-number)
  (filename . line-number)
  ...)'"
  `(progn
    ;; defun
     (defalias ',(intern (concat (symbol-name (eval db)) "-clear-all-visual-breakpoints"))
       (lambda ()
         ;; docstring
         ,(concat "Remove all visual breakpoints from " (upcase (symbol-name *breakpoints*)) " and their buffers.")
         ;; body
         (let ((broken-files (-uniq (mapcar (lambda (breakpoint)
                                              (car breakpoint))
                                            ,*breakpoints*))))
           (dolist (filename broken-files)
             (save-window-excursion
               (with-current-buffer (find-file filename)
                 (gdb-remove-breakpoint-icons (point-min) (point-max)))))
           (setq ,*breakpoints* ()))))))

(defmacro gudder-breakpoint-listener (db *breakpoints* output-parser)
  `(progn
     (defalias ',(intern (concat (symbol-name (eval db)) "-breakpoint-listener"))
       (lambda (output-str)
         ,(concat "Add visual breakpoints to the " (upcase (symbol-name (eval db))) " with gud.
These are shown as red dots in the left fringe, just like with gdb.")
         (let ((infos (,output-parser output-str)))
           (dolist (info infos)
             (let* ((action (car info))
                    (filepath (cadr info))
                    (line-number (caddr info)))
               (when (and action
                          (file-exists-p filepath))
                 (save-window-excursion
                   (with-current-buffer (find-file filepath)
                     (cond
                      ;; add breakpoint
                      ((equal action 'add)
                       (push (cons filepath line-number) ,*breakpoints*)
                       (gdb-put-breakpoint-icon t :bptno line-number))
                      ;; remove breakpoint
                      ((equal action 'remove)
                       (setq ,*breakpoints* (remove (cons filepath line-number) ,*breakpoints*))
                       (goto-char 0)
                       (forward-line (1- line-number))
                       (gdb-remove-breakpoint-icons (point) (point))))))))))
         output-str))
     ;; add hook
     (add-hook 'comint-output-filter-functions
               #',(intern (concat (symbol-name (eval db)) "-breakpoint-listener")))))

(defmacro gudder-toggle-breakpoint (db mm *breakpoints*)
  "Create `DB-toggle-mouse-breakpoint' bound to `[left-fringe mouse-1]')' and 
`DB-toggle-breakpoint' bound to `(kbd gudder-break-bind)' in `MM-map'."
  `(progn
     ;; mouse version
     (defalias ',(intern (concat (symbol-name (eval db)) "-toggle-mouse-breakpoint"))
       (lambda (event)
         (interactive "e")
         (mouse-set-point event)
         (when (not (running-gud))
           (call-interactively #',(intern (concat "gudder-" (symbol-name (eval db))))))
         (let ((filename (buffer-file-name))
               (line-number (1+ (current-line))))
           (if (member (cons filename line-number) ,*breakpoints*)
               (gud-remove nil)
             (gud-break nil)))))
     (define-key ,(intern (concat (symbol-name (eval mm)) "-map")) [left-fringe mouse-1]
       #',(intern (concat (symbol-name (eval db)) "-toggle-mouse-breakpoint")))
     ;; keyboard version
     (defalias ',(intern (concat (symbol-name (eval db)) "-toggle-breakpoint"))
       (lambda ()
         (interactive)
         (when (not (running-gud))
           (call-interactively #',(intern (concat "gudder-" (symbol-name (eval db))))))
         (let ((filename (buffer-file-name))
                 (line-number (1+ (current-line))))
           (if (member (cons filename line-number) ,*breakpoints*)
               (gud-remove nil)
             (gud-break nil)))))
     (define-key ,(intern (concat (symbol-name (eval mm)) "-map")) (kbd gudder-break-bind)
       #',(intern (concat (symbol-name (eval db)) "-toggle-breakpoint")))))

(defmacro gudder-continue (db mm *breakpoints*)
  "Create `DB-continue' bound to `(kbd gudder-cont-bind)'."
  `(progn
     (defalias ',(intern (concat (symbol-name (eval db)) "-continue"))
       (lambda ()
         "Continue debugger to the next breakpoint."
         (interactive)
         (while (let ((filepath (buffer-file-name (marker-buffer (or gud-overlay-arrow-position (make-marker)))))
                      (line-number (save-excursion
                                     (with-current-buffer (marker-buffer (or gud-overlay-arrow-position (make-marker)))
                                       (goto-char gud-overlay-arrow-position)
                                       (+ 1 (current-line))))))
                  (member (cons filepath line-number) ,*breakpoints*))
           (gud-step))))
     (define-key ,(intern (concat (symbol-name (eval mm)) "-map")) (kbd gudder-cont-bind)
       #',(intern (concat (symbol-name (eval db)) "-continue")))))

(defun gudder-find-source (partial-path extension sourcepaths)
  "Return the full path of a file with PARTIAL-PATH and file extension EXTENSION.
Search for it the current directory in SOURCEPATHS, a list of paths where the
file could be."
  (if sourcepaths
      (let* ((sourcepath (car sourcepaths))
             (filepath (concat (file-name-as-directory
                                sourcepath)
                               partial-path
                               extension)))
        (if (file-exists-p filepath)
            filepath
          (gudder-find-source partial-path extension (cdr sourcepaths))))
    ;; search in the current directory
    (concat partial-path extension)))

(defun running-gud ()
  "Return the first running instance of a gud debugger."
  (defpipeline plst (process-list)
    (mapcar (lambda (process)
              (when (and (s-contains-p "gud" (process-name process))
                         (process-live-p process))
                process))
            plst)
    (cons 'or plst)
    (eval plst)))

(cl-defun gudder-clear-debug-values (&optional (buffer (current-buffer)) (beg (point-min)) (end (point-max)))
  "Remove all overlays in region BEG to END in BUFFER."
  (interactive "bin: ")
  (with-current-buffer buffer
    (mapcar (lambda (overlay)
              (delete-overlay overlay))
            (gudder-debug-overlays buffer beg end))))

(cl-defun gudder-debug-overlays (&optional (buffer (current-buffer)) (beg (point-min)) (end (point-max)))
  "Return a list of all debug overlays in region BEG to END in BUFFER."
  (with-current-buffer buffer
    (-non-nil
     (mapcar (lambda (overlay)
               (when (equal (overlay-get overlay 'face)
                            'gudder-debug-value-face)
                 overlay))
             (overlays-in beg end)))))

(defun add-debug-info-here (info)
  "Add INFO as imaginary text at the end of the line but before the comment.
INFO is just a string.  INFO is displayed using `gudder-debug-value-face'."
  (let ((spot (save-excursion (goto-last-non-comment) (point))))
    (setq test-overlay (make-overlay spot spot (current-buffer)))
    (overlay-put test-overlay 'before-string (propertize info 'face 'gudder-debug-value-face))
    (overlay-put test-overlay 'face 'gudder-debug-value-face)))

(provide 'gudder)
;;; gudder.el ends here


















