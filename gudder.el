(require 's)
(require 'dash)
(require 'gud)
(require 'gdb-mi)
(require 'gudder-util)

(defgroup gudder () " Group for customizing gudder.")

(defface gudder:value-face '((t :inherit shadow :weight bold))
  "Face to show debug values at the end of lines.
Is a bold `shadow' by defaut.")

(defcustom gudder:break-bind "<C-f6>"
  "Keybinding to toggle a breakpoint on the current line.
It will be put through `kbd'.")

(defcustom gudder:cont-bind "<f6>"
  "Keybinding to continue to the next breakpoint, end of the program, or error.
It will be put through `kbd'.")

(defcustom gudder:next-bind "<f7>"
  "Keybinding to step to the next line.
It will be put through `kbd'.")

(defcustom gudder:step-into-bind "<C-f7>"
  "Keybinding to step into a subroutine.
It will be put through `kbd'.")

(defcustom gudder:step-out-bind "ESC <f7>"
  "Keybinding to step out of a subroutine.
It will be put through `kbd'.")

(defcustom gudder:max-value-length 50
  "The maximum length (in characters) of debug values before being truncated by `gudder:value-truncation'.")

(defcustom gudder:identifier-value-separator ": "
  "String to be put between the identifier and the value associated with it.
ident\": \"value")

(defcustom gudder:value-separator "  "
  "String to be put between identifier-value paris.
ident: value\"  \"ident: value")

(defcustom gudder:value-truncation "..."
  "String to truncate values that are longer than `gudder:max-value-length'.")

(defcustom gudder:values-head " "
  "String to be put before the begining of debug info.")

(defcustom gudder:values-tail " "
  "String to be put after the end of debug info.")

(defconst gudder:infos-delay-time .0005
  "The time to wait after starting `comint-output-filter-functions'
before running db-overlay-handler.")

(defmacro gudder:bind-keys (mm)
  "Bind the keybindings for gudder for MM mode in MM-map."
  (let ((mm-map (intern (concat (symbol-name mm) "-map"))))
    `(progn
       (define-key ,mm-map (kbd gudder:next-bind) #'gud-next)
       (define-key ,mm-map (kbd gudder:step-into-bind) #'gud-step)
       (define-key ,mm-map (kbd gudder:step-out-bind) #'gud-finish)
       (define-key ,mm-map (kbd gudder:cont-bind) #'gud-cont)
       (define-key ,mm-map [left-fringe mouse-1] #'gudder:toggle-mouse-breakpoint)
       (define-key ,mm-map (kbd gudder:break-bind) #'gudder:toggle-breakpoint))))

(defmacro gudder:breakpoint-setup (db db-breakpoint-handler)
  "Set up breakpoint listeners and facilities for adding red dots in the fringe.
DB is the symbol name of the debugger like jdb or pdb.
DB-BREAKPOINT-HANDLER is a function that takes output from the DB
debugger and returns a list of (action filepath line-number) where action can
be 'add, 'remove, or nil.  Nil means the there were no breakpoints encoded in the
gud output.  when action is nil, 'filepath' and 'line-number' should be nil too.

Generate DB-breakpoint-bait (for `comint-output-filter-functions'),
gudder:toggle-breakpoint, gudder:toggle-mouse-breakpoint, DB-reset-breakpoints-bait (for DB-mode-hook)."
  (let ((*breakpoints* (intern (concat "*" (symbol-name db) "-breakpoints*")))
        (db-breakpoint-bait (intern (concat (symbol-name db) "-breakpoint-bait")))
        (db-reset-breakpoints-bait (intern (concat (symbol-name db) "-reset-breakpoints-bait")))
        (db-mode-hook (intern (concat (symbol-name db) "-mode-hook"))))
    `(progn
       (defvar ,*breakpoints* '()
         ,(concat "Stores all currently active breakpoints for the `" (symbol-name db) "' session.
Breakpoints are stored in the following format:
'((filepath . line-number)
  (filepath . line-number))'."))
       ;; defun
       (defalias ',db-breakpoint-bait
         (lambda (output-str)
           ;; docstring
           ,(concat "Add visual breakpoints to the `" (symbol-name db) "' with gud.
These are shown as red dots in the left fringe, just like with `gdb'.
OUTPUT-STR is output from `" (symbol-name db) "' that could indicate that a
breakpoint has been set or removed.")
           ;; body
           (save-window-excursion
             (save-excursion
               (let ((infos (,db-breakpoint-handler output-str)))
                 (dolist (info infos)
                   (let* ((action (car info))
                          (filepath (cadr info))
                          (line-number (caddr info)))
                     (when (and action filepath line-number
                                (file-exists-p filepath))
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
                           (forward-line (- line-number 1))
                           (gdb-remove-breakpoint-icons (point) (point)))))))))))
           output-str))
       ;; add hook
       (add-hook 'comint-output-filter-functions #',db-breakpoint-bait)
       ;; mouse toggle
       (defalias 'gudder:toggle-mouse-breakpoint
         (lambda (event)
           ,(concat "Toggle a breakpoint for `" (symbol-name db) "'.")
           (interactive "e")
           (mouse-set-point event)
           (when (not (gudder:running-gud))
             (call-interactively #',(intern (concat "gudder:" (symbol-name db)))))
           (let ((filename (buffer-file-name))
                 (line-number (+ (current-line) 1)))
             (if (member (cons filename line-number) ,*breakpoints*)
                 (gud-remove nil)
               (gud-break nil)))))
       ;; keyboard version
       (defalias 'gudder:toggle-breakpoint
         (lambda ()
           ,(concat "Toggle a breakpoint for `" (symbol-name db) "'.")
           (interactive)
           (when (not (gudder:running-gud))
             (call-interactively #',(intern (concat "gudder:" (symbol-name db)))))
           (let ((filename (buffer-file-name))
                 (line-number (+ (current-line) 1)))
             (if (member (cons filename line-number) ,*breakpoints*)
                 (gud-remove nil)
               (gud-break nil)))))
       ;; reset breakpoints
       (defalias ',db-reset-breakpoints-bait
         (lambda ()
           ;; docstirng
           ,(concat "Upon restarting `" (symbol-name db) "', set all the breakpoints that were
there from the previous session.")
           ;; body
           (save-window-excursion
             (save-excursion
               (dolist (buffer (buffer-list)) ;clear debug values from all buffers
                 (gudder:clear-debug-values buffer))
               (let ((breakpoints (copy-tree ,*breakpoints*)))
                 (setq ,*breakpoints* '())
                 (dolist (breakpoint breakpoints)
                   (let ((filepath (car breakpoint))
                         (line-number (cdr breakpoint)))
                     (with-current-buffer (find-file filepath)
                       (goto-char (point-min))
                       (forward-line (- line-number 1))
                       (gud-break nil)))))))))
       (add-hook ',db-mode-hook #',db-reset-breakpoints-bait))))

(defun gudder:running-gud ()
  "Return the first running instance of a gud debugger."
  (cl-loop for process in (process-list)
           do (when (and (s-contains-p "gud" (process-name process))
                         (process-live-p process))
                (cl-return process))))

(defun gudder:clear-debug-values (&optional buffer beg end)
  "Remove all overlays in region BEG to END in BUFFER."
  (interactive)
  (unless buffer (setq buffer (current-buffer)))
  (with-current-buffer buffer
    (unless beg (setq beg (if mark-active (min (mark) (point)) (point-min))))
    (unless end (setq end (if mark-active (max (mark) (point)) (point-max))))
    (mapcar (lambda (overlay) (delete-overlay overlay))
            (gudder:debug-overlays buffer beg end))))

(cl-defun gudder:debug-overlays (&optional (buffer (current-buffer)) (beg (point-min)) (end (point-max)))
  "Return a list of all debug overlays in region BEG to END in BUFFER."
  (with-current-buffer buffer
    (-non-nil
     (mapcar (lambda (overlay)
               (when (equal (overlay-get overlay 'face)
                            'gudder:value-face)
                 overlay))
             (overlays-in beg end)))))

(defun gudder:add-debug-info-here (info)
  "Add INFO as imaginary text at the end of the current line but before the comment.
INFO is just a string.  INFO is displayed using `gudder:debug-value-face'."
  (let ((spot (save-excursion (goto-last-non-comment) (point))))
    (setq test-overlay (make-overlay spot spot (current-buffer)))
    (overlay-put test-overlay 'before-string (propertize info 'face 'gudder:value-face))
    (overlay-put test-overlay 'face 'gudder:value-face)))

(defmacro gudder:debug-info-setup (db db-overlay-subhandler db-step-complete-regexp)
  "Create DB-overlay-bait (which indirectly calls DB-OVARLAY-SUBHANDLER)
and adds it to `comint-output-filter-functions' hook.
DB is the symbol name of the debugger like jdb or pdb.
DB-OVERLAY-SUBHANDLER is a function that takes (filepath line-number) and returns
a list like so:
 '((line-number1 (ident1 value2) (ident2 value2))
   (line-number2 (ident3 value3) (ident4 value2))...)'.
Most of the time there will only be one line-number.  No need to `save-excursion'
or `save-window-excursion'.  The intputs (filepath line-number) describe the
current location of the debugger."
  (let ((db-overlay-handler (intern (concat (symbol-name db) "-overlay-handler")))
        (db-overlay-bait (intern (concat (symbol-name db) "-overlay-bait"))))
    `(progn
       ;; defun
       (defalias ',db-overlay-handler
         (lambda ()
           ;; docstring
           ,(concat "Uses position of `gud-overlay-arrow-position' to display variable values in buffer.
Advised by `" (symbol-name db-overlay-subhandler) "'.")
           ;; body
           (when gud-overlay-arrow-position
             (let* ((filepath (buffer-file-name (marker-buffer gud-overlay-arrow-position)))
                    (line-number (with-current-buffer (marker-buffer gud-overlay-arrow-position)
                                   (goto-char gud-overlay-arrow-position)
                                   (+ (current-line) 1))))
               (save-window-excursion
                 (save-excursion
                   (let ((buffer (find-file filepath))
                         (infos (,db-overlay-subhandler filepath line-number)))
                     (with-current-buffer buffer
                       (dolist (debug-info infos) ;for each line of infos
                         (goto-char (point-min))
                         (forward-line (- (car debug-info) 1))
                         (defpipeline debug-info
                           (-non-nil debug-info)
                           (cdr debug-info)
                           (mapcar (lambda (pair)
                                     (when pair
                                       (concat (car pair) gudder:identifier-value-separator
                                               (if (<= (length (cadr pair)) gudder:max-value-length)
                                                   (cadr pair)
                                                 (concat (substring (cadr pair) 0 gudder:max-value-length)
                                                         gudder:value-truncation)))))
                                   debug-info)
                           (s-join gudder:value-separator debug-info)
                           (when (not (string= debug-info ""))
                             (gudder:add-debug-info-here
                              (concat gudder:values-head debug-info gudder:values-tail))))))
                     (with-current-buffer buffer
                       (goto-char (point-min))
                       (forward-line (- line-number 1)))))))
             )))
       ;; defun
       (defalias ',db-overlay-bait
         (lambda (output-str)
           ;; docsting
           ,(concat "call `" (symbol-name db-overlay-handler)"' to display values
of variables in the buffer where `gud-overlay-arrow-position' is.")
           ;; body

           (when (s-matches-p ,db-step-complete-regexp output-str)
             (run-with-timer gudder:infos-delay-time nil #',db-overlay-handler))
           output-str))
       (when (not (eq (car (last comint-output-filter-functions)) ',db-overlay-bait))
         (setf comint-output-filter-functions (append comint-output-filter-functions
                                                      (list #',db-overlay-bait)))))))

(provide 'gudder)
;;; gudder.el ends here
