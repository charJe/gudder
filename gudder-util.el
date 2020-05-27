;;;; this file contains things that are used for gudder but could be very useful for other things as well
(defvar *sync-proccess-output*
  "Holds the output of the process.
Do not use.")
(defvar *sync-proccess-finished-reading*
  "A flag to tell when output is finished.
Do not use.")
(defvar *sync-process-ending-regex*
  "Regex to match the process prompt.")

(defun sync-process-output-filter-function (output)
  "Gets the OUTPUT from the process and puts it in `*sync-proccess-output*'."
  (when (not *sync-proccess-finished-reading*)
    (setq *sync-proccess-output* (concat *sync-proccess-output* output))
    (when (and (not (string= "" *sync-proccess-output*))
               (s-match *sync-process-ending-regex* *sync-proccess-output*))
      (setq *sync-proccess-finished-reading* t)))
  output)

(defun sync-process-send-string (process string ending-regex &optional timeout)
  "Send PROCESS the contents of STRING as input.
This function hangs until the output is received.
If TIMEOUT is non-nil, then return nil after that many seconds.
ENDING-REGEX marks the last thing that PROCESS sends and indicates the end of output."
  (if (and process
           (process-live-p process))
      (progn
        (setq *sync-process-ending-regex* ending-regex)
        (add-hook 'comint-preoutput-filter-functions #'sync-process-output-filter-function)
        (setq *sync-proccess-output* "")
        (setq *sync-proccess-finished-reading* nil)
        (process-send-string process string)
        (let ((time 0)
              (wait-time 0.001))
          (while (and (not *sync-proccess-finished-reading*)
                      (or (null timeout)
                          (< time timeout)))
            (setq time (+ time wait-time))
            (sleep-for wait-time)))
        (setq *sync-process-ending-regex* "")
        *sync-proccess-output*)
    ""))

(defmacro defpipeline (name &rest body)
  (-reduce-from
   (lambda (piped form)
     `((lambda (,name) ,form) ,piped))
   (car body) (cdr body)))

(defun without-last (lst)
  "Return LST with all of it's elements except the last one."
  (if (= (length lst) 1)
      '()
    (cons (car lst) (without-last (cdr lst)))))

(cl-defun without-embeded-strings (str &optional in-str (i 0))
  (cond
   ((>= i (length str)) "")
   ((string= "\"" (substring str i (1+ i)))
    (without-embeded-strings str (not in-str) (1+ i)))
   (in-str
    (without-embeded-strings str in-str (1+ i)))
   (:else
    (concat (substring str  i (1+ i))
            (without-embeded-strings str in-str (1+ i))))))

(defun goto-last-non-comment ()
  "Move point to the last non-comment and non-whitespace character in a line."
  (interactive)                         ;you might want to use this one
  (let ((beg-line (progn (beginning-of-line) (point)))
        (end-line (progn (end-of-line) (point))))
    (beginning-of-line)
    ;; move right to the frist coment character or end of line
    (while (and (not (or (equal 'font-lock-comment-face (face-at-point))
                         (equal 'font-lock-comment-delimiter-face (face-at-point))
                         (equal 'font-lock-doc-string-face (face-at-point))
                         (equal 'font-lock-doc-face (face-at-point))))
                (not (= (point) end-line)))
      (forward-char 1))
    (when (and (not (= (point) beg-line))
               (not (= (point) end-line)))
      (forward-char -1))
    ;; move left until not whitespace or begining of line
    (while (and (looking-at "\s")
                (not (= (point) beg-line)))
      (forward-char -1))
    (when (and (not (= (point) beg-line))
               (not (= (point) end-line)))
      (forward-char 1))))

(provide 'gudder-util)
;;; gudder-util.el ends here
