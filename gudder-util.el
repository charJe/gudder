;;;; This file contains things that are used for gudder but could be useful for other things as well
(require 's)

(defvar *sync-proccess-output* ""
  "Holds the output of the process.")
(defvar *sync-proccess-finished-reading* nil
  "A flag to tell when output is finished.")
(defvar *sync-process-ending-regex* ""
  "Regex to match the process prompt.")
(defun sync-process-output-filter-function (output)
  "Gets the OUTPUT from the process and puts it in `*sync-proccess-output*'."
  (if (not *sync-proccess-finished-reading*)
      (progn
        (setq *sync-proccess-output* (concat *sync-proccess-output* output))
        (when (and (not (string= "" *sync-proccess-output*))
                 (s-match *sync-process-ending-regex* *sync-proccess-output*)) ;end of transmision
          (setq *sync-proccess-finished-reading* t))
        "")
    output))
(defun sync-process-send-string (process string ending-regex &optional timeout)
  "Send PROCESS the contents of STRING as input.
This function hangs until the output is received.
If TIMEOUT is non-nil, then return nil after that many seconds.
ENDING-REGEX marks the last thing that PROCESS sends and indicates the end of output."
  (if (and process (process-live-p process))
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
        (remove-hook 'comint-preoutput-filter-functions #'sync-process-output-filter-function)
        (setq *sync-process-ending-regex* "")
        *sync-proccess-output*)
    ""))

(defmacro defpipeline (name &rest body)
  "NAME will evalueate to the result of the previous expression in BODY.
NAME should be whatever is the result of the pipeline. "
  (-reduce-from
   (lambda (piped form)
     `((lambda (,name) ,form) ,piped))
   (car body) (cdr body)))

(defun without-last (list)
  "Return LIST with all of it's elements except the last one."
  (if (= (length list) 1)
      '()
    (cons (car list) (without-last (cdr list)))))

(defun without-embeded-strings (str)
  "Return STR with out any \" or and characters between \"."
  (let ((chars (string-to-list str))
        (in-str))
    (apply 'string
           (-filter (lambda (char)
                      (if (/= char 34)
                          (not in-str)
                        (setq in-str (not in-str))
                        nil))
                    chars))))

(defun goto-last-non-comment ()
  "Move point to the last non-comment and non-whitespace character in a line."
  (interactive)                         ;you might want to use this for editing
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

(defun after-str (list str)
  "Return the rest of LIST after STR.
LIST must contain only strings."
  (cond
   ((null list) nil)
   ((string= (car list) str) (cdr list))
   (:else (after-str (cdr list) str))))

(defun s-next-index (needle str current-index &optional ignore-case)
  "Return the first index of NEEDLE in STR after CURRENT-INDEX."
  (let ((index (s-index-of needle (substring str (+ current-index 1)) ignore-case)))
    (when index (+ index current-index 1))))

(provide 'gudder-util)
;;; gudder-util.el ends here
