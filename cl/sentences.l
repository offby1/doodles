(defmacro lookup (c survey)
  `(svref ,survey (char-code (char-downcase ,c))))

(defun survey-string (s)
  (let ((survey (make-array 128 :initial-element 0)))
    (map nil
         (lambda (c)
           (incf (lookup c survey)))
         s)
    survey))

(defun survey->alist (s)
  (mapcar (lambda ())))

(defun update-template (input survey)
  (let (result)
    (loop
       (when (null input)
         (return (nreverse result)))
       (let ((thing (car input)))
         (when (consp thing)
           (setf (car thing)
                 (lookup (cdr thing) survey)))
         (push thing result))
       (setf input (cdr input)))))

(defun template->string (tmplt)
  (apply #'concatenate 'string
         (mapcar (lambda (thing)
                   (if (consp thing)
                       (format nil "~R ~:C'~:p" (car thing) (cdr thing))
                       thing)) tmplt)))

(let ((tmplt (list "This sentence has "
                   '(0 . #\e)
                   ", "
                   '(0 . #\a)
                   ", and "
                   '(0 . #\t)
                   ".")))
  (dotimes (x 10)
           (format t "~s => ~s~%" tmplt (template->string tmplt))
           (update-template tmplt (survey-string (template->string tmplt)))))

