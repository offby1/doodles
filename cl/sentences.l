(defmacro lookup (c survey)
  `(aref ,survey (char-code (char-downcase ,c))))

(defun survey-string (s)
  (let ((survey (make-array 128 :initial-element 0)))
    (map nil
         (lambda (c)
           (incf (lookup c survey)))
         s)
    survey))

(defun fill-template (input survey)
  (let* ((format-string-components (mapcar
                         (lambda (thing)
                           (cond
                             ((characterp thing)
                               "~R"
                              )
                             ((stringp thing)
                              thing)
                             (t (error "wha? ~s" thing))
                             ))
                         input
                         ))
         (fs (apply #'concatenate 'string format-string-components))
         (just-the-chars (remove-if-not #'characterp input)))
  
    (let ((counts (mapcar (lambda (c)
                            (aref survey (char-code (char-downcase c)))) just-the-chars)))
      
      (apply #'format nil fs counts))))

(let ((s (list "This sentence has " #\e " and " #\a)))
  (format t "~s => " s)
  (setf s (fill-template
           s
           (survey-string
            (apply #'concatenate 'string
                   (remove-if-not #'stringp s)))))
  (format t "~s~%" s))
