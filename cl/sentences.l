(defmacro lookup (c survey)
  `(svref ,survey (char-code (char-downcase ,c))))

(defun survey-string (s)
  (let ((survey (make-array 128 :initial-element 0)))
    (map nil
         (lambda (c)
           (incf (lookup c survey)))
         s)
    survey))

;; returns nil when INPUT is a "true" template.
(defun update-template (input survey)
  (setf copy  (copy-tree input))
  (let (result)
    (loop
       (when (null copy)
         (setf result (nreverse result))
         (if (equal input result)
             (return nil))
         (return result))
       (let ((thing (car copy)))
         (when (consp thing)
           (setf (car thing)
                 (lookup (cdr thing) survey)))
         (push thing result))
       (setf copy (cdr copy)))))

(defun template->string (tmplt)
  (apply #'concatenate 'string
         (mapcar (lambda (thing)
                   (if (consp thing)
                       (let ((letter (format nil "~:C" (cdr thing))))
                         (format nil (concatenate 'string "~R " letter "~:p") (car thing)))
                       thing)) tmplt)))

(let ((tmplt (list  "This sentence contains exactly "
                   '(0 . #\z) ", "
                   '(0 . #\q) ", "
                   '(0 . #\r) ", "
                   '(-99 . #\Space)
                   ", and "
                   '(0 . #\e)
                   ))
      (duplicate-detector (make-hash-table :test #'equal)))
  (loop
     (let ((str (template->string tmplt)))
       (format t "~s~%" str)
       (when  (gethash str duplicate-detector nil)
         (format t "Looped, durn it~%")
         (return))
       (setf (gethash str duplicate-detector) t)
       (setf tmplt (update-template tmplt (survey-string str)))
       (when (not tmplt)
         (format t "We've got a winner: ``~a''~%" str)
         (return)))))

