;; This is _way_ faster under sbcl than under clisp.

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

(defun update-template (input func)
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
                 (funcall func (cdr thing))))
         (push thing result))
       (setf copy (cdr copy)))))

(defun spell-out-letter (c)
  (cond
    ((alpha-char-p c)
     (format nil "~:C" (char-upcase c)))
    (t
     (case c
       ((#\') "apostrophe")
       ((#\,) "comma")
       ((#\.) "full stop")
       ((#\Space) "space")
       (t ""))))
  )

(defun template->string (tmplt)
  (apply #'concatenate 'string
         (mapcar (lambda (thing)
                   (if (consp thing)
                       (let ((letter (spell-out-letter (cdr thing))))
                         (format nil (concatenate 'string "~R " letter "~:p") (car thing)))
                       thing)) tmplt)))

(
 #+clisp ext:times
 #-clisp progn
 (let ((tmplt 
        (append
         (list
          "When boiling, baking, or stweing, be sure to include " '(0 . #\,)
          ", " '(0 . #\Space)
          ", " '(0 . #\.)
          ", ")
         (let (thing)
           (dotimes (c (- (char-code #\z)
                          (char-code #\a))
                     thing)
             (push (cons 0 (code-char (+ (char-code #\a) c))) thing)
             (push ", " thing))
           (push "and " thing)
           (push '(0 . #\z) thing)
           (push "." thing)
           (nreverse thing))))
               
       (duplicate-detector (make-hash-table :test #'equal)))
   (format t "Here's that template, you drunken sot: ~s~%" tmplt)
   (loop
      (let ((str (template->string tmplt)))
        (format t "~a: ~s~%" (hash-table-count duplicate-detector) str)
        (when  (gethash str duplicate-detector nil)
          (format t "Whop!~%")
          (setf tmplt (update-template tmplt #'(lambda (char)
                                                 (random (length str)))))
          (setf str (template->string tmplt))
          (format t "~s~%" str))
        (setf (gethash str duplicate-detector) t)
        (setf tmplt (update-template tmplt #'(lambda (char)
                                               (lookup char (survey-string str)))))
        (when (not tmplt)
          (format t "We've got a winner: ``~a''~%" str)
          (return))))))
