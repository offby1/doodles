;; http://info.lboro.ac.uk/departments/ma/gallery/selfref/
;;
;; Even though that site describes almost exactly this, I wrote this
;; long before I saw that site.

;; http://home.att.net/~rhhardin/decimalv2.html

;; This is _way_ faster under sbcl than under clisp.

(defmacro lookup (c survey)
  `(svref ,survey (char-code (char-downcase ,c))))

(defparameter *survey* (make-array 128 :initial-element 0))
(defun survey-string (s)
  (fill *survey* 0)
  (map nil
       (lambda (c)
         (incf (lookup c *survey*)))
       s)
  *survey*)

;; returns nil when INPUT is a "true" template.

(defun update-template (input func)
  (let ((copy  (copy-tree input))
        (result))
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

(defun spell-out-character (c)
  (cond
    ((alphanumericp c)
     (format nil "~:C" (char-upcase c)))
    (t
     (case c
       ((#\') "apostrophe")
       ((#\,) "comma")
       ((#\.) "full stop")
       ((#\Space) "space")
       (t "")))))

(defun template->string (tmplt)
  (apply #'concatenate 'string
         (mapcar (lambda (thing)
                   (if (consp thing)
                       (let ((letter (spell-out-character (cdr thing))))
                         (format nil (concatenate 'string "~R " letter "~:p") (car thing)))
                       thing))
                 tmplt)))

(
 #+clisp ext:times
 #-clisp progn
 #+sbcl (sb-profile:profile template->string spell-out-character update-template survey-string)
 #+sbcl (sb-profile:reset)
 (let ((tmplt
        (append 
         (cons "This rather nifty sentence contains "
               (loop for (item rest) on
                    (loop for n
                       from (char-code #\a)
                       to   (char-code #\y)
                       collect (cons 0 (code-char n)))
                    collect item when rest collect ", "))
         (list " and " '(0 . #\z) ".")))

       (duplicate-detector (make-hash-table :test #'equal)))
   (loop
      (let ((str (template->string tmplt)))
        (when  (gethash (sxhash str) duplicate-detector nil)
          ;(format t "~a: ~s~%" (hash-table-count duplicate-detector) str)
          (setf tmplt (update-template tmplt #'(lambda (char)
                                                 (declare (ignore char))
                                                 (random (length str)))))
          (setf str (template->string tmplt)))
        (setf (gethash (sxhash str) duplicate-detector) t)
        (setf tmplt (update-template tmplt #'(lambda (char)
                                               (lookup char (survey-string str)))))
        (when (= 100 (hash-table-count duplicate-detector))
          (format t "Oh hell, I give up.~%")
          (return))
      
        (when (not tmplt)
          (format t "We've got a winner: ``~a''~%" str)
          (return)))))
 #+sbcl(sb-profile:report)

 )
