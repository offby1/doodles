;; http://info.lboro.ac.uk/departments/ma/gallery/selfref/
;;
;; Even though that site describes almost exactly this, I wrote this
;; long before I saw that site.

;; http://home.att.net/~rhhardin/decimalv2.html

;; This is _way_ faster under sbcl than under clisp.

;; sxhash under clisp appears to examine only the first few (31?)
;; characters of strings, and hence cannot be used (lest we get far
;; too many collisions).  Unfortunatley the only alternative I know of
;; is the debian package `cl-md5', which is very slow, and whose
;; presence (in the form of ``(clc:clc-require :md5)'') prevents this
;; file from being compiled (clisp complains that there is no package
;; named MD5).

#+clisp (error "I don't work under clisp 2.33.2")

(proclaim `(optimize (speed 3)))

(defmacro lookup (c survey)
  `(the fixnum (svref ,survey (char-code (char-downcase ,c)))))

;; presumably, keeping our array in a global is faster than
;; SURVEY-STRING creating a new one every time it's called.
(defparameter *survey* (make-array 128 :initial-element 0))
(defun survey-string (s)
  (declare (type simple-string s))
  "Given string S, return an array that describes the number of each character."
  (fill *survey* 0)
  (map nil
       (lambda (c)
         (incf (lookup c *survey*)))
       s)
  *survey*)

;; returns nil when INPUT is a "true" template.
(defun update-template (input func)
  (declare (type function func))
  "Given a \"template\" INPUT, and a function FUNC, update the counts
in all the conses by calling FUNC on the character in the cons."
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
  "Given a character C, return a string that names the character.  For
  example, #\\c => \"C\"; #\\, => \"comma\"."
  (cond
    ((alphanumericp c)
     (format nil "~:C" (char-upcase c)))
    (t
     (case c
       ((#\') "apostrophe")
       ((#\,) "comma")
       ((#\.) "full stop")
       ((#\-) "hyphen")
       ((#\Space) "space")
       (t "")))))

(defun template->string (tmplt)
  "Return a string that is like TMPLT, but with the conses replaced by
  text."
  (apply #'concatenate 'string
         (mapcar (lambda (thing)
                   (if (consp thing)
                       (let ((letter (the simple-string (spell-out-character (cdr thing))))
                             (maybe-apostrophe 
                              (if (and
                                   (not (= 1 (the fixnum (car thing))))
                                   (alpha-char-p (cdr thing)))
                                  "'"
                                  "")))
                         (format 
                          nil 
                          (concatenate 'string "~R " letter
                                       maybe-apostrophe "~:p") (car thing)))
                       thing))
                 tmplt)))

(defun yow (tries)
  (declare (type fixnum tries))
  (let ((tmplt
         (append 
          (cons 
           ;;"Please help yourself to "
           "The sentence contains exactly "
           (loop for (item rest) on
                (loop for n
                   from (char-code #\a)
                   to   (char-code #\z)
                   collect (cons 0 (code-char n)))
                collect item when rest collect ", "))
          (list 
           ", " '(0 . #\Space)
           ", " '(0 . #\.)
           ", " '(0 . #\,)
           ", " '(0 . #\-)
           ", and " '(0 . #\')
           ".")))

        (duplicate-detector (make-hash-table :test #'eql)))
    (loop
       (let ((str (template->string tmplt)))
         (when (zerop (mod tries 1000))
           (format t "~a: ~s~%" tries str))

         ;; If we find ourselves examining a sentence that we've
         ;; already seen, start over with random values.
           
         ;; this doesn't work reliably in clisp -- sxhash returns
         ;; the same value on very different strings.  This isn't a
         ;; bug; it's just unfortunate.
         (when  (gethash (sxhash (the simple-string str)) duplicate-detector nil)
           ;;(warn "~s is a duplicate; starting over with random values" str)
           (setf tmplt (update-template tmplt #'(lambda (char)
                                                  (declare (ignore char))
                                                  (random (length str)))))
           (setf str (template->string tmplt)))
                                                 
         (setf (gethash (sxhash (the simple-string str)) duplicate-detector) t)
         (setf tmplt (update-template tmplt #'(lambda (char)
                                                (lookup char (survey-string str)))))

         (when (not tmplt)
           (format t "(~a): We've got a winner: ``~a''~%"
                   tries
                   str)
           (return)))
       
       (when (zerop tries)
         (return))

       (decf tries))))
