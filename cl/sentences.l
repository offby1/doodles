;; http://rec-puzzles.org/sol.pl/language/english/self.ref/self.ref.letters
;; describes the inspiration

;; http://www.cs.indiana.edu/~tanaka/GEB/pangram.txt has some example
;; programs in Perl and C

;; http://info.lboro.ac.uk/departments/ma/gallery/selfref/
;;
;; Even though that site describes almost exactly this, I wrote this
;; long before I saw that site.

;; http://home.att.net/~rhhardin/decimalv2.html

;; This is _way_ faster under sbcl than under clisp.

(proclaim `(optimize (speed 3)))

(defmacro lookup (c survey)
  `(the fixnum (aref ,survey (char-code (char-downcase ,c)))))

;; presumably, creating our array just once (at load time) is faster
;; than creating a new one every time this function is called.
(setf (fdefinition 'survey-string) 
      (let ((survey (make-array 128
                                :initial-element 0
                                :element-type '(unsigned-byte 8)))) 
        (lambda (s)
          (declare (type simple-string s))
          "Given string S, return an array that describes the number of each character."
          (fill survey 0)
          (map nil
               (lambda (c)
                 (incf (lookup c survey)))
               s)
          survey)))

(defun random-inclusively-between (a b)
  (let ((min (min a b)))
    (+ min (random (- (max a b) min -1)))))

(defun update-template (input survey)
  (let (result
        (true t))
    (dolist (thing (copy-tree input) (if true nil (nreverse result)))
      (when (consp thing)
        (let ((claimed (car thing))
              (actual (lookup (cdr thing) survey)))
          (when (not (= claimed actual))
            (setf true nil))
          (setf (car thing) (random-inclusively-between claimed actual))))
      (push thing result))))

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
       ((#\() "left parenthesis")
       ((#\)) "right parenthesis")
       (t "")))))

(defun template->string (tmplt)
  "Return a string that is like TMPLT, but with the conses replaced by
  text."
  (apply #'concatenate 'string
         (mapcar (lambda (thing)
                   (if (consp thing)
                       (let* ((letter (the simple-string (spell-out-character (cdr thing))))
                              (pv (car thing))
                              (maybe-apostrophe 
                               (if (and
                                    (numberp pv)
                                    (not (= 1 (the fixnum pv)))
                                    (alpha-char-p (cdr thing)))
                                   "'"
                                   "")))
                         (format 
                          nil 
                          (concatenate 'string "~R " letter
                                       maybe-apostrophe "~:p") 
                          (or pv 0)))
                       thing))
                 tmplt)))

(defmacro while (test &rest body)
  `(do ()
       ((not ,test))
     ,@body))

(defun yow (max-tries)
  (declare (type fixnum max-tries))
  (let ((tmplt
         (append 
          (cons 
           ;;"Please help yourself to "
           "It seems that, the longer a sentence like this one is, the harder it becomes for it to truthfully claim (as this one does) that it contains exactly "
           (loop for (item rest) on
                (loop for n
                   from (char-code #\a)
                   to   (char-code #\z)
                   collect (cons 0 (code-char n)))
                collect item when rest collect ", "))
          (list 
           ", "     `(0 . #\Space)
           ", "     `(0 . #\()
           ", "     `(0 . #\))
           ", "     `(0 . #\.)
           ", "     `(0 . #\,)
           ", "     `(0 . #\-)
           ", and " `(0 . #\')
           ".")))
        (tries 0))

    (while t ;;(< tries max-tries)
                              
      (let*  ((str   (template->string tmplt))
              (new-tmplt (update-template tmplt (survey-string str))))
         
        (when (not new-tmplt)
          (format t "~a: We've got a winner!: ~s~%" tries str)
          (return))

        (when (zerop (mod tries 1000))
          (format t "~&~a: ~a~%" tries str))
        
        (setf tmplt new-tmplt)
           
        )
      (incf tries))))
