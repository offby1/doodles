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
;;(proclaim `(optimize (debug 3)))

(defmacro lookup (c survey)
  `(the fixnum (aref ,survey (char-code ,c))))

(defparameter  alphabet
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz().,0123456789-'"
  "The characters that will get spelled out")

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

(defvar *the-template* nil)

;; this holds the same conses as *the-template*.  It's easier to mess
;; with them if there aren't any strings mixed in.
(defvar *alist-to-fiddle* nil)

(setf (fdefinition 'update-template)
      (let ((current-character-index 0))
        (lambda (survey)
          (let ((true t)
                (the-character (aref alphabet current-character-index)))
            (dolist (the-cons *alist-to-fiddle*)
              (let* ((claimed (cdr the-cons))
                     (actual (lookup (car the-cons) survey)))
                (when (not (= claimed actual))
                  (setf true nil)
                  (when (eq the-character (car the-cons))
                    (setf (cdr the-cons) (random-inclusively-between claimed actual))))))
            (when true
              (throw :weve-got-a-winner *the-template*)))
          (setf current-character-index (mod (+ 1 current-character-index) (length alphabet))))))

(defun spell-out-character (c)
  "Given a character C, return a string that names the character.  For
  example, #\\c => \"C\"; #\\, => \"comma\"."
  (assert (find c alphabet))
  (cond
    ((alphanumericp c)
     (format nil "~:C" c))
    (t
     (case c
       ((#\') "apostrophe")
       ((#\,) "comma")
       ((#\.) "full stop")
       ((#\-) "hyphen")
       ((#\Space) "space")
       ((#\() "left paren")
       ((#\)) "right paren")
       (t "")))))

(defun template->string ()
  "Return a string that is like TMPLT, but with the conses replaced by
  text."
  (apply #'concatenate 'string
         (mapcar (lambda (thing)
                   (if (consp thing)
                       (progn
                         (let* ((letter (the simple-string (spell-out-character (car thing))))
                                (pv (cdr thing))
                                (maybe-apostrophe 
                                 (if (and
                                      (numberp pv)
                                      (not (= 1 (the fixnum pv)))
                                      (alpha-char-p (car thing)))
                                     "'"
                                     "")))
                           (format 
                            nil 
                            (concatenate 'string "~R " letter
                                         maybe-apostrophe "~:p") 
                            (or pv 0))))
                       thing))
                 *the-template*)))

(defmacro while (test &rest body)
  `(do ()
       ((not ,test))
     ,@body))

(defun yow (max-tries)
  (declare (type fixnum max-tries))
  (setf *the-template* 
        (cons 
         "This sentence, which is Copyright (c) 2005 Eric Hanchrow, has got just about "
          
         (append
          (loop for (item penultimate last)
             on (map 'list (lambda (c)
                             (cons c 0))
                     alphabet)
             collect item when penultimate collect ", " when (and  penultimate (not last)) collect "and " )
                 
         (list ".")))
        )
  (setf *alist-to-fiddle*  (remove-if-not 'consp *the-template*))
  (let ((tries 0))

    (format 
     t 
     "We've got a winner!: ~s~%"
     (progn
       (catch :weve-got-a-winner
         (while 
             (< tries max-tries)
           ;;t
      
           (let ((str   (template->string)))
             (update-template (survey-string str))
          
             (when (zerop (mod tries 1000))
               (format t "~&~a: ~a~%" tries str)))
      
           (incf tries))
         (return-from yow))
       (template->string)))
    ))
