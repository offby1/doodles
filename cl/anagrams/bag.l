(defun bag (string)
  (declare (type (simple-array standard-char) string))
  (sort (string-downcase string) #'char-lessp))

(defun bag-emptyp (b)
  (declare (type (simple-array standard-char) b))
  (zerop (length b)))

(assert (bag-emptyp (bag "")))
(assert (equal "ehy" (bag "hey")))

;; for debugging
(defun bag-to-string (bag)
  bag)

(defun bags-equal (b1 b2)
  (equalp b1 b2))

;; Doesn't modify its argument.

;;         M I N U E N D
;; - S U B T R A H E N D
;; ---------------------
;;   D I F F E R E N C E

(defun string-car (s)
  (elt s 0))

(declaim (inline string-car))

;; This conses!  That surprises me ...
(defun string-cdr (s)
  (make-array (- (length s) 1) :element-type 'standard-char
              :displaced-to s
              :displaced-index-offset 1))
(declaim (inline string-cdr))

(defun br3 (minuend subtrahend accum)
  (check-type minuend    (vector standard-char))
  (check-type subtrahend (vector standard-char))
  (cond
   ;; I'm assuming that finding the length of a string takes constant
   ;; time.
   ((zerop (length subtrahend))
    (setf accum (merge 'string accum  minuend 'char<)))
   ((zerop (length minuend))
    nil)
   (t
    (let ((m (string-car minuend))
          (s (string-car subtrahend)))
      (cond
       ((char< s m)
        nil)
       ((char= s m)
        (br3 (string-cdr minuend)
             (string-cdr subtrahend)
             accum))
       (t
        (br3 (string-cdr minuend)
             subtrahend
             (progn (vector-push (string-car minuend) accum) 
                    accum))))))))

(defun bag-reduce  (minuend subtrahend)
  (check-type minuend    (simple-array standard-char))
  (check-type subtrahend (simple-array standard-char))
  (br3 minuend subtrahend (make-array (length minuend)
                                      :fill-pointer 0
                                      :element-type 'standard-char)))


(let ((initial (bag "barf")))
  (assert (null (bag-reduce initial (bag "sammy")))))

(let ((initial (bag "yayyou")))
  (let ((smaller (bag-reduce initial (bag "yy"))))
    (assert (string= initial (bag "yayyou")))
    (assert (string= smaller (bag "yaou")))
    ))

(let ((initial (bag "heyyou")))
  (let ((smaller (bag-reduce initial (bag "you"))))
    (assert (string= smaller (bag "hey"))))

  (let ((empty (bag-reduce initial (bag "heyyou"))))
    (assert (bag-emptyp empty))))

(let ((actual (bag-reduce "" "")))
  (assert (string= actual "")
          (actual)))

(let ((actual (bag-reduce "" "a")))
  (assert (null actual)
          (actual)))

(assert (null (bag-reduce "abcd" "abcx")))