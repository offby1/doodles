(defun bag-number (b)
  (car b))
(defun size (b)
  (cdr b))

;; TODO -- arrange these in order of frequency, so that the most
;; frequent letters (i.e., `e') have the smallest primes.  Like Morse
;; code.
(defparameter *primes* #(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 101))
(declaim (type (simple-array  fixnum (26))  *primes*))
(defun char_to_index (c)
  (declare  (type character c))
  (- (char-code c)
     (char-code #\a)))

(defun index_to_char (i)
  (declare (type fixnum i))
  (code-char (+ i (char-code #\a))))

(defun bag (s)
  (declare (type simple-string s))
  (let ((return-value (cons 1 0)))
    (dotimes (chars-processed (length s) return-value)
      (let ((this-char (char-downcase (aref s chars-processed))))
        (when (and
               (alpha-char-p this-char)
               (>= (char-code this-char)
                   (char-code #\a))
               (<= (char-code this-char)
                   (char-code #\z)))
          (setf (car return-value) 
                (* (the integer (car return-value))
                   (the (unsigned-byte 32) (aref   *primes* (char_to_index this-char)))))
          (incf (cdr return-value)))))
    (assert (not (zerop (cdr return-value))))
    return-value))

(defvar *verbose* nil)
(defun subtract-bags (minuend subtrahend)
  (declare (type cons minuend))
  (declare (type cons subtrahend))
  (when *verbose* (format t "Subtracting ~a from ~a~%" subtrahend minuend))
  (let ((new-size (- (size minuend)
                     (size subtrahend))))
  (when (minusp new-size) 
    (when *verbose* (format t "subtrahend too large: returning nil.~%"))
    (return-from subtract-bags nil))

  (multiple-value-bind (q r)
      (floor (bag-number minuend)
             (bag-number subtrahend))
    (when (not (zerop r))
      (when *verbose* (format t "remainder when dividing: returning nil.~%"))
      (return-from subtract-bags nil))
    (cons q new-size))))

(defun bag-emptyp (b)
  (declare (type (or (satisfies not) cons) b))
  (when *verbose* (format t "bag-emptyp: ~s~%" b))
  (or (not b)
      (zerop (size b))))

(defun bags-equalp (b1 b2)
  (equalp b1 b2))

;; debugging only
(defun bag_to_string (b)
  (format nil "~a" (bag-number b)))