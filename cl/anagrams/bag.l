(defpackage :bag
  (:use :common-lisp)
  (:export :add :emptyp :subtract :string->bag))
(in-package :bag)



(defclass bag () ((h :initform (make-hash-table) )))
(defmethod print-object ((x bag)
                         stream)
  (format stream "bag: ~S" (as-alist x)))

(defmethod as-alist ((x null))
  nil)

(defmethod as-alist ((x bag))
  (let ((result '()))
    (maphash #'(lambda (k v)
                 (setq result (cons (cons k v) result)))
             (slot-value x 'h))
    result))

(defmethod elements ((x bag))
  (let ((result 0))
    (maphash #'(lambda (k v)
                 (declare (ignore k))
                 (incf result v))
             (slot-value x 'h))
    result))

;; todo -- figure out how to write *one* method, that works for both
;; bags and nil, rather than two methods.
(defmethod emptyp ((x null))
  t)

(defmethod emptyp ((x bag))
  (zerop (elements x)))

(defmethod contents ((x bag))
  (apply 'append
         (mapcar #'(lambda (alist)
                     (make-list (cdr alist)
                                :initial-element (car alist)))
                 (as-alist x))))

(defmethod add ((x bag) datum)
  (incf (gethash datum (slot-value x 'h) 0)))

(defmethod subtract ((minuend    bag)
                     (subtrahend bag))

  (defmethod keys ((x bag))
    (mapcar #'car (as-alist x)))

  (let ((difference (make-instance 'bag)))
    (mapc #'(lambda (key)
              (let ((diff-element (- (gethash key (slot-value minuend    'h) 0)
                                     (gethash key (slot-value subtrahend 'h) 0))))
                (if (minusp diff-element)
                    (return-from subtract nil))
                (if (plusp diff-element)
                    (setf (gethash key (slot-value difference 'h) 0)
                          diff-element)))) 
          (union (keys minuend)
                 (keys subtrahend)) )
    difference))


(defun string->bag (s)
  (let ((result (make-instance 'bag)))
    (mapc #'(lambda (ch)
              (add result ch)) (map 'list #'identity s))
    result))


;;; Unit tests

(defun baglists-equal (a b)
  (equal (sort (copy-seq a) 'string<)
         (sort (copy-seq b) 'string<)))

(let ((actual (make-instance 'bag)))
  (assert (emptyp actual) nil "Bag ~A was not empty: "
          actual)
  (assert (emptyp nil) nil "nil was not empty")
  (add actual 'a)
  (assert (not (emptyp actual))
          nil  "Bag ~A was empty:" actual))

(let ((the-bag (make-instance 'bag)))
  (add the-bag 'a)
  (add the-bag 'b)
  (add the-bag 'a)
  (let ((actual   (contents the-bag))
        (expected (list 'a 'a 'b)))
    (assert (baglists-equal actual expected) nil "Expected ~A; got ~A"
            expected actual)))

(let ((a (make-instance 'bag))
      (b (make-instance 'bag)))
  (defun check-for (expected)
    (let ((actual (contents (subtract a b))))
      (assert (baglists-equal actual expected) nil "Expected ~A; got ~A"
              expected actual)))
  (mapc #'(lambda (datum)
            (add a datum))
        (list 'a 'a 'b 'c))
  (mapc #'(lambda (datum)
            (add b datum))
        (list 'a 'b))
    
  (check-for (list 'a 'c))
  (check-for (list 'a 'c))
  (add a 'x)
  (check-for (list 'a 'c 'x))

  (let ((actual (subtract b a)))
    (assert (null actual) nil "Expected nil when subtracting a big bag (~A) from a small one (~A)" a b)))

(let ((actual   (contents  (string->bag "hey you")))
      (expected '(#\space #\e #\h #\o #\u #\y #\y)))
  (assert (baglists-equal actual expected) nil "Expected contents ~A and ~A to be identical"
             actual expected))

(format t "All tests passed.~%")
