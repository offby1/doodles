(require 'xptest)
(defpackage #:bag
  (:use #:common-lisp #:xp-test))
(in-package #:bag)



(defclass bag () ((stuff :initform (make-hash-table) )))
(defmethod print-object ((x bag)
                         stream)
  (format stream "~A" (slot-value x 'stuff)))

(defmethod as-alist ((x bag))
  (let ((result '()))
    (maphash #'(lambda (k v)
                 (setq result (cons (cons k v) result)))
             (slot-value x 'stuff))
    result))

(defmethod contents ((x bag))
  (apply 'append
         (mapcar #'(lambda (alist)
                     (make-list (cdr alist)
                                :initial-element (car alist)))
                 (as-alist x))))

(defmethod add ((x bag) datum)
  (incf (gethash datum (slot-value x 'stuff) 0)))

(defmethod subtract ((a bag)
                     (b bag))
  (defmethod keys ((x bag))
    (mapcar #'car (as-alist x)))
  (let ((result (make-instance 'bag))
        (all-keys (union (keys a)
                         (keys b))))
    (mapc #'(lambda (key)
              (incf (gethash key (slot-value result 'stuff) 0)
               (- (gethash key (slot-value a 'stuff) 0)
                  (gethash key (slot-value b 'stuff) 0)))) all-keys )
    result
    ))



(def-test-fixture bag-fixture ()
  ((the-bag :accessor the-bag))
  (:documentation "Test fixture for bag testing"))


(defmethod initially-empty ((test bag-fixture))
  (let ((actual (contents (make-instance 'bag))))
    (unless (null actual)
      (failure "Bag ~A was not empty: ~A"
	       (the-bag test)
               actual))))

(defmethod what-goes-in-comes-back-out ((test bag-fixture))
  (let ((the-bag (make-instance 'bag)))
    (add the-bag 'a)
    (add the-bag 'b)
    (add the-bag 'a)
    (let ((actual   (contents the-bag))
          (expected (list 'a 'a 'b)))
      (unless (equal (sort (copy-seq actual  ) 'string<)
                     (sort (copy-seq expected) 'string<))
        (failure "Expected ~A; got ~A"
                 expected actual)))))

(defmethod subtraction ((test bag-fixture))
  (let ((a (make-instance 'bag))
        (b (make-instance 'bag)))
    (defun check-for (expected)
      (let ((actual (contents (subtract a b))))
        (unless (equal (sort (copy-seq actual)   'string<)
                       (sort (copy-seq expected) 'string<))
          (failure "Expected ~A; got ~A"
                   expected actual))))
    (mapc #'(lambda (datum)
              (add a datum))
          (list 'a 'a 'b 'c))
    (mapc #'(lambda (datum)
              (add b datum))
          (list 'a 'b))
    
    (check-for (list 'a 'c))
    (check-for (list 'a 'c))
    (add a 'x)
    (check-for (list 'a 'c 'x))))


(setf bag-test-suite (make-test-suite
		       "Bag Test Suite"
		       "Simple test suite for arithmetic operators."
		       ("Snurkly Test" 'bag-fixture
			:test-thunk 'initially-empty
			:description "Make sure he's initially empty.")
                       ("What goeth in cometh out" 'bag-fixture
                        :test-thunk 'what-goes-in-comes-back-out
                        )
                       ("Subtraccion" 'bag-fixture
                        :test-thunk 'subtraction))
      )

;;;; Finally we can run our test suite and see how it performs.
(report-result (run-test bag-test-suite) :verbose t)
