(require 'xptest)
(defpackage #:bag
  (:use #:common-lisp #:xp-test))
(in-package #:bag)



(defclass bag () ((stuff :initform nil )))
(defmethod print-object ((x bag)
                         stream)
  (format stream "~A" (slot-value x 'stuff)))

(defmethod contents ((x bag))
  (slot-value x 'stuff))

(defmethod add ((x bag) datum)
  (setf (slot-value x 'stuff)
        (cons datum (slot-value x 'stuff))))

(defmethod subtract ((a bag)
                     (b bag))
  (make-instance 'bag))



(def-test-fixture bag-fixture ()
  ((the-bag :accessor the-bag))
  (:documentation "Test fixture for bag testing"))


(defmethod initially-empty ((test bag-fixture))
  (let ((result (contents (make-instance 'bag))))
    (unless (null result)
      (failure "Bag ~A was not empty: ~A"
	       (the-bag test)
               result))))

(defmethod what-goes-in-comes-back-out ((test bag-fixture))
  (let ((the-bag (make-instance 'bag)))
    (add the-bag 'a)
    (add the-bag 'b)
    (add the-bag 'a)
    (let ((result (contents the-bag))
          (expected (list 'a 'a 'b)))
      (unless (equal (sort result 'string<)
                     expected)
        (failure "Expected ~A; got ~A"
                 expected result)))))

(defmethod subtraction ((test bag-fixture))
  (let ((a (make-instance 'bag))
        (b (make-instance 'bag)))
    (defun check-for (expected)
      (let ((result (contents (subtract a b))))
        (unless (equal (sort (copy-seq result)   'string<)
                       (sort (copy-seq expected) 'string<))
          (failure "Expected ~A; got ~A"
                   expected result))))
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
