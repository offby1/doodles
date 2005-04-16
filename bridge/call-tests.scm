#! /bin/sh
#|
exec mzscheme -qr "$0" ${1+"$@"}
|#

(require
 (planet "test.ss" ("schematics" "schemeunit.plt" 1))
 (planet "text-ui.ss" ("schematics" "schemeunit.plt" 1))
 ;(planet "graphical-ui.ss" ("schematics" "schemeunit.plt" 1))
 (planet "util.ss" ("schematics" "schemeunit.plt" 1)))

(require/expose  "calls.ss" (*all-bids* bid->number))

(define file-tests
  (make-test-suite
   "Tests for calls.ss"
   (make-test-case
    "35 bids"
    (assert = 35 (length *all-bids*)))
   
   (make-test-case
    "All bids increasing"
    (assert-true (apply < (map bid->number *all-bids*)) "I say, all bids increasing."))

   (make-test-case
    "sam"
    (assert = 0 (length (all-legal-calls-I-could-make-now '(pass pass (1 diamond) you uggly (7 notrump)))))
    (assert = 5 (length (all-legal-calls-I-could-make-now '(pass pass (1 diamond) you uggly (6 notrump))))))

   (make-test-case
    "completion"
    (assert-true (auction-is-completed '(pass pass pass pass)))
    (assert-false(auction-is-completed '(pass pass pass)))
    (assert-true (auction-is-completed '((2 diamonds) pass pass pass))))
   ))

(test/text-ui file-tests)
;(test/graphical-ui file-tests)
