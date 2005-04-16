#! /bin/sh
#|
exec mzscheme -qr "$0" ${1+"$@"}
|#

(require
 (planet "test.ss" ("schematics" "schemeunit.plt" 1))
 (planet "text-ui.ss" ("schematics" "schemeunit.plt" 1))
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
   ))

(test/text-ui file-tests)
