#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec mzscheme -qr "$0" ${1+"$@"}
|#

(require
 (planet "test.ss" ("schematics" "schemeunit.plt" 1))
 (planet "text-ui.ss" ("schematics" "schemeunit.plt" 1))
 "call.ss"
 "auction.ss")

(test/text-ui
 (make-test-suite
  "Tests for the auction stuff."

  (make-test-case
   "error when given garbage"
   (assert-exn exn:fail:contract? (lambda () (make-auction "you ugly"))))

  (make-test-suite
   "Rolanda"
   (let ((one-club      (make-call 1 'clubs))
         (pass          (make-call 'pass))
         (seven-notrump (make-call 7 'notrump)))

     (make-test-case
      "Trivial auction"
      (let ((a (make-auction)))
        (auction-add! a one-club)
        (assert = 1 (auction-length a))
        (assert-false (auction-contract a))))

     (make-test-case
      "Can only add calls to an auction"
      (let ((a (make-auction)))
        (assert-exn exn:fail:contract? (lambda () (auction-add! a "you _really_ ugly")))
        (assert-exn exn:fail:contract? (lambda () (auction-add! a (make-bid 3 'notrump))))
        ))))
  ))