#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec mzscheme -qr "$0" ${1+"$@"}
|#

(require
 (planet "test.ss" ("schematics" "schemeunit.plt" 1))
 (planet "text-ui.ss" ("schematics" "schemeunit.plt" 1))
 "call.ss"
 "auction.ss")

(when
    (test/text-ui
     (make-test-suite
      "Tests for the auction stuff."

      (make-test-case
       "error when given garbage"
       (assert-exn exn:fail:contract? (lambda () (make-auction "you ugly"))))

      (let ((one-club      (make-call 1 'clubs))
            (pass          (make-call 'pass))
            (seven-notrump (make-call 7 'notrump)))
        (make-test-suite
         "Rolanda"
   
         (make-test-case
          "Trivial auction"
          (let ((a (make-auction)))
            (auction-add! a one-club)
            (assert = 1 (auction-length a))
            (auction-add! a pass)
            (assert = 2 (auction-length a))
            (assert-false (auction-contract a))))

         (make-test-case
          "Can only add calls to an auction"
          (let ((a (make-auction)))
            (assert-not-exn (lambda () (auction-add! a (make-bid 3 'notrump))))
            (assert-exn exn:fail:contract? (lambda () (auction-add! a "you _really_ ugly")))
            ))

         (make-test-case
          "Knows the contract"
          (let ((a (make-auction)))
            (auction-add! a pass)
            (auction-add! a pass)
            (auction-add! a pass)
            (assert-false (auction-complete? a))
            (auction-add! a pass)
            (assert-true (auction-complete? a))
            (assert-false (auction-contract a))))))
  
      ))
  (exit 0))
(exit 1)