#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec mzscheme -qr "$0" ${1+"$@"}
|#

(require
 (planet "test.ss" ("schematics" "schemeunit.plt" 1))
 (planet "text-ui.ss" ("schematics" "schemeunit.plt" 1))
 "contract.ss"
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
            (assert-not-exn (lambda () (auction-add! a '(4 notrump))) "Converts data to calls if necessary")
            (assert-exn exn:fail:contract? (lambda () (auction-add! a "you _really_ ugly")))
            (assert-exn exn:fail:contract? (lambda () (auction-add! a 'piss)) "Isn't fooled by non-calls")
            ))
         (make-test-case
          "accepts valid, and rejects invalid, doubles & redoubles"
          ;;; existing risk         attempted double        attempted redouble
          ;;;    undefined               reject                   reject
          ;;;      1                     accept                   reject
          ;;;      2                     reject                   accept
          ;;;      4                     reject                   reject

          (let ((undef (make-auction))
                (one   (make-auction))
                (two   (make-auction))
                (four  (make-auction)))

            (for-each (lambda (a)
                        (auction-add! a '(2 diamonds)))
                      (list one two four))

            (for-each (lambda (a)
                        (auction-add! a 'double))
                      (list two four))

            (auction-add! four 'redouble)

            (assert-exn exn:fail:contract? (lambda () (auction-add! undef   'double)) "rejects double   when risk undefined")
            (assert-exn exn:fail:contract? (lambda () (auction-add! undef 'redouble)) "rejects redouble when risk undefined")

            ;; check for failures before successes, so that the side
            ;; effect from the success doesn't interfere with the
            ;; failure check.
            (assert-exn exn:fail:contract? (lambda () (auction-add! one   'redouble)) "rejects redouble when risk is one")
            (assert-not-exn                (lambda () (auction-add! one     'double)) "accepts double   when risk is one")

            (assert-exn exn:fail:contract? (lambda () (auction-add! two     'double)) "rejects double   when risk is two")
            (assert-not-exn                (lambda () (auction-add! two   'redouble)) "accepts redouble when risk is two")

            (assert-exn exn:fail:contract? (lambda () (auction-add! four    'double)) "rejects double   when risk is four")
            (assert-exn exn:fail:contract? (lambda () (auction-add! four  'redouble)) "rejects redouble when risk is four")
            ) )
         (make-test-case
          "Recognizes a passed-out auction"
          (let ((a (make-auction)))
            (auction-add! a pass)
            (auction-add! a pass)
            (auction-add! a pass)
            (assert-false (auction-complete? a))
            (auction-add! a pass)
            (assert-true (auction-complete? a))
            (assert-eq? 'passed-out (auction-contract a))))

         (make-test-case
          "Knows the contract"
          (let ((a (make-auction)))
            (auction-add! a pass)
            (auction-add! a pass)
            (auction-add! a '(1 club))
            (assert-false (auction-complete? a))
            (auction-add! a pass)
            (auction-add! a pass)
            (auction-add! a pass)
            (assert-true (auction-complete? a))
            (let ((c  (auction-contract a)))
              (assert =   1      (level        (contract-bid c)))
              (assert eq? 'clubs (denomination (contract-bid c)))
              (assert =   1      (contract-risk c)))))

         (make-test-case
          "Gacks if we try to add an insufficient bid"
          (let ((a (make-auction)))
            (auction-add! a '(2 notrump))
            (assert-exn exn:fail:contract? (lambda () (auction-add! a '(1 clubs))))))
         ))

      ))
  (exit 0))
(exit 1)