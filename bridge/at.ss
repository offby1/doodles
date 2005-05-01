#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec mzscheme -M errortrace -qr "$0" ${1+"$@"}
|#

(require
 (planet "test.ss" ("schematics" "schemeunit.plt" 1))
 (planet "text-ui.ss" ("schematics" "schemeunit.plt" 1))
 (planet "util.ss" ("schematics" "schemeunit.plt" 1))
 "contract.ss"
 "call.ss"
 "auction.ss"
 "tree.ss"
 "exceptions.ss")

(when
    (test/text-ui
     (make-test-suite
      "Tests for the auction stuff."

      (make-test-case
       "error when given garbage"
       (assert-exn exn:fail:bridge? (lambda () (make-auction "you ugly")))
       (assert-not-exn                (lambda () (make-auction 'east)))

       ;; Didn't you know the reader is case-sensitive?
       (assert-exn exn:fail:bridge? (lambda () (make-auction 'EAST)))
       )

      (make-test-case
       "Copying"
       
       (let ((incomplete (make-auction 'south))
             (complete (make-auction 'east)))
         (auction-add! incomplete '(1 club))
         (auction-add! incomplete 'pass)
         (for-each (lambda (c)
                     (auction-add! complete c))
                   '(pass (2 diamonds) pass pass pass))
         
         (let ((c-of-i (copy-auction incomplete))
               (c-of-c (copy-auction complete)))
           (assert = (auction-length incomplete) (auction-length c-of-i))
           (assert = (auction-length   complete) (auction-length c-of-c))
           (assert eq? (auction-complete? incomplete) (auction-complete? c-of-i))
           (assert eq? (auction-complete?   complete) (auction-complete? c-of-c))

           (let ((ct1 (auction-contract complete))
                 (ct2 (auction-contract c-of-c)))
             (assert eq? (contract-denomination ct1) (contract-denomination ct2))
             (assert eq? (contract-declarer ct1)     (contract-declarer ct2))
             (assert =   (contract-level ct1)        (contract-level ct2))
             (assert =   (contract-risk ct1)         (contract-risk ct2))

             (auction-add! incomplete 'pass)
             (assert-false (= (auction-length incomplete) (auction-length c-of-i))
                           "Aagh!  Modifying one auction caused its copy to change!")
             
             )
           )))


      (let ((one-club      (make-call 1 'clubs))
            (pass          (make-call 'pass))
            (seven-notrump (make-call 7 'notrump)))
        (make-test-suite
         "Rolanda"

         (make-test-case
          "Trivial auction"
          (let ((a (make-auction 'north)))
            (auction-add! a one-club)
            (assert = 1 (auction-length a))
            (auction-add! a pass)
            (assert = 2 (auction-length a))
            (assert-false (auction-contract a))))

         (make-test-case
          "Can only add calls to an auction"
          (let ((a (make-auction 'north)))
            (assert-not-exn (lambda () (auction-add! a (make-bid 3 'notrump))))
            (assert-not-exn (lambda () (auction-add! a '(4 notrump))) "Converts data to calls if necessary")
            (assert-exn exn:fail:bridge? (lambda () (auction-add! a "you _really_ ugly")))
            (assert-exn exn:fail:bridge? (lambda () (auction-add! a 'piss)) "Isn't fooled by non-calls")
            ))
         (make-test-case
          "accepts valid, and rejects invalid, doubles & redoubles"
          ;;; existing risk         attempted double        attempted redouble
          ;;;    undefined               reject                   reject
          ;;;      1                     accept                   reject
          ;;;      2                     reject                   accept
          ;;;      4                     reject                   reject

          (let ((undef (make-auction 'north))
                (one   (make-auction 'north))
                (two   (make-auction 'north))
                (four  (make-auction 'north)))

            (for-each (lambda (a)
                        (auction-add! a '(2 diamonds)))
                      (list one two four))

            (for-each (lambda (a)
                        (auction-add! a 'double))
                      (list two four))

            (auction-add! four 'redouble)

            (assert-exn exn:fail:bridge? (lambda () (auction-add! undef   'double)) "rejects double   when risk undefined")
            (assert-exn exn:fail:bridge? (lambda () (auction-add! undef 'redouble)) "rejects redouble when risk undefined")

            ;; check for failures before successes, so that the side
            ;; effect from the success doesn't interfere with the
            ;; failure check.
            (assert-exn exn:fail:bridge? (lambda () (auction-add! one   'redouble)) "rejects redouble when risk is one")
            (assert-not-exn                (lambda () (auction-add! one     'double)) "accepts double   when risk is one")

            (assert-exn exn:fail:bridge? (lambda () (auction-add! two     'double)) "rejects double   when risk is two")
            (assert-not-exn                (lambda () (auction-add! two   'redouble)) "accepts redouble when risk is two")

            (assert-exn exn:fail:bridge? (lambda () (auction-add! four    'double)) "rejects double   when risk is four")
            (assert-exn exn:fail:bridge? (lambda () (auction-add! four  'redouble)) "rejects redouble when risk is four")
            ) )
         (make-test-case
          "Recognizes a passed-out auction"
          (let ((a (make-auction 'north)))
            (auction-add! a pass)
            (auction-add! a pass)
            (auction-add! a pass)
            (assert-false (auction-complete? a))
            (auction-add! a pass)
            (assert-true (auction-complete? a))
            (assert-eq? 'passed-out (auction-contract a))))

         (make-test-case
          "Knows the contract"
          (let ((a (make-auction 'north)))
            (define (expect-maxes dealer dealers-opps)
              (let ((maxes (auction-max-levels a)))
                (assert = dealer (car maxes))
                (assert = dealers-opps (cdr maxes))))
            (expect-maxes 0 0)
            (auction-add! a pass)
            (auction-add! a pass)
            (expect-maxes 0 0)
            (auction-add! a '(1 club))
            (expect-maxes 1 0)
            (assert-false (auction-complete? a))
            (auction-add! a '(1 diamond))
            (expect-maxes 1 1)
            (auction-add! a pass)
            (auction-add! a '(2 diamonds))
            (auction-add! a pass)
            (auction-add! a pass)
            (auction-add! a pass)
            (assert-true (auction-complete? a))
            (expect-maxes 1 2)
            (let ((c  (auction-contract a)))
              (assert =   2      (contract-level c))
              (assert eq? 'diamonds (contract-denomination c))
              (assert eq? 'west (contract-declarer c))
              (assert =   1      (contract-risk c)))))

         (make-test-case
          "Gacks if we try to add an insufficient bid"
          (let ((a (make-auction 'north)))
            (auction-add! a '(2 notrump))
            (assert-exn exn:fail:bridge? (lambda () (auction-add! a '(1 clubs))) "strictly less")
            (assert-exn exn:fail:bridge? (lambda () (auction-add! a '(2 notrump))) "exactly the same")))

         (make-test-case
          "Exercise best-auction-from-prefix"
          (let ((bsa (best-auction-from-prefix  (make-auction 'south))))
            (when bsa
              (let ((s (auction->string bsa)))
                (assert-regexp-match "^S +W +N +E +\n-+\np- +p- +p- +" s "Auction string don't look right!")
                )
              )))
         
         (make-test-case
          "Gacks if we add to a completed auction"
          (let ((completed (make-auction 'west)))
            (auction-add! completed '(1 notrump))
            (for-each (lambda (c)
                        (auction-add! completed c)) '(pass pass pass))
            (assert-true (auction-complete? completed))
            (assert-exn exn:fail? (lambda () (auction-add! completed '(2 diamonds))))
            (assert-exn exn:fail? (lambda () (auction-add! completed 'pass)))
            (assert-exn exn:fail? (lambda () (auction-add! completed 'double)))
            (assert-exn exn:fail? (lambda () (auction-add! completed 'redouble)))
            ))
         
         ))

      ))
    
  (exit 0))
(exit 1)
