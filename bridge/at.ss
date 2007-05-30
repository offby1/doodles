#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec mzscheme -qr "$0" ${1+"$@"}
|#

(require
 (planet "test.ss" ("schematics" "schemeunit.plt" 2))
 (planet "text-ui.ss" ("schematics" "schemeunit.plt" 2))
 (planet "util.ss" ("schematics" "schemeunit.plt" 2))
 "contract.ss"
 "call.ss"
 "auction.ss"
 "exceptions.ss")

(when
    (zero?
     (test/text-ui
      (test-suite
       "Tests for the auction stuff."

       (test-case
        "error when given garbage"
        (check-exn exn:fail:bridge? (lambda () (make-auction "you ugly")))
        (check-not-exn                (lambda () (make-auction 'east)))

        ;; Didn't you know the reader is case-sensitive?
        (check-exn exn:fail:bridge? (lambda () (make-auction 'EAST)))
        )

       (test-case
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
            (check = (auction-length incomplete) (auction-length c-of-i))
            (check = (auction-length   complete) (auction-length c-of-c))
            (check eq? (auction-complete? incomplete) (auction-complete? c-of-i))
            (check eq? (auction-complete?   complete) (auction-complete? c-of-c))

            (let ((ct1 (auction-contract complete))
                  (ct2 (auction-contract c-of-c)))
              (check eq? (contract-denomination ct1) (contract-denomination ct2))
              (check eq? (contract-declarer ct1)     (contract-declarer ct2))
              (check =   (contract-level ct1)        (contract-level ct2))
              (check =   (contract-risk ct1)         (contract-risk ct2))

              (auction-add! incomplete 'pass)
              (check-false (= (auction-length incomplete) (auction-length c-of-i))
                           "Aagh!  Modifying one auction caused its copy to change!")

              )
            )))

       (test-suite
        "Rolanda"

        (test-case
         "Trivial auction"
         (let ((a (make-auction 'north)))
           (auction-add! a '(1 club))
           (check = 1 (auction-length a))
           (auction-add! a 'pass)
           (check = 2 (auction-length a))
           (check-false (auction-contract a))))

        (test-case
         "Can only add calls to an auction"
         (let ((a (make-auction 'north)))
           (check-not-exn (lambda () (auction-add! a (make-bid 3 'notrump))))
           (check-not-exn (lambda () (auction-add! a '(4 notrump))) "Converts data to calls if necessary")
           (check-exn exn:fail:bridge? (lambda () (auction-add! a "you _really_ ugly")))
           (check-exn exn:fail:bridge? (lambda () (auction-add! a 'piss)) "Isn't fooled by non-calls")
           ))
        (test-case
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

           (check-exn exn:fail:bridge? (lambda () (auction-add! undef   'double)) "rejects double   when risk undefined")
           (check-exn exn:fail:bridge? (lambda () (auction-add! undef 'redouble)) "rejects redouble when risk undefined")

           ;; check for failures before successes, so that the side
           ;; effect from the success doesn't interfere with the
           ;; failure check.
           (check-exn exn:fail:bridge? (lambda () (auction-add! one   'redouble)) "rejects redouble when risk is one")
           (check-not-exn                (lambda () (auction-add! one     'double)) "accepts double   when risk is one")

           (check-exn exn:fail:bridge? (lambda () (auction-add! two     'double)) "rejects double   when risk is two")
           (check-not-exn                (lambda () (auction-add! two   'redouble)) "accepts redouble when risk is two")

           (check-exn exn:fail:bridge? (lambda () (auction-add! four    'double)) "rejects double   when risk is four")
           (check-exn exn:fail:bridge? (lambda () (auction-add! four  'redouble)) "rejects redouble when risk is four")
           ) )
        (test-case
         "Recognizes a 'passed-out auction"
         (let ((a (make-auction 'north)))
           (auction-add! a 'pass)
           (auction-add! a 'pass)
           (auction-add! a 'pass)
           (check-false (auction-complete? a))
           (auction-add! a 'pass)
           (check-true (auction-complete? a))
           (check-eq? 'passed-out (auction-contract a))))

        (test-case
         "Knows the contract"
         (let ((a (make-auction 'north)))
           (define (expect-maxes dealer dealers-opps)
             (let ((maxes (auction-max-levels a)))
               (check = dealer (car maxes))
               (check = dealers-opps (cdr maxes))))
           (expect-maxes 0 0)
           (auction-add! a 'pass)
           (auction-add! a 'pass)
           (expect-maxes 0 0)
           (auction-add! a '(1 club))
           (expect-maxes 1 0)
           (check-false (auction-complete? a))
           (auction-add! a '(1 diamond))
           (expect-maxes 1 1)
           (auction-add! a 'pass)
           (auction-add! a '(2 diamonds))
           (auction-add! a 'pass)
           (auction-add! a 'pass)
           (auction-add! a 'pass)
           (check-true (auction-complete? a))
           (expect-maxes 1 2)
           (let ((c  (auction-contract a)))
             (check =   2      (contract-level c))
             (check eq? 'diamonds (contract-denomination c))
             (check eq? 'west (contract-declarer c))
             (check =   1      (contract-risk c)))))

        (test-case
         "Gacks if we try to add an insufficient bid"
         (let ((a (make-auction 'north)))
           (auction-add! a '(2 notrump))
           (check-exn exn:fail:bridge? (lambda () (auction-add! a '(1 clubs))) "strictly less")
           (check-exn exn:fail:bridge? (lambda () (auction-add! a '(2 notrump))) "exactly the same")))

        (test-case
         "Gacks if we add to a completed auction"
         (let ((completed (make-auction 'west)))
           (auction-add! completed '(1 notrump))
           (for-each (lambda (c)
                       (auction-add! completed c)) '(pass pass pass))
           (check-true (auction-complete? completed))
           (check-exn exn:fail? (lambda () (auction-add! completed '(2 diamonds))))
           (check-exn exn:fail? (lambda () (auction-add! completed 'pass)))
           (check-exn exn:fail? (lambda () (auction-add! completed 'double)))
           (check-exn exn:fail? (lambda () (auction-add! completed 'redouble)))
           ))

        (test-case
         "alist"
         (let ((a (make-auction 'west)))
           (auction-add! a 'pass)         ;west
           (auction-add! a '(1 diamond))  ;north
           (auction-add! a '(1 heart))    ;east
           (auction-add! a 'pass)         ;south
           (auction-add! a 'pass)         ;west
           (auction-add! a 'pass)         ;north
           (let* ((alist (auction->alist a))
                  (north (cdr (assq 'north alist)))
                  (east  (cdr (assq 'east  alist)))
                  (south (cdr (assq 'south alist)))
                  (west  (cdr (assq 'west  alist))))
             (check-equal? north (map make-call '((1 diamond) pass)))
             (check-equal? east  (map make-call '((1 heart)       )))
             (check-equal? south (map make-call '(            pass)))
             (check-equal? west  (map make-call '(pass        pass)))
             )
           ))

        )

       )))

  (exit 0))
(exit 1)
