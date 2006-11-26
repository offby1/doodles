#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -qu "$0" ${1+"$@"}
|#

(module auction mzscheme
  (require (all-except (lib "1.ss" "srfi") reverse! member map
                       for-each assoc append!))
  (require "call.ss")
  (require "misc.ss")

  (require (planet "test.ss"    ("schematics" "schemeunit.plt" 2)))
  (require (planet "text-ui.ss" ("schematics" "schemeunit.plt" 2)))

  (provide (rename public-make-auction make-auction)
           note-call
           contract-settled?
           last-bid
           whose-turn)

  ;; Note that the calls are stored in reverse order -- that is,
  ;; (first calls) is the last call that was made.
  (define-struct auction (dealer calls))

  (define (auction->string a)
    (format "Dealer ~A; calls: ~S"
            (auction-dealer a)
            (map call->string (reverse (auction-calls a)))))

  (define (public-make-auction dealer)
    (make-auction dealer '()))

  (define (note-call a call)
    (set-auction-calls! a (cons call (auction-calls a))))

  (define (contract-settled? a)
    (let ((c (auction-calls a)))
      (and (< 3 (length c))
           (eq? 'pass (first  c))
           (eq? 'pass (second c))
           (eq? 'pass (third  c)))))

  (define (last-bid a)
     (find bid? (auction-calls a)))

  (define (whose-turn a)
    (list-ref *compass-directions*
              (+
               (remainder (length (auction-calls a)) 4)
               (index (auction-dealer a)
                      *compass-directions*))))
  (test/text-ui
   (test-suite
    "everything"

    (test-case
     "uh ..."
     (let ((a (public-make-auction 'south)))
       (check-eq? 'south (whose-turn a))
       (check-false (contract-settled? a))
       (check-false (last-bid a))
       (check-pred null? (auction-calls a))
       ))

    (test-case
     "err ..."
     (let ((a (public-make-auction 'south)))
       (note-call a 'pass)
       (check-eq? 'west (whose-turn a))
       (check-false (contract-settled? a))
       (check-false (last-bid a))
       (check-equal? 1  (length (auction-calls a)))
       ))


    (test-case
     "em ..."
     (let ((a (public-make-auction 'south)))
       (note-call a 'pass)
       (note-call a 'pass)
       (note-call a 'pass)
       (note-call a 'pass)
       (check-true (contract-settled? a))
       (check-false (last-bid a))
       (check-equal? 4  (length (auction-calls a)))
       ))

    (test-case
     "em ..."
     (let ((a (public-make-auction 'south)))
       (note-call a (make-bid 1 'spade))
       (note-call a 'pass)
       (note-call a 'pass)
       (note-call a 'pass)
       (check-true (contract-settled? a))
       (let ((expected (make-bid 1 'spade)))
         (check string=?
                 (call->string expected)
                 (call->string (last-bid a)))
         (check-equal? 4  (length (auction-calls a))))
       ))

    (test-case
     "geez ..."
     (let ((a (public-make-auction 'south)))
       (note-call a (make-bid 1 'spade))
       (note-call a 'pass)
       (note-call a 'pass)
       (note-call a (make-bid 2 'diamonds))
       (note-call a 'pass)
       (check-false (contract-settled? a))
       (let ((expected (make-bid 2 'diamonds)))
         (check string=?
                 (call->string expected)
                 (call->string (last-bid a)))
         (check-equal? 5  (length (auction-calls a))))
       (check-eq? 'west (whose-turn a))
       )))))
