(module auction mzscheme
  (require (lib "1.ss" "srfi"))
  (require "call.ss")
  (require "misc.ss")

  (require (lib "test.ss"    "schemeunit"))
  (require (lib "text-ui.ss" "schemeunit"))

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
   (make-test-suite
    "everything"

    (make-test-case
     "uh ..."
     (let ((a (public-make-auction 'south)))
       (assert-eq? 'south (whose-turn a))
       (assert-false (contract-settled? a))
       (assert-false (last-bid a))
       (assert-pred null? (auction-calls a))
       ))

    (make-test-case
     "err ..."
     (let ((a (public-make-auction 'south)))
       (note-call a 'pass)
       (assert-eq? 'west (whose-turn a))
       (assert-false (contract-settled? a))
       (assert-false (last-bid a))
       (assert-equal? 1  (length (auction-calls a)))
       ))


    (make-test-case
     "em ..."
     (let ((a (public-make-auction 'south)))
       (note-call a 'pass)
       (note-call a 'pass)
       (note-call a 'pass)
       (note-call a 'pass)
       (assert-true (contract-settled? a))
       (assert-false (last-bid a))
       (assert-equal? 4  (length (auction-calls a)))
       ))

    (make-test-case
     "em ..."
     (let ((a (public-make-auction 'south)))
       (note-call a (make-bid 1 'spade))
       (note-call a 'pass)
       (note-call a 'pass)
       (note-call a 'pass)
       (assert-true (contract-settled? a))
       (let ((expected (make-bid 1 'spade)))
         (assert string=?
                 (call->string expected)
                 (call->string (last-bid a)))
         (assert-equal? 4  (length (auction-calls a))))
       ))

    (make-test-case
     "geez ..."
     (let ((a (public-make-auction 'south)))
       (note-call a (make-bid 1 'spade))
       (note-call a 'pass)
       (note-call a 'pass)
       (note-call a (make-bid 2 'diamonds))
       (note-call a 'pass)
       (assert-false (contract-settled? a))
       (let ((expected (make-bid 2 'diamonds)))
         (assert string=?
                 (call->string expected)
                 (call->string (last-bid a)))
         (assert-equal? 5  (length (auction-calls a))))
       (assert-eq? 'west (whose-turn a))
       )))))
