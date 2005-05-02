;; make-call is a confusing name, since this directory contains a
;; module named 'call' that defines (but thankfully doesn't provide) a
;; function by that name.
(module make-call mzscheme
;;;the entry point for bidding
  (require "tree.ss"
           "auction.ss"
           "constants.ss"
           "call.ss"
           (lib "trace.ss"))

  (define (do-some-bidding vulnerability auction-so-far dealer)
    (auction-ref
     (best-auction-from-prefix auction-so-far)
     (auction-length auction-so-far)))

  ;; a little exercise
  (if #t
      (let ((dealer 'south))
        (let loop ((a (make-auction dealer)))
          (if (auction-complete? a)
              (printf "OK, we're done:~n~a~n" (auction->string a))
            (let ((new-call (do-some-bidding 'dunno a dealer)))
              (auction-add! a new-call)
              (printf "~a~n" (call->string new-call))
              (loop a)))))
    
    (let ((a (make-auction 'south)))
      (auction-add! a 'pass)
      (auction-add! a 'pass)
      (auction-add! a 'pass)
      (auction-add! a '(7 notrump))
      (auction-add! a 'pass)
      (do-some-bidding 'dunno a 'south)
      ))

  )