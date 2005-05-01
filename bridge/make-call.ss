(module make-call mzscheme
;;;the entry point for bidding
  (require "tree.ss"
           "auction.ss"
           "constants.ss"
           (lib "trace.ss"))
  (provide make-call)

  (define (make-call vulnerability auction-so-far dealer)
    (auction-ref
     (best-auction-from-prefix auction-so-far)
     (auction-length auction-so-far)))

  (trace make-call)
  
  ;; a little exercise
  (if #t
      (let ((dealer 'south))
        (let loop ((a (make-auction dealer)))
          (if (auction-complete? a)
              (printf "OK, we're done:~n~a~n" (auction->string a))
            (begin
              (auction-add! a (make-call 'dunno a dealer))
              (loop a)))))
    
    (let ((a (make-auction 'south)))
      (auction-add! a 'pass)
      (auction-add! a 'pass)
      (auction-add! a 'pass)
      (auction-add! a '(7 notrump))
      (auction-add! a 'pass)
      (make-call 'dunno a 'south)
      ))

  )