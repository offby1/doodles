;; Creates trees of possible auctions.
(require "auction.ss"
         "multiply.ss"
         "call.ss"
         (lib "pretty.ss")
         (lib "list.ss" "srfi" "1"))

(define-syntax false-if-exception
  (syntax-rules ()
    ((_ body-forms ...)
     (with-handlers
         ((exn:fail?
           (lambda (exn) #f)))
       body-forms ...
       #t))))

(define (would-be-legal? call incomplete-auction)
  (let ((c (copy-auction incomplete-auction)))
    (false-if-exception
     (auction-add! c call))))

(define all-legal-calls
  (let ((all-calls-period (map make-call (append (multiply (iota 7 1) '(clubs diamonds hearts spades notrump)) '(pass double redouble)))))
    (lambda (i)
      (filter (lambda (c)
                (would-be-legal? c i))
              all-calls-period)
      )))

(define (all-auctions-with-given-prefix i)
  (unless (and (auction? i)
               (not (auction-complete? i)))
    (raise-type-error 'all-auctions-with-given-prefix "incomplete auction" i))

  (append-map
   (lambda (c)
     (let ((extended (copy-auction i)))
       (auction-add! extended c)
       (if (auction-complete? extended )
           (list extended)
         (all-auctions-with-given-prefix extended))))
   (all-legal-calls i))
  )

(define a (make-auction 'east))
(auction-add! a '(7 spades))
(auction-add! a 'double)
(auction-add! a 'redouble)
(pretty-display (all-auctions-with-given-prefix a))
