#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec mzscheme -qr "$0" ${1+"$@"}
|#

(print-struct #t)

;; Creates trees of possible auctions.
(require "auction.ss"
         "multiply.ss"
         "call.ss"
         "map.ss"
         (lib "trace.ss")
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
;(trace all-legal-calls)
(define (some-auctions-with-given-prefix i)
  (unless (and (auction? i)
               (not (auction-complete? i)))
    (raise-type-error 'some-auctions-with-given-prefix "incomplete auction" i))

  (append-map-at-most
   (lambda (c)
     (let ((extended (copy-auction i)))
       (auction-add! extended c)
       (if (auction-complete? extended )
           (list extended)
         (some-auctions-with-given-prefix extended))))
   3
   (take-at-most 
    ;; a heuristic: don't consider auctions which have two doubles,
    ;; since they're kind of rare.
    (remove (lambda (c)
              (and (auction-has-a-double? i)
                   (double? c)))
            (all-legal-calls i))
    3))
  )

(define a (make-auction 'east))
(auction-add! a '(6 spades))
(auction-add! a 'double)
(auction-add! a 'redouble)
(pretty-display (some-auctions-with-given-prefix a))
