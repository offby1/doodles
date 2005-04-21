#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec mzscheme -qr "$0" ${1+"$@"}
|#

(print-struct #t)

;; Creates trees of possible auctions.
(require "auction.ss"
         "multiply.ss"
         "call.ss"
         "misc.ss"
         (lib "trace.ss")
         (lib "pretty.ss")
         (prefix list- (lib "list.ss"))
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

(define (call>? c1 c2)
  (define (call->int c)
    (if (bid? c)
        (bid-to-number c)
      0))
  (> (call->int c1)
     (call->int c2)))

(define all-legal-calls
  (let ((all-calls-period
         (reverse
          (list-quicksort
           (map make-call (append '(pass double redouble) (multiply (iota 7 1) '(clubs diamonds hearts spades notrump))))
           call>?))))
    (lambda (i)
      (filter (lambda (c)
                (would-be-legal? c i))
              all-calls-period)
      )))
;(trace all-legal-calls)
(define (some-auctions-with-given-prefix i)
  (define alc (all-legal-calls i))

  ;; this depeonds upon all-legal-calls being sorted in increasing order.
  (define minimum-bid (find bid? alc))

  ;; a heuristic: don't consider auctions which have two doubles,
  ;; since they're kind of rare.
  (define silly-double?
    (lambda (c)
      (and (auction-has-a-double? i)
           (double? c))))
  (define silly-competition?
    (let* ((maxes (auction-max-levels a))
           (my-side (car maxes))
           (opponents (cdr maxes)))
      (when (odd? (auction-length i))
        (swap! my-side opponents))
      
      (lambda (c)
        ;; actual bid, where opponents have bid to the four level
        (and (bid? c)
             (< 3 opponents)))))
  (define crazy-jump?
    (lambda (c)
      ;; a bid whose level is more than two more than the minimum
      ;; possible
      (and (bid? c)
           (< 2 (- (level-or-zero c)
                   (level-or-zero minimum-bid))))
      ))
  ;(trace silly-double? silly-competition? crazy-jump?)
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
    (remove silly-competition? (remove crazy-jump? (remove silly-double? alc)))
    2))
  )
;(trace some-auctions-with-given-prefix)

(define a (make-auction 'east))
(auction-add! a '(5 spades))
(auction-add! a 'double)
(auction-add! a 'redouble)
(pretty-display (some-auctions-with-given-prefix a))
