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

(define *the-channel* (make-channel))
(define *consumers-favorite* #f)
(define *producer-keep-going* #t)

(define (thing-score thing)
  (if thing
      (* 360 (- (modulo (equal-hash-code thing) 20) 10))
    0))

(define (thing-max t1 t2)
  
  (define (thing>? t1 t2)
    (> (thing-score t1)
       (thing-score t2)))

  (if (thing>? t1 t2)
      (begin
        (printf "Ooh -- ~s (score ~a) > ~s (score ~a)~n"
                t1 (thing-score t1)
                t2 (thing-score t2))
        t1)
    t2))

(define consumer
  (lambda ()
    (let loop ((ca (channel-get *the-channel*))
               (passes 10))
      (if (zero? passes)
          (begin
            (printf "Consumer: used up its passes, so that means it's time to quit.~n")
            (set! *producer-keep-going* #f))
        (begin
          (printf "Consumer: got a completed auction: ~s~n" ca)
          (set! *consumers-favorite* (thing-max ca *consumers-favorite*))
          (loop (channel-get *the-channel*)
                (- passes 1))))
      
      )))

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

  (for-each
   (lambda (c)
     (when *producer-keep-going*
       (let ((extended (copy-auction i)))
         (auction-add! extended c)
         (if (auction-complete? extended )
             (channel-put *the-channel* extended)
           (some-auctions-with-given-prefix extended)))))
   alc)
  )
;(trace some-auctions-with-given-prefix)

(define a (make-auction 'east))
(auction-add! a '(4 spades))
(auction-add! a 'double)
(auction-add! a 'redouble)
(define consumer-thread-id (thread consumer))
(some-auctions-with-given-prefix a)
(printf "Best thing the consumer saw so far: ~s~n" (cons (thing-score *consumers-favorite*)
                                                         *consumers-favorite*))
