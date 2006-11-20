#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -qu "$0" ${1+"$@"}
|#

(module trick mzscheme
(print-struct #t)
(require (lib "assert.ss" "offby1")
         (only (lib "1.ss" "srfi" )
               alist-copy
               circular-list
               every
               last
               last-pair
               reduce
               take
               )
         (only (lib "43.ss" "srfi")
               vector-copy
               vector-every
               vector-fold
               vector-for-each
               vector-map
               )
         "card.ss"
         (lib "trace.ss")
         (only (lib "etc.ss") compose)
         (lib "pretty.ss"))

(provide
         (rename add-card t:add-card)
         (rename my-make-trick make-trick)
         (rename my-trick-cards trick-cards)
         *seats*
         *trump-suit*
         annotated-cards
         augmented-rank
         last-play
         leader
         led-suit
         mt
         partner
         rotate
         rotate-until
         seat<
         trick-complete?
         trick-ref
         trick?
         whose-turn
         winner
         winner/int
         with-seat-circle
         )
(display "$Id$" (current-error-port))
(newline (current-error-port))

(define *seats* '(n e s w))
(assert (zero? (remainder (* *num-suits* *num-ranks*) (length *seats*))))
(define (seat< a b)
  (> (length (memq a *seats*))
     (length (memq b *seats*))))

(define *trump-suit*
  (make-parameter
   #f
   (let ((allowed (cons #f *suits*)))
     (lambda (s)
       (unless (memq s allowed)
         (raise-mismatch-error
          '*trump-suit*
          (format "wanted one of ~s, not " allowed)
          s))
       s))))

(define (rotate seq steps)
  (if (positive? steps)
      (rotate (append (cdr seq)
                      (list (car seq)))
              (sub1 steps))
    seq))

(define (rotate-until seq criterion)
  (define (inner seq counter)
    (unless (< counter (length seq))
      (error 'rotate-until  "We're in an endless loop! seq is ~s; criterion is ~s" seq
             criterion))
    (if (criterion seq)
        seq
      (inner (rotate seq 1) (add1 counter))))
  (inner seq 0))


(define (rotate-until-car-eq seq sought)
  (rotate-until seq (lambda (x) (eq? (car x) sought))))

;; rotate the seats until SEAT is first, then apply the proc to the
;; list.
(define (with-seat-circle seat proc)
  (check-type 'with-seat-circle (lambda (thing)
                                  (memq thing *seats*))
              seat)

  (proc (rotate-until-car-eq *seats* seat)))

(define (partner seat)
  (with-seat-circle seat caddr))

(define (trick-print trick port write?)
  (vector-for-each (lambda (i cs)
                     (fprintf port "~a:~a" (cdr cs) (car cs))
                     (if (< i (sub1 (vector-length (trick-card-seat-pairs trick))))
                         (fprintf port ", ")))
                   (trick-card-seat-pairs trick)))

(define-values (s:trick make-trick trick? s:trick-ref trick-set!)
  (make-struct-type 'trick #f 1 0 #f
                    (list (cons prop:custom-write trick-print)) #f))
(define (trick-card-seat-pairs t) (s:trick-ref t 0))
(define (trick-complete?       t)
  (= (length *seats*) (vector-length (trick-card-seat-pairs t))))

(define (my-is-trick? thing)
  (and (trick? thing)
       (let ((v (trick-card-seat-pairs thing)))
         (and (vector v)
              (vector-every pair? v)))))

(define (my-make-trick cards leader)
  (define (all-distinct? seq)
    (let ((h (make-hash-table 'equal)))
      (for-each (lambda (i)
                  (hash-table-put! h (card->number i #f) #t))
                seq)
      (= (length seq)
         (hash-table-count h))))
  (check-type 'make-trick list?  cards)
  (check-type 'make-trick (lambda (c) (not (null? c))) cards)
  (check-type 'make-trick (lambda (cs) (every card? cs)) cards)
  (check-type 'make-trick all-distinct? cards)
  (check-type 'make-trick (lambda (leader) (memq leader *seats*)) leader)
  ;;(printf "Making trick led by ~a ... " leader)
  (with-seat-circle
   leader
   (lambda (sc)
     (let ((l (length cards)))
       (make-trick (list->vector (map cons cards (take sc  l))))))))

;(trace my-make-trick)

;; for testing -- allows me to build a trick with a lot less typing
;; e.g., (mt 'north 'c3 'c6 'c9 'cj)
(define (mt* leader . card-syms)
  (let ((rv  (my-make-trick
              (map mc* card-syms)
              leader)))
    rv))
;(trace mt)

;; similar to mt, but even nicer: I don't have to quote the symbols.
(define-syntax mt
  (syntax-rules ()
    ((_ leader card-syms ...)
     (apply mt* `(leader card-syms ...)))))

(define (leader t)
  (cdr (vector-ref (trick-card-seat-pairs t) 0)))

(define (annotated-cards t)
  (vector->list (trick-card-seat-pairs t)))

(define (my-trick-cards t)
  (map car (vector->list (trick-card-seat-pairs t))))

;; nondestructive
(define (add-card t c)
  (assert (my-is-trick? t))
  (assert (not (trick-complete? t)))
  (let* ((new-length (add1 (vector-length (trick-card-seat-pairs t))))
         (v (vector-copy (trick-card-seat-pairs t) 0 new-length))
         (next-seat (with-seat-circle (cdr (vlast (trick-card-seat-pairs t))) cadr)))
    (vector-set! v (sub1 new-length) (cons c  next-seat))
    (make-trick v)))
;;(trace add-card)
(define (trick-ref t k)
  (car (vector-ref (trick-card-seat-pairs t)
                   k)))

(define (vlast v)
  (vector-ref v (sub1 (vector-length v))))

(define (last-play t)
  (let ((pairs (trick-card-seat-pairs t)))
    (assert (not (zero? (vector-length pairs))))
    (car (vlast pairs))))

(define (whose-turn t)
  (assert (not (trick-complete? t)))
  (with-seat-circle
   (cdr (vlast (trick-card-seat-pairs t)))
   cadr))

(define (led-suit t)
   (card-suit (trick-ref t 0)))

;; the "trick-taking power" of this card.  It's pretty much the rank,
;; except: if it's in the trump suit, it gets a boost of 13 so that it
;; beats all cards from all other suits; and if we were passed a
;; trick, and it's not of the led suit, then its power is 0.
(define (augmented-rank card . t)
  (when (pair? t)
    (set! t (car t)))

  (cond
   ((eq? (*trump-suit*)
         (card-suit card))
    (+ (card-rank card) *num-ranks*))
   ((and t
         (not (null? t))
         (not (eq? (led-suit t)
                   (card-suit card))))
    0)
   (else
    (card-rank card)))  )

;;(trace augmented-rank)

;; returns just a seat
(define (winner t)
  (cdr (winner/int t)))

;; returns (cons card seat)

;; TODO -- consider keeping statistics on whether the players followed
;; the dictum "second hand low; third hand high".  It might turn out
;; that they mostly do, which would be cool, since I never explcitily
;; taught them to.
(define (winner/int t)
  (assert (trick-complete? t))
  (vector-fold
   (lambda (index state element)
     (if (> (augmented-rank (car element) t)
            (augmented-rank (car state) t))
         element
       state))
   (vector-ref (trick-card-seat-pairs t) 0)
   (trick-card-seat-pairs t)))

;(trace winner)
)

