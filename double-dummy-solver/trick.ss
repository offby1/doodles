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
               vector-for-each
               vector-map
               )
         "card.ss"
         (lib "trace.ss")
         (only (lib "etc.ss") compose)
         (lib "pretty.ss"))

(provide (rename my-make-trick make-trick)
         annotated-cards
         (rename my-trick-cards trick-cards)
         mt
         (rename add-card t:add-card)
         led-suit
         last-play
         leader
         partner
         rotate
         rotate-until
         trick?
         trick-complete?
         trick-ref
         whose-turn
         winner
         with-seat-circle
         *seats*)

(define *seats* '(n e s w))

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
  (= 4 (vector-length (trick-card-seat-pairs t))))

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

(define (winner t)
  (define (worth card)
    (if (eq? (led-suit t)
             (card-suit card))
        (card-rank card)
      0))
  (assert (trick-complete? t))
  (cdr
   (let ((cards (trick-card-seat-pairs t)))
     (let loop ((best (vector-ref cards 0))
                (examined 1))
       (if (= examined (vector-length cards))
           best
         (let ((this (vector-ref cards examined)))
           (loop (if (> (worth (car this))
                        (worth (car best)))
                     this best)
                 (add1 examined))))))))

;(trace winner)
)

