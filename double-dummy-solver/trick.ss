#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -qu "$0" ${1+"$@"}
|#

(module trick mzscheme
(print-struct #t)
(require (lib "assert.ss" "offby1")
         (only (lib "1.ss" "srfi" ) last every circular-list take alist-copy reduce)
         "card.ss"
         (lib "trace.ss")
         (lib "pretty.ss"))

(provide (rename my-make-trick make-trick)
         annotated-cards
         (rename my-trick-cards trick-cards)
         mt
         (rename add-card t:add-card)
         (rename copy t:copy)
         leader
         rotate
         rotate-until
         trick?
         trick-complete?
         trick-ref
         whose-turn
         winner
         with-seat-circle
         *seats*)

(define *seats* (list 'north 'east 'south 'west))

(define (rotate seq steps)
  (if (positive? steps)
      (rotate (append (cdr seq)
                      (list (car seq)))
              (sub1 steps))
    seq))

(define (rotate-until seq criterion)
  (if (criterion seq)
      seq
    (rotate-until (rotate seq 1)
                  criterion)))

(define (rotate-until-car-eq seq sought)
  (rotate-until seq (lambda (x) (eq? (car x) sought))))

;; rotate the seats until SEAT is first, then apply the proc to the
;; list.
(define (with-seat-circle seat proc)
  (proc (rotate-until-car-eq *seats* seat)))

;; complete? is redundant -- it's always (= 4 (length
;; card-seat-pairs)).  But profiling shows that cacheing that number
;; saves noticeable time.
(define-struct trick (card-seat-pairs complete?) #f)

(define (my-make-trick cards leader)
  (define (all-distinct? seq)
    (let ((h (make-hash-table 'equal)))
      (for-each (lambda (i)
                  (hash-table-put! h i #t))
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
       (make-trick (map cons cards (take sc  l))
                   (= 4 l))))))

;(trace my-make-trick)

;; for testing -- allows me to build a trick with a lot less typing
;; e.g., (mt 'north 'c3 'c6 'c9 'cj)
(define (mt leader . card-syms)
  (let ((rv  (my-make-trick
              (map mc/quick card-syms)
              leader)))
    (check-type 'mt trick-complete? rv)
    rv))
;(trace mt)
(define (copy t)
  (make-trick (alist-copy (trick-card-seat-pairs t))
              (trick-complete? t)))

(define (leader t)
  (cdr (car (trick-card-seat-pairs t))))

(define (annotated-cards t)
  (trick-card-seat-pairs t))

(define (my-trick-cards t)
  (map car (trick-card-seat-pairs t)))

;; nondestructive, but the returned value can share structure with the
;; input.
(define (add-card t c)
  (assert (not (trick-complete? t)))
  (my-make-trick (append (my-trick-cards t)
                         (list c))
                 (leader t)))

(define (trick-ref t k)
  (list-ref (my-trick-cards t)
            k))

(define (whose-turn t)
  (assert (not (trick-complete? t)))
  (with-seat-circle
   (cdr (last (trick-card-seat-pairs t)))
   cadr))

(define (winner t)
  (assert (trick-complete? t))
  (let ((led-suit (card-suit (trick-ref t 0))))
    (define (worth card)
      (if (eq? led-suit (card-suit card))
          (card-rank card)
        0))
    (cdr
     (reduce (lambda (a b) (if (> (worth (car a))
                                  (worth (car b)))
                               a b))
             (car (trick-card-seat-pairs t))
             (trick-card-seat-pairs t)))))
;(trace winner)
)
