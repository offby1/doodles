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
         (rename add-card t:add-card)
         (rename copy t:copy)
         leader
         trick?
         trick-complete?
         trick-ref
         whose-turn
         winner
         *seats*)

(define *seats* (list 'north 'east 'south 'west))

(define (with-seat-circle seat proc)
  (let loop ((seat-circle (apply circular-list *seats*)))
    (if (eq? (car seat-circle) seat)
        (proc seat-circle)
      (loop (cdr seat-circle)))))

(define-struct trick (card-seat-pairs) #f)

(define (my-make-trick cards leader)
  (check-type 'make-trick list?  cards)
  (assert (not (null? cards)))
  (assert (every card? cards))
  (assert (memq leader *seats*))
  ;;(printf "Making trick led by ~a ... " leader)
  (with-seat-circle
   leader
   (lambda (sc)
     (make-trick (map cons cards (take sc  (length cards)))))))

;(trace my-make-trick)

(define (copy t)
  (make-trick (alist-copy (trick-card-seat-pairs t))))

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
(define (trick-complete? t)
  (= (length (trick-card-seat-pairs t))
      4))

(define (whose-turn t)
  (assert (not (trick-complete? t)))
  (with-seat-circle
   (cdr (last (trick-card-seat-pairs t)))
   cadr))

(define (winner t)
  (assert (trick-complete? t))
  (let* ((led-suit (card-suit (trick-ref t 0)))
         (rank-seat-pairs
          (map (lambda (c-s)
                 (let* ((card (car c-s))
                        (rank (if (eq? led-suit (card-suit card))
                                  (card-rank card)
                                0))
                        (seat (cdr c-s)))
                   (cons rank seat)))
               (trick-card-seat-pairs t))))
    (cdr
     (reduce (lambda (a b)
               (if (> (car a) (car b))
                   a
                 b))
             (car rank-seat-pairs)
             rank-seat-pairs))))
;(trace winner)
)
