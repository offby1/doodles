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
         trick?
         trick-complete?
         trick-ref
         whose-turn
         winner
         *seats*)

(define *seats* (list 'north 'east 'south 'west))

(define with-seat-circle
  (let ((seat-circle (apply circular-list *seats*)))
    (lambda (seat proc)
      (let loop ((seat-circle seat-circle))
        (if (eq? (car seat-circle) seat)
            (proc seat-circle)
          (loop (cdr seat-circle)))))))

;; complete? is redundant -- it's always (= 4 (length
;; card-seat-pairs)).  But profiling shows that cacheing that number
;; saves noticeable time.
(define-struct trick (card-seat-pairs complete?) #f)

(define (my-make-trick cards leader)
  (check-type 'make-trick list?  cards)
  (assert (not (null? cards)))
  (assert (every card? cards))
  (assert (memq leader *seats*))
  ;;(printf "Making trick led by ~a ... " leader)
  (with-seat-circle
   leader
   (lambda (sc)
     (let ((l (length cards)))
       (make-trick (map cons cards (take sc  l)) (= 4 l))))))

;(trace my-make-trick)

;; for testing -- allows me to build a trick with a lot less typing
;; e.g., (mt 'north 'c3 'c6 'c9 'cj)
(define (mt leader . card-syms)
  (my-make-trick
   (map (lambda (pair)
          (let ((suit (string->symbol (string (car pair))))
                (rank (cond
                       ((char-numeric? (cadr pair))
                        (- (char->integer (cadr pair))
                           (char->integer #\0)))
                       (else
                        (case (cadr pair)
                          ((#\t) 10)
                          ((#\j) 11)
                          ((#\q) 12)
                          ((#\k) 13)
                          ((#\a) 14)
                          (else (error "Bad character: " (cadr pair)))))

                       )))
            (make-card suit rank)))
        (map string->list (map symbol->string card-syms)))
   leader))

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
     (reduce (lambda (a b) (if (> (car a) (car b)) a b))
             (car rank-seat-pairs)
             rank-seat-pairs))))
;(trace winner)
)
