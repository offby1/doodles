#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -qu "$0" ${1+"$@"}
|#

(module trick mzscheme
(print-struct #t)
(require (lib "assert.ss" "offby1")
         (lib "kw.ss")
         (only (lib "1.ss" "srfi" ) every circular-list list-tabulate)
         "card.ss"
         (lib "trace.ss"))
(provide (rename my-make-trick make-trick)
         annotated-cards
         trick-cards
         trick-complete?
         trick-ref)

(define seats (list 'north 'east 'south 'west))

(define-struct trick (cards leader) #f)

(define/kw (my-make-trick cards #:optional (leader 'unknown))
  (check-type 'make-trick list?  cards)
  (assert (not (null? cards)))
  (assert (every card? cards))
  (assert (or (eq? 'unknown leader)
              (memq leader seats)))
  (make-trick cards leader))

(define (annotated-cards t)
  (assert (memq (trick-leader t) seats))
  (let loop ((seat-circle (apply circular-list seats)))
    (if (eq? (car seat-circle)
             (trick-leader t))
        (map cons (trick-cards t) (list-tabulate (length (trick-cards t))
                                                 (lambda (i)
                                                   (list-ref seat-circle i))))
      (loop (cdr seat-circle)))))

;(trace annotated-cards)
(define (trick-ref t k)
  (list-ref (trick-cards t)
            k))
(define (trick-complete? t)
  (= (length (trick-cards t))
      4))

)