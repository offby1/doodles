#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -qu "$0" ${1+"$@"}
|#

(module trick mzscheme
(print-struct #t)
(require (lib "assert.ss" "offby1")
         (only (lib "1.ss" "srfi" ) every)
         "card.ss")
(provide (rename my-make-trick make-trick)
         trick-cards
         trick-complete?
         trick-ref)

(define-struct trick (cards) #f)
(define (my-make-trick cards)
  (assert (list? cards))
  (assert (every card? cards))
  (make-trick cards))
(define (trick-ref t k)
  (list-ref (trick-cards t)
            k))
(define (trick-complete? t)
  (= (length (trick-cards) 4)))

)