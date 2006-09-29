#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -qu "$0" ${1+"$@"}
|#

(module card mzscheme
(print-struct #t)
(require (lib "assert.ss" "offby1")
         (lib "trace.ss"))
(provide (rename my-make-card make-card)
         card?
         card-suit
         card-rank
         cards=)

(define-struct card (suit rank) #f)
(define my-make-card
  (lambda (suit rank )
    (unless (memq suit '(clubs diamonds hearts spades))
      (error make-card
             "first field must be a suit"))
    (unless (and (exact? rank)
                 (integer? rank)
                 (<= 2 rank 14))
      (error make-card
             "second field must be a number 'twixt 2 and 14"))
    (make-card suit rank)))

(define (cards= a b)
  (and (eq? (card-suit a)
          (card-suit b))
       (= (card-rank a)
            (card-rank b))
       ))
)