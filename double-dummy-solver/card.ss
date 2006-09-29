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
         cards=
         card<
         *suits*)

(define *suits*  '(clubs diamonds hearts spades))
(define *num-ranks* 13)

(define-struct card (suit rank) #f)
(define my-make-card
  (lambda (suit rank )
    (unless (memq suit *suits*)
      (error make-card
             "first field must be a suit"))
    (unless (and (exact? rank)
                 (integer? rank)
                 (<= 2 rank (add1 *num-ranks*)))
      (error make-card
             "second field must be a number 'twixt 2 and 14"))
    (make-card suit rank)))

(define (cards= a b)
  (and (eq? (card-suit a)
          (card-suit b))
       (= (card-rank a)
            (card-rank b))
       ))

(define (card< a b)
  (define (card->number c)

    (define (suit->number s)
      (- (length *suits*)
         (length (member s *suits*))))
    (define (rank->number r)
      (- r 2))

    (+ (* *num-ranks* (suit->number (card-suit c)))
       (rank->number (card-rank c))))
  (< (card->number a)
     (card->number b)))
)
