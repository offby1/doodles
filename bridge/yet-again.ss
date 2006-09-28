#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

;; same old stuff, expressed perhaps a bit more simply.

(module yet-again mzscheme
(require (only (lib "1.ss" "srfi") every filter lset-intersection append-map))

(print-struct #t)
(define-syntax assert
  (syntax-rules ()
    ((assert _expr)
     (or _expr
         (error "failed assertion: " '_expr)))))

;; I'm using make-struct-type rather than define-struct here, simply
;; so that I can provide a guard procedure.
(define-values (struct:card make-card card? card-ref card-set!)
  (make-struct-type 'card #f 2 0 #f null #f #f '(0 1)
                    ;; Guard checks for a number, and makes it inexact
                    (lambda (suit rank name)
                      (unless (memq suit '(clubs diamonds hearts spades))
                        (error (string->symbol (format "make-~a" name))
                               "first field must be a suit"))
                      (unless (and (exact? rank)
                                   (integer? rank)
                                   (<= 2 rank 14))
                        (error (string->symbol (format "make-~a" name))
                               "second field must be a number 'twixt 2 and 14"))
                      (values suit rank))))

(define (card-suit c) (card-ref c 0))
(define suits= =)
(define-struct trick (cards) #f)
(define (trick-complete? t)
  (= (length (trick-cards) 4)))

(define-struct history (tricks) #f)
(define (history-length h)
  (vector-length (history-tricks h)))
(define (history-empty? h)
  (zero? (history-length h)))
(define (history-latest-trick h)
  (vector-ref (history-tricks h)
              (history-length h)))
(define (history-complete? h)
  (and (= 13 (history-length h))
       (trick-complete? (history-latest-trick h))))
(define (history-card-set h)
  (append-map trick-cards (vector->list (history-tricks h))))
;;; choose-card

;; sequence of tricks, set of cards -> card

;; the sequence of tricks is the history of the play so far.

;; the set of cards is the hand.

;; preconditions: no card appears in both the sequence and the set.
;; the history is incomplete.

;; postconditions: the returned card is in the set, and follows the
;; rules of bridge (i.e., it is the same suit as the card that led the
;; current trick, if it can be)

;; find which cards are legal (i.e., which follow suit)
;; if there's exactly one, play it.
;; otherwise, predict the score from playing each card; play the highest-scoring one.

(define (choose-card history hand)
  (assert (not (history-complete? history)))
  (assert (null? (lset-intersection eq? (history-card-set history)
                                    hand)))
  (assert (every card? hand))
  (let ((legal-choices
         (if (history-empty? history)
             hand
           (let* ((suit-led (card-suit (car (history-latest-trick history))))
                  (mine-of-led-suit (filter (lambda (mine)
                                              (suits= (card-suit mine)
                                                      suit-led)))))
             (if (null? mine-of-led-suit)
                 hand
               mine-of-led-suit)))))
    (let ((choice (car legal-choices)))
      (assert (card? choice))
      (assert (memq choice hand))
      choice)))

;;; predict-score

;; same inputs as choose-card, plus a single card.

;; extra precondition: the single card could have been returned from a call to choose-card.

;; add this card to the last trick in the sequence.

;; let the current player be the guy to our left
;; while the history is thus incomplete
;;   call choose-card with the current player
;;   modify the history
;;   bump the current player

;;   count the number of tricks won by us, and by them, and subtract;
;;   that's the answer.


(printf "A card: ~s~%"
        (choose-card (make-history (vector))
                     (list (make-card 'spades 2))))
)