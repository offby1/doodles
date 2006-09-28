#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -qu "$0" ${1+"$@"}
|#

;; same old stuff, expressed perhaps a bit more simply.

(module yet-again mzscheme
(require (only (lib "1.ss" "srfi") lset-intersection append-map))

(define-syntax assert
  (syntax-rules ()
    ((assert _expr)
     (or _expr
         (error "failed assertion: " '_expr)))))

(define-struct trick (cards) #f)
(define (trick-complete? t)
  (= (length (trick-cards) 4)))

(define-struct history (tricks) #f)
(define (history-length h)
  (vector-length (history-tricks h)))
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

(define (choose-card history hand)
  (assert (not (history-complete? history)))
  (assert (null? (lset-intersection eq? (history-card-set history)
                                        hand)))
  (let ((choice (car hand)))
    (assert (memq choice hand))
    choice))

;; find which cards are legal (i.e., which follow suit)
;; if there's exactly one, play it.
;; otherwise, predict the score from playing each card; play the highest-scoring one.

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
                     '(1 2 3 4)))
)