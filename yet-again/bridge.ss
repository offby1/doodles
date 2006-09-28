#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

;; same old stuff, expressed perhaps a bit more simply.

(module bridge mzscheme
(require (only (lib "1.ss" "srfi") every filter lset-intersection append-map)
         (lib "assert.ss" "offby1")
         "card.ss"
         "trick.ss"
         "history.ss"
         (lib "trace.ss"))
(provide choose-card)
(print-struct #t)


(define suits= eq?)

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
           (let* ((suit-led (card-suit (trick-ref (history-latest-trick history) 0)))
                  (mine-of-led-suit (filter (lambda (mine)
                                              (suits= (card-suit mine)
                                                      suit-led))
                                            hand)))
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


)