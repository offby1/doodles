#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

;; same old stuff, expressed perhaps a bit more simply.

(module bridge mzscheme
(require (only (lib "1.ss" "srfi")
               append-map
               circular-list
               every
               filter
               last-pair
               lset-intersection
               remove
               take
               )
         (lib "pretty.ss")
         (lib "assert.ss" "offby1")
         "card.ss"
         "trick.ss"
         (all-except "history.ss" whose-turn)
         (rename "history.ss" history:whose-turn whose-turn)
         (prefix ha: "hand.ss")
         (only (lib "list.ss") sort)
         (lib "trace.ss"))
(provide choose-card play-loop)
(print-struct #t)


;; (list 1 2 3 9 10 11 7 6 5) (lambda (a b) (= a (add1 b))) => ((1 2
;; 3) (9 10 11) (7) (6) (5))
(define (group-into-adjacent-runs seq adjacent?)
  (let loop ((in seq)
             (out '()))
    (if (null? in)
        (reverse (map reverse out))
      (let ((this (car in)))
        (loop (cdr in)
              (if (or (null? out)
                      (not (adjacent? this (caar out))))
                  (cons (list this) out)
                (cons (cons this (car out))
                      (cdr out))))))))

(define suits= eq?)
(define (hi-lo seq)
  (cons (car seq)
        (last-pair seq)))

(define *recursion-level* (make-parameter 0))

(define (play-loop history hands num-tricks max-lookahead termination-history-proc)
  (let ((ha (car hands)))
    (if (or (zero? num-tricks)
            (ha:empty? ha))
        (termination-history-proc history)
      (let* ((choice (choose-card history hands max-lookahead))
             (new-hi (add-card history choice))
             (new-hands (append (cdr hands)
                                (list (ha:remove-card ha choice)))))

        (when (zero? (*recursion-level*))
          (printf "~a plays ~a~%" (history:whose-turn history) (ca->string choice))
          (when  (hi:trick-complete? new-hi)
            (newline))

          ;; rotate the hands until the winner is in front.
          (when (and (hi:trick-complete? new-hi)
                     (not (history-complete? new-hi)))
            (let loop ((hc (apply circular-list new-hands))
                       (rotations 0))
              (when (= 4 rotations)
                (pretty-display hc)
                (error (format "Oh shit, our hand circle doesn't contain the winner ~s: "
                               (history:whose-turn new-hi))
                       'damn-it)
                )
              (if (eq? (ha:seat (car hc))
                       (history:whose-turn new-hi))
                  (set! new-hands (take hc 4))
                (loop (cdr hc)
                      (add1 rotations))))))

        (play-loop new-hi
                   new-hands
                   (if (hi:trick-complete? new-hi)
                       (sub1 num-tricks)
                     num-tricks)
                   max-lookahead
                   termination-history-proc)))))

;;; choose-card

;; sequence of tricks, list of (set of cards) -> card

;; the sequence of tricks is the history of the play so far.

;; the (first of the) set of cards is our hand.

;; preconditions: no card appears in both the sequence and the set.
;; the history is incomplete.

;; postconditions: the returned card is in the set, and follows the
;; rules of bridge (i.e., it is the same suit as the card that led the
;; current trick, if it can be)

;; find which cards are legal (i.e., which follow suit)
;; if there's exactly one, play it.
;; otherwise, predict the score from playing each card; play the highest-scoring one.

(define (choose-card history hands max-lookahead)

  (define us (history:whose-turn history))

;;; predict-score

  ;; same inputs as choose-card, plus a single card, plus the number
  ;; of tricks into the future to look.

  ;; extra precondition: the single card could have been returned from
  ;; a call to choose-card.

  ;; add this card to the last trick in the sequence.

  ;; let the current player be the guy to our left
  ;; while the history is thus incomplete and max-lookahead > 0
  ;;   call choose-card with the current player
  ;;   modify the history
  ;;   bump the current player

  ;;   count the number of tricks won by us, and by them, and subtract;
  ;;   that's the answer.

  (define (predict-score card max-lookahead)
    (define (we-won? t) (eq? us (winner t)))
    (define (our-trick-score history)
      (apply + (map (lambda (t) (if (we-won? t) 1 -1))
                    (filter trick-complete? (history-tricks history)))))
    (parameterize
        ((*recursion-level* (add1 (*recursion-level*))))
      (play-loop
       (add-card history card)
       (cons (ha:remove-card (car hands) card)
             (cdr hands))
       max-lookahead
       (sub1 max-lookahead)
       our-trick-score)))

  ;(trace predict-score)
  (let ((hand (car hands)))
    (define already-played-cards
      (lset-intersection eq? (history-card-set history) (ha:cards hand)))
    (define (already-played? c)
      (member c already-played-cards))
    (check-type 'choose-card history? history)
    (unless (ha:hand? hand)
      (raise-mismatch-error 'choose-card "Not a list of hands: " hands))
    (if (ha:empty? hand)
        (raise-mismatch-error 'choose-card "Empty hand!" hands))
    (if (history-complete? history)
        (raise-mismatch-error 'choose-card "the game's already over!" history))
    (unless (null? already-played-cards)
      (raise-mismatch-error 'choose-card "These cards have been already played, you foul cheater, you" already-played-cards))

    (when (zero? (*recursion-level*))
      (printf "~a: " us))

    (let* ((legal-choices
            (cond
             ((history-empty? history)
              ;; on the opening lead, all cards are legal.
              (ha:cards hand))

             ;; if we're leading, all cards are legal.
             ((hi:trick-complete? history)
              (ha:cards hand))
             (else
              ;; otherwise me have to follow suit if we can.
              (let ((mine-of-led-suit (filter (lambda (mine)
                                                (suits= (card-suit mine)
                                                        (suit-led history)))
                                              (ha:cards hand))))
                (if (null? mine-of-led-suit)
                    (ha:cards hand)
                  mine-of-led-suit)))))

           ;; Don't consider _every_ legal choice; instead, prune
           ;; them.  Perhaps consider only the lowest and highest;
           ;; perhaps treat sequences of cards like 2-3-4-5 as all the
           ;; same, and thus consider only one card from such
           ;; sequences.
           (legal-choices (hi-lo
                           (map car
                                ;; TODO: do better than this -- note
                                ;; that even if two cards have a gap
                                ;; between them, they're effectively
                                ;; adjacent if the cards in the gap
                                ;; have already been played.
                                (group-into-adjacent-runs
                                 legal-choices
                                 (lambda (a b)
                                   (and (eq? (card-suit a)
                                             (card-suit b))
                                        (or
                                         (= 1 (abs (- (card-rank a)
                                                      (card-rank b))))
                                         (every already-played? (cards-between a b)))))))))

           (choice
            ;; don't call predict-score if there's just one choice.
            (if (null? (cdr legal-choices))
                (car legal-choices)
              (let ((pairs (sort
                            (map (lambda (card)
                                   (cons (predict-score card max-lookahead) card))
                                 legal-choices)

                            ;; sort by score, of course; but if the
                            ;; scores are equal, choose the
                            ;; lower-ranking card.
                            (lambda (a b)
                              (if (= (car a)
                                     (car b))
                                  (< (card-rank (cdr a))
                                     (card-rank (cdr b)))
                                (> (car a)
                                   (car b)))))))
                (when (zero? (*recursion-level*))
                  (printf "from ~a ... " pairs))
                (cdar pairs)))))

      (assert (card? choice))
                                        ;(printf "playing ~a~%" choice)
      ;;(assert (memq choice (ha:hand-cards hand)))
      choice)))

)
