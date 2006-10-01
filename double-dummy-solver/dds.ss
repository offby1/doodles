#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module dds mzscheme
(require (only (lib "1.ss" "srfi")
               append-map
               circular-list
               every
               filter
               fold
               last-pair
               lset-intersection
               remove
               take
               take-right
               )
         (lib "pretty.ss")
         (lib "assert.ss" "offby1")
         "card.ss"
         "trick.ss"
         "zprintf.ss"
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
(define (bot-one/top-two seq)
  (if (< (length seq) 4)
      seq
    (cons (car seq)
          (take-right seq (min 2 (length seq))))))

;; nondestructive.  Take CARD from (car HANDS), and move it into the
;; history.  Return that new history, and the new set of hands, with
;; the card gone, and with the hands rotated one notch.
(define (play-card history hands c)
  (let ((h (car hands)))
    (assert (member c (ha:cards h)))
    (zprintf "~a plays ~a~%" (ha:seat (car hands)) (ca->string c))
    (let* ((new-hand (ha:remove-card h c))
           (new-history (add-card history c))
           (new-hand-list (cons new-hand (cdr hands)))
           (rotated (if (and (not (history-complete? new-history))
                             (hi:trick-complete? new-history))
                        (rotate-until new-hand-list (lambda (h)
                                                      (eq? (ha:seat (car h))
                                                           (history:whose-turn new-history))))
                      (rotate new-hand-list 1))))
      (when (hi:trick-complete? new-history)
        (zprintf "~%"))
      (values new-history rotated))))
;;(trace play-card)

;; plays at most NUM-TRICKS from the given hands, starting with (car HANDS).
(define (play-loop history hands num-tricks max-lookahead termination-history-proc)
  (let ((ha (car hands)))
    (if (or (zero? num-tricks)
            (ha:empty? ha))
        (termination-history-proc history)
      (let-values (((new-hi new-hands)
                    (play-card history hands (choose-card history hands max-lookahead))))

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

  (define us      (car (rotate hands 0)))
  (define lho     (car (rotate hands 1)))
  (define partner (car (rotate hands 2)))
  (define rho     (car (rotate hands 3)))

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
    (define (we-won? t) (member (winner t)
                                (map ha:seat (list  us partner))))

    (define (our-trick-score history)
      (fold
       (lambda (t sum)
         (+ sum (cond
                 ((not (trick-complete? t)) 0)
                 ((we-won? t) 1)
                 (else -1))))
       0
       (history-tricks history)))

    (parameterize ((*recursion-level* (add1 (*recursion-level*))))
      (let-values (((new-hi new-hands)
                    (play-card history hands card)))
        (play-loop
         new-hi
         new-hands
         max-lookahead
         (sub1 max-lookahead)
         our-trick-score))))
  ;;  (trace predict-score)

  (define already-played-cards
    (history-card-set history))
  (define (already-played? c)
    (member c already-played-cards))

  (define (held-by-enemy? c)
    (or (member c (ha:cards lho))
        (member c (ha:cards rho))))

  (check-type 'choose-card history? history)
  (unless (ha:hand? us)
    (raise-mismatch-error 'choose-card "Not a list of hands: " hands))
  (if (ha:empty? us)
      (raise-mismatch-error 'choose-card "Empty hand!" hands))
  (if (history-complete? history)
      (raise-mismatch-error 'choose-card "the game's already over!" history))
  (unless (null? (lset-intersection eq? (ha:cards us) already-played-cards))
    (raise-mismatch-error 'choose-card "These cards have been already played, you foul cheater, you" already-played-cards))

  (zprintf "~a: " (ha:seat us))

  (let* ((legal-choices
          (cond
           ((history-empty? history)
            ;; on the opening lead, all cards are legal.
            (ha:cards us))

           ;; if we're leading, all cards are legal.
           ((hi:trick-complete? history)
            (ha:cards us))
           (else
            ;; otherwise me have to follow suit if we can.
            (let ((mine-of-led-suit (filter (lambda (mine)
                                              (suits= (card-suit mine)
                                                      (suit-led history)))
                                            (ha:cards us))))
              (if (null? mine-of-led-suit)
                  (ha:cards us)
                mine-of-led-suit)))))

         ;; Don't consider _every_ legal choice; instead, prune
         ;; them.  Perhaps consider only the lowest and highest;
         ;; perhaps treat sequences of cards like 2-3-4-5 as all the
         ;; same, and thus consider only one card from such
         ;; sequences.
         (grouped
          (group-into-adjacent-runs
           legal-choices
           (lambda (a b)
             (and (eq? (card-suit a)
                       (card-suit b))
                  (or
                   (= 1 (abs (- (card-rank a)
                                (card-rank b))))
                   (every (lambda (c) (not (held-by-enemy? c))) (cards-between a b)))))))
         (pruned-legal-choices (bot-one/top-two (map car grouped)))
         (choice
          ;; don't call predict-score if there's just one choice.
          (if (null? (cdr pruned-legal-choices))
              (car pruned-legal-choices)

            ;; TODO -- use "fold" or "reduce".  Won't save any time,
            ;; but might be a tad neater.
            (let ((pairs (sort
                          (map (lambda (card)
                                 (cons (predict-score card max-lookahead) card))
                               pruned-legal-choices)

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

              (zprintf "~a -> ~a -> ~a -> ~a ... "
                       (map ca->string legal-choices)
                       (map (lambda (seq) (map ca->string seq)) grouped)
                       (map ca->string pruned-legal-choices)
                       (map (lambda (p )
                              (cons (ca->string (cdr p))
                                    (car p))) pairs))

              (cdar pairs)))))

    choice))
;;(trace choose-card)

)
