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
               pair-fold
               remove
               take
               take-right
               )
         (only (lib "etc.ss") compose)
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
(define non-negative? (compose not negative?))

;; (list 1 2 3 9 10 11 7 6 5) (lambda (a b) (= a (add1 b))) => ((1 2
;; 3) (9 10 11) (7) (6) (5))
(define (group-into-adjacent-runs seq adjacent?)
  (reverse
   (map reverse
        (fold (lambda (item seq)
                (if (null? seq)
                    (list (list item))
                  (if (adjacent? item (caar seq))
                      (cons (cons item (car seq))
                            (cdr seq))
                    (cons (list item)
                          seq))))
              '()
              seq))))

;; Returns the least, and the two greatest, cards, based on rank.
(define (bot-one/top-two seq)
  (if (< (length seq) 4)
      seq
    (let ((sorted (sort seq card</rank)))
      (cons (car sorted)
            (take-right sorted (min 2 (length sorted)))))))

(define (top-two seq)
  (if (< (length seq) 3)
      seq
    (let ((sorted (sort seq card</rank)))
      (take-right sorted (min 2 (length sorted))))))

;; nondestructive.  Take CARD from (car HANDS), and move it into the
;; history.  Return that new history, and the new set of hands, with
;; the card gone, and with the hands rotated one notch.
(define (play-card history hands c)
  (let ((h (car hands)))
    (assert (member c (ha:cards h)))
    (let* ((new-hand    (ha:remove-card h c))
           (new-history (add-card history c))
           (new-hand-list (cons new-hand (cdr hands)))
           (rotated (if (and (not (history-complete? new-history))
                             (hi:trick-complete? new-history))
                        (rotate-until new-hand-list (lambda (h)
                                                      (eq? (ha:seat (car h))
                                                           (history:whose-turn new-history))))
                      (rotate new-hand-list 1))))
      (values new-history rotated))))
;;(trace play-card)

;; plays at most NUM-TRICKS from the given hands, starting with (car HANDS).
(define (play-loop history hands num-tricks max-lookahead termination-history-proc)
  (define (inner history hands num-tricks max-lookahead counter)
    (let ((trick-number (add1 (quotient counter 4)))
          (ha (car hands)))
      (if (or (zero? num-tricks)
              (ha:empty? ha))
          (termination-history-proc history hands)
        (begin
          (when (or (history-empty? history)
                    (hi:trick-complete? history))
            (zprintf "~%Trick ~a:~%" trick-number))
          (zprintf "~a thinks ..." (ha:seat ha))
          (let-values (((new-hi new-hands)
                        (play-card
                         history
                         hands
                         (parameterize ((*really-loud* (and #f
                                                            (or (*really-loud*)
                                                                (= trick-number 1)))) )
                           (zp " plays ~a~%"
                               (choose-card history hands max-lookahead))))))
            (inner new-hi
                   new-hands
                   (if (hi:trick-complete? new-hi)
                       (sub1 num-tricks)
                     num-tricks)
                   max-lookahead
                   (add1 counter)))))))
   (check-type 'play-loop non-negative? num-tricks)
   (check-type 'play-loop non-negative? max-lookahead)
   (inner history hands num-tricks max-lookahead 0))
;;(trace play-loop)
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

  ;; computes the score from the point of view of whoever played last.
  ;; If the last trick is incomplete, it guesses (which is why it
  ;; needs to a set of hands).
  (define (score-from-history history hands)
    (zprintf " score-from-history: we are ~a; ~a ..." (ha:seat us) history)
    (zp " returning ~a ..."
        (fold
         (lambda (t sum)

           (define (we-won? t)
             (let ((w (winner t)))
               (or (eq? w (ha:seat us))
                   (eq? w (ha:seat partner)))))

           (+ sum (cond
                   ((not (trick-complete? t))
                    (let* ((card (car (last-pair (trick-cards t))))
                           (right-suit? (lambda (c) (eq? (card-suit c) (led-suit t))))
                           (cards-in-current-trick (length (trick-cards t))))

                      ;; "us" is the set who _last_ played, as contrasted to
                      ;; choose-card, in which case "us" is the seat who is _about_ to
                      ;; play.
                      (define us      (ha:filter right-suit? (car (rotate hands 3))))
                      (define lho     (ha:filter right-suit? (car (rotate hands 0))))
                      (define partner (ha:filter right-suit? (car (rotate hands 1))))
                      (define rho     (ha:filter right-suit? (car (rotate hands 2))))

                      ;; if a given side has played, or been forced
                      ;; to play, a card that is higher than their
                      ;; enemy's cards of that suit, then they will
                      ;; win.

                      ;; Keep in mind that the question we're trying
                      ;; to answer is: "if we play this card, will we
                      ;; win this trick?"  That's subtly different
                      ;; from "will this card win this trick", because
                      ;; this card might be a deuce, but our partner
                      ;; has a singleton ace.
                      (define (relevant-cards playa)
                        (let ((already (assoc-backwards
                                         (ha:seat playa)
                                         (annotated-cards t))))
                           (if already
                               (filter right-suit? (list (car already)))
                             (ha:cards playa))))

                      (if (right-suit? card)
                          (let ((theirs (append-map relevant-cards (list lho rho)))
                                (pards (relevant-cards partner)))
                            (define (beats-all-enemy-cards? c)
                              (or (null? theirs)
                                  (< (apply max (map card-rank theirs)) (card-rank c))))
                            (cond
                             ((beats-all-enemy-cards? card)
                              (zp " my ~a beats enemy's ~a:~a"
                                  card
                                  (append-map relevant-cards (list lho rho))
                                  1))

                             ;; if all partner's relevant cards beat
                             ;; all the enemy cards, then we's gonna
                             ;; win.
                             ((and (not (null? pards))
                                   (every beats-all-enemy-cards? pards))
                              (zp " each of partner's ~a beats enemy's ~a:~a"
                                  pards theirs 1))
                             (else
                              (zp " ~a ain't the boss:~a" card 0))))
                        ;; Wrong suit?  We'll surely lose.  (This will
                        ;; of course need to be updated once I introduce
                        ;; trumps.)
                        (zp " ~a is wrong suit:~a" card -1))))
                   ((we-won? t) (zp " complete trick; we won:~a" 1))
                   (else (zp " complete trick; we lost:~a" -1)))))
         0
         (history-tricks history))))

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
    (check-type 'predict-score non-negative? max-lookahead)
    (parameterize ((*recursion-level* (add1 (*recursion-level*))))
      (let-values (((new-hi new-hands)
                    (play-card history hands card)))
        (play-loop
         new-hi
         new-hands
         max-lookahead
         (max (sub1 max-lookahead) 0)
         score-from-history))))

  (define already-played-cards
    (history-card-set history))

  (define (held-by-enemy? c)
    (or (member c (ha:cards lho))
        (member c (ha:cards rho))))

  (define (assoc-backwards obj backwards-alist)
    (cond
     ((null? backwards-alist)
      #f)
     ((equal? obj (cdr (car backwards-alist)))
      (car backwards-alist))
     (else
      (assoc-backwards obj (cdr backwards-alist)))))

  ;;(trace predict-score)
  ;;(trace score-from-history)
  (check-type 'choose-card non-negative? max-lookahead)
  (check-type 'choose-card history? history)
  (unless (ha:hand? us)
    (raise-mismatch-error 'choose-card "Not a list of hands: " hands))
  (if (ha:empty? us)
      (raise-mismatch-error 'choose-card "Empty hand!" hands))
  (if (history-complete? history)
      (raise-mismatch-error 'choose-card "the game's already over!" history))
  (unless (null? (lset-intersection eq? (ha:cards us) already-played-cards))
    (raise-mismatch-error 'choose-card "These cards have been already played, you foul cheater, you" already-played-cards))

  (let* ((legal-choices
          (cond
           ;; If we're leading, all cards are legal.
           ((or (history-empty? history)
                (hi:trick-complete? history))

            ;; we sort by suits so that it will be easy to make groups
            ;; of adjacent cards of the same suit.
            (sort (ha:cards us) card</suit)
            )
           (else
            ;; otherwise me have to follow suit if we can.
            (let ((mine-of-led-suit (filter (lambda (mine)
                                              (eq? (card-suit mine)
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

          ;; here we're counting on the cards in the hand having been
          ;; sorted with card</rank previously.  Presumably if we were
          ;; to do that sorting ourselves here, we'd waste time.

          (group-into-adjacent-runs
           legal-choices
           (lambda (a b)
             (and (eq? (card-suit a)
                       (card-suit b))
                  (every (lambda (c) (member c (ha:cards us)))
                         (cards-between a b))))))

         ;; diagnostic only--vvvvv
         (runs (filter (lambda (l)
                         (< 1 (length l)))
                       grouped))
         (has-a-gap (lambda (seq)
                      (pair-fold
                       (lambda (pair accumulator)
                         (or accumulator
                             (and (not (null? (cdr pair)))
                                  (< 1 (- (card-rank (cadr pair))
                                          (card-rank (car pair)))))))
                       #f seq)))
         (gappy-runs (filter has-a-gap runs))
         (dummy (when (not (null? gappy-runs))
                  (printf " gappy runs: ~a" gappy-runs)))
         ;; diagnostic only--^^^^^

         (cards-in-current-trick (or (and (not (history-empty? history))
                                          (length (trick-cards (history-latest-trick history))))
                                     0))

         ;;(pruned-legal-choices (top-two (map car grouped)))

         ;; TODO -- if we can't follow suit, perhaps we should only
         ;; consider low cards.

         ;; And when we're leading, we should probably consider at
         ;; least one card from each suit.
         (pruned-legal-choices (bot-one/top-two (map car grouped)))

         (choice
          (if (null? (cdr pruned-legal-choices))
              (begin
                (if (null? (cdr legal-choices))
                    (zprintf " (duh, singleton)")
                  (when (null? (cdr grouped))
                    (zprintf " ~a all the same" (car grouped))))

                (car pruned-legal-choices))

            (let* ((max-lookahead
                    (zp " looks ahead ~a"
                        (min max-lookahead
                             (case  cards-in-current-trick
                               ;; if we're the last to play to
                               ;; this trick, let's not strain our
                               ;; brains -- we'll simply try to
                               ;; win the trick if we can.
                               ((1 2) 0)
                               ((3) 1)
                               (else max-lookahead)))))
                   (pairs (sort
                           (map (lambda (card)
                                  (cons (predict-score card max-lookahead)
                                        card))
                                (zp " considers ~a ..." pruned-legal-choices))

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

              (cdar (zp " -> ~a" pairs))))))



    choice))
;;(trace choose-card)

)
