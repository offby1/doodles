#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec  mzscheme --require "$0" --main -- ${1+"$@"}
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
               )
         (only (lib "etc.ss") compose)
         (lib "pretty.ss")
         (only rnrs/base-6 assert)
         "card.ss"
         "trick.ss"
         "zprintf.ss"
         "predict.ss"
         (all-except "history.ss" whose-turn)
         (rename "history.ss" history:whose-turn whose-turn)
         (prefix ha: "hand.ss")
         (only (lib "list.ss") sort)
         (lib "trace.ss")
         (planet schematics/schemeunit:3)
         (planet schematics/schemeunit:3/text-ui))
(provide
 choose-card
 play-loop
 predict-score
 )
(display "$Id$" (current-error-port))
(newline (current-error-port))

(print-struct #t)
(define non-negative? (compose not negative?))


(define (group-into-adjacent-runs seq adjacent?)
  (reverse
   (map reverse
        (fold (lambda (item seq)
                (if (or (null? seq)
                        (not (adjacent? item (caar seq))))
                    (cons (list item) seq)
                  (cons (cons item (car seq))
                        (cdr seq))))
              '()
              seq))))

(check-equal?  (group-into-adjacent-runs
                (list 1 2 3 9 10 11 7 6 5)
                (lambda (a b) (= a (add1 b))))
                '((1 2 3) (9 10 11) (7) (6) (5)))

(define (hi-lo-each-suit seq t)

  (define (w< a b) (< (augmented-rank a t)
                      (augmented-rank b t)))

  (define (hi-lo seq)
    (if (< (length seq) 3)
        seq
      (let ((sorted (sort seq w<)))
        (cons (car sorted) (last-pair sorted)))))

  (define (partition-by-suits cs)
    (list (filter spade? cs)
          (filter heart? cs)
          (filter diamond? cs)
          (filter club? cs)))

  (append-map hi-lo (partition-by-suits seq)))

(define (ass0 obj alist)
  (cond ((assq obj alist)=> cdr) (else 0)))

;; each is (cons seat integer?)
(define (sum-scores a b)
  (for-each (lambda (s)
              (assert (list? s))
              (assert (every (lambda (p)
                               (or (eq? (car p) 'depends-on-what-they-play)
                                   (member (car p) *seats*))) s)))
            (list a b))

  (map (lambda (seat)
         (cons seat
               (+ (ass0 seat a)
                  (ass0 seat b))))
       *seats*))

(define (numeric-team-score sp our-seat)
  (assert (list? sp))
  (+ (ass0 our-seat sp)
     (ass0 (partner our-seat) sp)))

;; nondestructive.  Take CARD from (car HANDS), and move it into the
;; history.  Return that new history, and the new set of hands, with
;; the card gone, and with the hands rotated so that the next player
;; to play is in front.
(define (play-card history hands c)
  (let ((h (car hands)))
    (unless (member c (ha:cards h))
      (error 'play-card  "~a is not from the first of ~a" c  hands ))
    (let* ((new-history (add-card history c))
           (hands (cons (ha:remove-card h c) (cdr hands)))
           (rotated (if (and (not (history-complete? new-history))
                             (hi:trick-complete? new-history))
                        (rotate-until hands (lambda (h)
                                                      (eq? (ha:seat (car h))
                                                           (history:whose-turn new-history))))
                      (rotate hands 1))))

      ;; just for fun, see if the winner took a finesse.
      (when (hi:trick-complete? new-history)
        (letrec ((hand-of (lambda (seat hands)
                            (if (eq? seat (ha:seat (car hands)))
                                (car hands)
                              (hand-of seat (cdr hands))))))
          (let* ((t (history-latest-trick new-history))
                 (leader (leader t))
                 (winning-seat (winner t))
                 (offset
                  (with-seat-circle
                   winning-seat
                   (lambda (sc)  (length (member leader sc)))))
                 (winning-card (car (assoc-backwards winning-seat (annotated-cards t))))
                 (lho (with-seat-circle winning-seat cadr))
                 (rho (with-seat-circle lho caddr))

                 (enemy-holding
                  (append (ha:cards (hand-of lho hands))
                          (ha:cards (hand-of rho hands))))
                 (suckers (filter (lambda (en)
                                    (and (eq? (card-suit en)
                                              (card-suit (trick-ref t 0)))
                                         (< (augmented-rank winning-card t)
                                            (augmented-rank en t))))
                                  enemy-holding)))
            (case offset
              ((2 3)
               (when (not (null? suckers))
                 (when (zero? (*recursion-level*))
                   (printf "Ha! ~a just finessed the ~a: ~a~%" winning-seat suckers t))
                 ))))))

      (values new-history rotated))))
;;(trace play-card)

;; plays tricks from the given hands, starting with (car HANDS),
;; stopping when TERMINATION-PROC returns a true value, or the hands
;; run out.  Calls SUMMARIZE-PROC on the then-current history and
;; hands.

;; This function is starting to look like "unfold".  Perhaps someday I
;; should explicitly write it to call "unfold".
(define (play-loop history
                   hands
                   choose-card
                   max-lookahead
                   termination-proc
                   summarize-proc
                   )
  (define (inner history hands max-lookahead counter)
    (let ((trick-number (add1 (quotient counter (length hands))))
          (ha (car hands))
          (rv (termination-proc history hands)))

      (if (or rv (ha:empty? ha))
          (summarize-proc history hands)
        (begin
          (when (or (history-empty? history)
                    (hi:trick-complete? history))
            (zprintf "~%Trick ~a:~%" trick-number))
          (zprintf "~a thinks ..." (ha:seat ha))
          (let-values (((new-hi new-hands)
                        (play-card
                         history
                         hands
                         (zp "plays ~a~%"
                             (choose-card history hands max-lookahead #f)))))
            (inner new-hi
                   new-hands
                   max-lookahead
                   (add1 counter)))))))

  (assert (non-negative? max-lookahead))
  (inner history (rotate-until hands (lambda (hands)
                                       (eq? (ha:seat (car hands))
                                            (history:whose-turn history))))
         max-lookahead 0))
;;(trace play-loop)

(define (assert-alist-or-false value)
  (unless (or (not value)
              (and (list? value)))
    (raise-type-error 'choose-card (format "alist or #f") value))
  value)
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

(define (choose-card history hands max-lookahead quick-and-dirty?)

  (define ours      (car (rotate hands 0)))
  (define us        (ha:seat ours))
  (define lho^s     (car (rotate hands 1)))
  (define partner^s (car (rotate hands 2)))
  (define rho^s     (car (rotate hands 3)))


  (define already-played-cards
    (history-card-set history))

  (define (held-by-enemy? c)
    (or (member c (ha:cards lho^s))
        (member c (ha:cards rho^s))))

  ;;(trace predict-score)
  ;;(trace numeric-team-score)
  (assert (non-negative? max-lookahead))
  (assert (history? history))
  (unless (ha:hand? ours)
    (raise-mismatch-error 'choose-card "Not a list of hands: " hands))
  (if (ha:empty? ours)
      (raise-mismatch-error 'choose-card "Empty hand!" hands))
  (if (history-complete? history)
      (raise-mismatch-error 'choose-card "the game's already over!" history))
  (unless (null? (lset-intersection eq? (ha:cards ours) already-played-cards))
    (raise-mismatch-error 'choose-card "These cards have been already played, you foul cheater, you" already-played-cards))

  (let* ((t (and (not (history-empty? history))
                 (not (hi:trick-complete? history))
                 (history-latest-trick history)))
         (legal-choices
          (cond
           ;; If we're leading, all cards are legal.
           ((not t)
            ;; we sort by suits so that it will be easy to make groups
            ;; of adjacent cards of the same suit.
            (sort (ha:cards ours) card</suit)
            )
           (else
            ;; otherwise me have to follow suit if we can.
            (let ((mine-of-led-suit (filter (lambda (mine)
                                              (eq? (card-suit mine)
                                                   (suit-led history)))
                                            (ha:cards ours))))
              (if (null? mine-of-led-suit)
                  (ha:cards ours)
                mine-of-led-suit)))))

         ;; Don't consider _every_ legal choice; instead, treat
         ;; sequences of cards like 2-3-4-5 as all the same, and thus
         ;; consider only one card from such sequences.
         (grouped

          ;; here we're counting on the cards in the hand having been
          ;; sorted with card</rank previously.  Presumably if we were
          ;; to do that sorting ourselves here, we'd waste time.

          (group-into-adjacent-runs
           legal-choices
           (lambda (a b)
             (and (eq? (card-suit a)
                       (card-suit b))
                  (every (lambda (c) (member c (ha:cards ours)))
                         (cards-between a b))))))

         (cards-in-current-trick (or (and t
                                          (length (trick-cards t)))
                                     0))

         ;; TODO -- if we can't follow suit, perhaps we should only
         ;; consider low cards.

         ;; And when we're leading, we should probably consider at
         ;; least one card from each suit.

         (pruned-legal-choices
          (if (zero? max-lookahead)
              ;; Shit, Jackson: as long as we're only looking ahead
              ;; zero, why not consider every group?  Shouldn't slow
              ;; us down _too_ much.  I hope.
              (map car grouped)
            (hi-lo-each-suit (map car grouped) t)))

         (choice
          (if (or quick-and-dirty?
                 (null? (cdr pruned-legal-choices)))
              (begin
                (if (null? (cdr legal-choices))
                    (zprintf "(duh, singleton)")
                  (when (null? (cdr grouped))
                    (zprintf "~a all the same" (car grouped))))

                (car pruned-legal-choices))

            (begin
              (zprintf "~a looks ahead ~a" us max-lookahead)

              ;; Don't call predict-score on _every_ card; rather,
              ;; stop as soon as we find a card whose score is 1.

              (let/ec return
                (let ((best #f))

                  ;; if our partner has already played, sort 'em so
                  ;; that we look at the smaller cards first -- that
                  ;; way we should avoid playing a high card on our
                  ;; partner's winner.
                  (zp "considers ~a ..." pruned-legal-choices)

                  (for-each
                   (lambda (choice)
                     ;; sort by score, of course; but if the scores
                     ;; are equal, choose the lower-ranking card.
                     (define (better a b)
                       (if (= (car a)
                              (car b))
                           (< (augmented-rank (cdr a))
                              (augmented-rank (cdr b)))
                         (> (car a)
                            (car b))))
                     (let* ((score (numeric-team-score
                                    (predict-score choice history hands max-lookahead)
                                    us))
                            (score-delta
                             (-
                              score
                              (numeric-team-score
                               (compute-score
                                (history-complete-tricks-only
                                 history))
                               us))))

                       (when (= score-delta (add1 max-lookahead))
                         (return
                          (zp "This card's good enough: ~a~%" choice)))
                       (when (or (not best)
                                 (better (cons score choice) best))
                         (set! best (cons score choice)))))

                   (case cards-in-current-trick
                     ((2 3)
                      (sort
                       pruned-legal-choices
                       (lambda (a b)
                         (let ((wa (augmented-rank a t))
                               (wb (augmented-rank b t)))
                           (if (= wa wb)
                               (< (card-rank a)
                                  (card-rank b))
                             (< wa wb))))))
                     (else
                      pruned-legal-choices)
                     ))
                  (cdr best)))))))

    choice))

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

(define (predict-score card history hands max-lookahead)

  ;; computes the score from the given history.  If the last trick is
  ;; incomplete, it guesses (which is why it needs to a set of hands).
  ;; returns an alist like: ((n . 3) (e . 2) (s . 0) (w . 1))
  (define (score-from-history history hands)
    (fold
     (lambda (t accum)
       (sum-scores
        accum
        (cond
         ((not (trick-complete? t))
          (list (cons (predict-winner-of-incomplete-trick t hands) 1)))
         (else
          (list (cons (winner t) 1))))))
     '()
     (history-tricks history)))
  ;;(trace score-from-history)

  (assert (non-negative? max-lookahead))
  (let ((rl (*recursion-level*)))
    (parameterize ((*recursion-level* (add1 (*recursion-level*))))
      (let-values (((new-hi new-hands)
                    (play-card history hands card)))

        (play-loop
         new-hi
         new-hands
         choose-card
         (max (sub1 max-lookahead) 0)

         ;; hypo-thetical, that is.
         (lambda (hypo-history hypo-hands)
           (= (* (length *seats*)
                 max-lookahead)
              ( - (history-length-cards hypo-history)
                  (history-length-cards new-hi))))

         (lambda (hypo-history hypo-hands)
           (parameterize ((*recursion-level* rl))
             (score-from-history hypo-history hypo-hands))))))
    ))
;;(trace predict-score)
;;(trace choose-card)

)
