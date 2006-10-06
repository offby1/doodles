#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec mzscheme -qu "$0" ${1+"$@"}
|#

(module predict mzscheme
(require (only (lib "1.ss" "srfi")
               append-map
               every
               filter
               last-pair
               )
         "card.ss"
         (prefix ha: "hand.ss")
         "trick.ss"
         "zprintf.ss")
(provide (all-defined))

(define (assoc-backwards obj backwards-alist)
    (cond
     ((null? backwards-alist)
      #f)
     ((equal? obj (cdr (car backwards-alist)))
      (car backwards-alist))
     (else
      (assoc-backwards obj (cdr backwards-alist)))))

(define (predict-winner-of-incomplete-trick t hands)
  (let* ((card (car (last-pair (trick-cards t))))
         (right-suit? (lambda (c) (eq? (card-suit c) (led-suit t))))
         (cards-in-current-trick (length (trick-cards t))))

    ;; "ours" is the hand who _last_ played, as contrasted to
    ;; choose-card, in which case "ours" is the hand who is _about_ to
    ;; play.
    (define ours      (ha:filter right-suit? (car (rotate hands 3))))
    (define us        (ha:seat ours))
    (define lho^s     (ha:filter right-suit? (car (rotate hands 0))))
    (define partner^s (ha:filter right-suit? (car (rotate hands 1))))
    (define pard      (ha:seat partner^s))
    (define rho^s     (ha:filter right-suit? (car (rotate hands 2))))

    ;; if a given side has played, or been forced to play, a card that
    ;; is higher than their enemy's cards of that suit, then they will
    ;; win.

    ;; Keep in mind that the question we're trying to answer is: "if
    ;; we play this card, will _our team_ win this trick?"  That's
    ;; subtly different from "will this card win this trick", because
    ;; this card might be a deuce, but our partner has a singleton
    ;; ace.

    ;; a player's relevant cards are either: the single card he's
    ;; already played to this trick; or else all the cards in his hand.
    (define (relevant-cards playa)
      (let ((already (assoc-backwards
                      (ha:seat playa)
                      (annotated-cards t))))
        (if already
            (list (car already))
          (ha:cards playa))))

    (let ((theirs (append-map relevant-cards (list lho^s rho^s)))
          (pards (relevant-cards partner^s)))

      (define (beats-all-enemy-cards? c)
        (and (right-suit? card)
             (or (null? theirs)
                 (< (apply max (map card-rank theirs)) (card-rank c)))))
      (cond
       ((beats-all-enemy-cards? card)
        us)

       ;; if all partner's relevant cards beat
       ;; all the enemy cards, then we's gonna
       ;; win.
       ((and (not (null? pards))
             (every beats-all-enemy-cards? pards))
        pard)

       (else
        'depends-on-what-they-play)))))


)