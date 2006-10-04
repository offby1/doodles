#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec mzscheme -qu "$0" ${1+"$@"}
|#

(module predict mzscheme

(provide (all-defined))
(define (predict-winner-of-incomplete-trick t hands)
  'depends-on-what-they-play)
;; (let* ((card (car (last-pair (trick-cards t))))
;;        (right-suit? (lambda (c) (eq? (card-suit c) (led-suit t))))
;;        (cards-in-current-trick (length (trick-cards t))))

;;   ;; "us" is the seat who _last_ played, as
;;   ;; contrasted to choose-card, in which case
;;   ;; "us" is the seat who is _about_ to play.
;;   (define us      (ha:filter right-suit? (car (rotate hands 3))))
;;   (define lho     (ha:filter right-suit? (car (rotate hands 0))))
;;   (define partner (ha:filter right-suit? (car (rotate hands 1))))
;;   (define rho     (ha:filter right-suit? (car (rotate hands 2))))

;;   ;; if a given side has played, or been forced
;;   ;; to play, a card that is higher than their
;;   ;; enemy's cards of that suit, then they will
;;   ;; win.

;;   ;; Keep in mind that the question we're trying
;;   ;; to answer is: "if we play this card, will we
;;   ;; win this trick?"  That's subtly different
;;   ;; from "will this card win this trick", because
;;   ;; this card might be a deuce, but our partner
;;   ;; has a singleton ace.
;;   (define (relevant-cards playa)
;;     (let ((already (assoc-backwards
;;                     (ha:seat playa)
;;                     (annotated-cards t))))
;;       (if already
;;           (filter right-suit? (list (car already)))
;;         (ha:cards playa))))

;;   (if (right-suit? card)
;;       (let ((theirs (append-map relevant-cards (list lho rho)))
;;             (pards (relevant-cards partner)))
;;         (define (beats-all-enemy-cards? c)
;;           (or (null? theirs)
;;               (< (apply max (map card-rank theirs)) (card-rank c))))
;;         (cond
;;          ((beats-all-enemy-cards? card)
;;           (zp "~a's ~a beats enemy's ~a:~a"
;;               (ha:seat us)
;;               card
;;               (append-map relevant-cards (list lho rho))
;;               1))

;;          ;; if all partner's relevant cards beat
;;          ;; all the enemy cards, then we's gonna
;;          ;; win.
;;          ((and (not (null? pards))
;;                (every beats-all-enemy-cards? pards))
;;           (zp "each of ~a's partner's ~a beats enemy's ~a:~a"
;;               (ha:seat us) pards theirs 1))
;;          (else
;;           (zp "~a ain't the boss:~a" card 0))))
;;     ;; Wrong suit?  We'll surely lose.  (This will
;;     ;; of course need to be updated once I introduce
;;     ;; trumps.)
;;     (zp "~a is wrong suit:~a" card -1)))

)