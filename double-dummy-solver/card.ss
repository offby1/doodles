#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -qu "$0" ${1+"$@"}
|#

(module card mzscheme
(print-struct #t)
(require (lib "assert.ss" "offby1")
         (lib "trace.ss")
         (only (lib "1.ss" "srfi") iota)
         (only (lib "misc.ss" "swindle") memoize!))
(provide (rename my-make-card make-card)
         card?
         card-suit
         card-rank
         cards=
         card</rank
         card</suit
         *suits*
         mc/quick
         ca->string
         cards-between)

(define *suits*  '(c d h s))
(define *num-ranks* 13)
(define *num-suits* (length *suits*))   ;duh!

(define-struct card (rank suit) #f)
(define (ca->string c)
  (string-append (case (card-rank c)
                   ((10)"t")
                   ((11)"j")
                   ((12)"q")
                   ((13)"k")
                   ((14)"a")

                   (else (number->string (card-rank c)))
                   )
                 (symbol->string (card-suit c))))
(define my-make-card
  (lambda (suit rank )
    (unless (memq suit *suits*)
      (error 'make-card
             (format "first slot must be one of ~a, not ~s"
                     *suits*
                     suit)))
    (unless (and (exact? rank)
                 (integer? rank)
                 (<= 2 rank (add1 *num-ranks*)))
      (error 'make-card
             "second slot must be a number 'twixt 2 and 14, not ~s" rank))
    (make-card rank suit)))

(define (cards= a b)
  (and (eq? (card-suit a)
          (card-suit b))
       (= (card-rank a)
            (card-rank b))
       ))

(define (card->number c by-rank?)

  (define suit-number
    (- (length *suits*)
       (length (member (card-suit c) *suits*))))
  (define rank-number
    (- (card-rank c) 2))

  (if by-rank?
      (+ (* *num-suits* rank-number) suit-number)
    (+ (* *num-ranks* suit-number) rank-number)))

;; sorting by rank first, and by suits _within_ a rank, is
;; inconvenient for display, but essential for determining which to
;; play.
(define (card</rank a b)
  (< (card->number a #t)
     (card->number b #t)))

(memoize! card</rank)

(define (card</suit a b)
  (< (card->number a #f)
     (card->number b #f)))

(memoize! card</suit)

;; for testing -- allows me to type a card easily -- e.g. 'c3
(define (mc/quick card-sym)
  (let ((chars (string->list (symbol->string card-sym))))
    (let ((suit (string->symbol (string (car chars))))
          (rank (cond
                 ((char-numeric? (cadr chars))
                  (- (char->integer (cadr chars))
                     (char->integer #\0)))
                 (else
                  (case (cadr chars)
                    ((#\t) 10)
                    ((#\j) 11)
                    ((#\q) 12)
                    ((#\k) 13)
                    ((#\a) 14)
                    (else (error "Bad character: " (cadr chars)))))

                 )))
      (my-make-card suit rank))))

(define (cards-between l u)
  (assert (eq? (card-suit l)
               (card-suit u)))
  (let ((lr (card-rank l))
        (ur (card-rank u)))
    (when (< ur lr)
      (let ((tmp ur))
        (set! ur lr)
        (set! lr tmp)))
    (map (lambda (rank)
           (my-make-card (card-suit l)
                         rank)) (iota (- ur lr 1) (add1 lr)))))
)
