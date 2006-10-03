#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -qu "$0" ${1+"$@"}
|#

;; find _either_ the first 1, or else the maximum value.
(define (max-or-1 seq)
  (define (carmax a b)
    (if (< (car a) (car b)) b a))
  (if (null? seq)
      (error 'max-or-1 "wanted non-empty list; got '()")
    (let ((this (car seq)))
      (if (null? (cdr seq))
          this
        (if (= 1 (car this))
            this
          (carmax this (max-or-1 (cdr seq))))))))
(trace max-or-1)

;; compare
(max-or-1 '((0 . one) (-1 . two) (0 . three) (0 . four) (-1 . five) (0 . six) (0 . seven)))

;; with
(max-or-1 '((0 . one) (1 . two) (0 . three) (0 . four) (-1 . five) (0 . six) (0 . seven)))


;; the second finishes faster.
