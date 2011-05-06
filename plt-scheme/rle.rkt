#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec racket -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang racket
(require rackunit rackunit/text-ui)

(provide rle)
(define (rle seq)
  (let loop ([seq seq]
             [last-elt #f]
             [first-elt-of-current-run #f]
             [result '()])
    (cond
     ((null? seq) (reverse result))
     ((and last-elt (equal? (car seq) (add1 last-elt)))
      (loop (cdr seq)
            (car seq)
            first-elt-of-current-run
            result))
     (else
      (loop (cdr seq)
            (car seq)
            (car seq)
            (cons
             (if (equal? last-elt first-elt-of-current-run)
                 (car seq)
                 (list first-elt-of-current-run (car seq)))
             result)))
     )))

(define-test-suite rle-tests
  (check-equal? (rle ' (1 3 4 5 8 10 11 13)) '(1 (3 5) 8 (10 11) 13)))

(define-test-suite all-tests
  rle-tests)

(provide main)
(define (main . args)
  (exit (run-tests all-tests 'verbose)))
