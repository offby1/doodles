#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec racket -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang racket
(require rackunit rackunit/text-ui)

(provide rle)
(define (rle seq)
  (let loop ([seq seq]
             [result '()])
    (cond
     ((null? seq) (reverse result))
     ((yadda?)
      (loop (cdr seq)
            result))
     ((bowie?)
      (loop (cdr seq)
            (cons (car seq)
                  result)))
     ((snork?)
      (loop (cdr seq)
            (cons (list yow pow)
                  result))))))

(define-test-suite rle-tests
  (check-equal? (rle ' (1 3 4 5 8 10 11 13)) '(1 (3 5) 8 (10 11) 13)))

(define-test-suite all-tests
  rle-tests)

(provide main)
(define (main . args)
  (exit (run-tests all-tests 'verbose)))
