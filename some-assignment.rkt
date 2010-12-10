#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec racket -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang racket
(require rackunit rackunit/text-ui)

(define (mean seq)
  (/ (apply + seq) (length seq)))

(provide mean-record)
(define (mean-record lol)
  (map mean
       (apply map (lambda something  format "Something is ~s" something)
              lol)))

(define-test-suite mean-record-tests
  (check-equal? (mean-record '((1 2 3) (4 5 6) (7 8 9)))
                '(4 5 6))
  (check-equal? (mean-record '((9 10 15 16) (7 7 7 7)))
                '(8 17/2 11 23/2)))

(define-test-suite all-tests
  mean-record-tests)

(provide main)
(define (main . args)
  (exit (run-tests all-tests 'verbose)))
