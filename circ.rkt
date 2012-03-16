#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec racket -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang racket
(require rackunit rackunit/text-ui)

(provide is-circular?)
(define (is-circular? seq)
  #f)

(define-test-suite is-circular?-tests
  (check-false (is-circular? '())))

(define-test-suite all-tests
  is-circular?-tests)

(provide main)
(define (main . args)
  (exit (run-tests all-tests 'verbose)))
