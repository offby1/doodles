#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec racket -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang racket
(require rackunit rackunit/text-ui)

(provide hmm)
(define (hmm . stuff)
  "dude, maybe you should write some tests")

(define-test-suite hmm-tests
  (check-equal? 'actual 'expected "For Phillip Morris ... from Western Union"))

(define (main . args)
  (exit (run-tests hmm-tests 'verbose)))

(provide main)
