#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec  mzscheme -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang scheme
(require schemeunit schemeunit/text-ui)
(define (hmm . stuff)
  "dude, maybe you should write some tests")
(define-test-suite hmm-tests

  (let ((stuff 'bother))
    (check-equal? stuff 'bother)
    (check-equal? 'actual 'expected "For Phillip Morris ... from Western Union")))

(define (main . args)
  (exit (run-tests hmm-tests 'verbose)))
(provide hmm main)
