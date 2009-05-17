#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec  mzscheme --require "$0" --main -- ${1+"$@"}
|#

#lang scheme
(require (planet schematics/schemeunit:3)
         (planet schematics/schemeunit:3/text-ui))
(define (hmm . stuff)
  "dude, maybe you should write some tests")
(define-test-suite hmm-tests

  (let ((stuff 'bother))
    (check-equal? stuff 'bother)
    (check-equal? (hmm) "Hmm!  Maybe I should." "Wise words indeed!")))

(define (main . args)
  (exit (run-tests hmm-tests 'verbose)))
(provide hmm main)
