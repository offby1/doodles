#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec  mzscheme --require "$0" --main -- ${1+"$@"}
|#

#lang scheme
(require (planet schematics/schemeunit:3)
         (planet schematics/schemeunit:3/text-ui))
(define hmm-tests

  (test-suite
   "loop"
   (test-case
    "yow"
    (check-regexp-match
     #rx"dude, maybe you"
     "should write some tests"))))
(define (main . args)
  (exit (run-tests hmm-tests 'verbose)))
(provide (all-defined-out))
