#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec  mzscheme --require "$0" --main -- ${1+"$@"}
|#

(module v4-script-template scheme
(require (planet "test.ss"    ("schematics" "schemeunit.plt" ))
         (planet "text-ui.ss" ("schematics" "schemeunit.plt" ))
         (planet "util.ss"    ("schematics" "schemeunit.plt" )))
(define hmm-tests

  (test-suite
   "loop"
   (test-case
    "yow"
    (check-regexp-match
     #rx"dude, maybe you"
     "should write some tests"))))
(define (main . args)
  (exit (test/text-ui hmm-tests 'verbose)))
(provide (all-defined-out)))
