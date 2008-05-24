#! /usr/bin/env mzscheme

#lang scheme

;$Id$

(require (lib "trace.ss")
         (planet "test.ss"    ("schematics" "schemeunit.plt" 2))
         (planet "util.ss"    ("schematics" "schemeunit.plt" 2))
         (planet "text-ui.ss" ("schematics" "schemeunit.plt" 2)))

(define hmm-tests

  (test-suite
   "hmm"
   (test-case
    "yow"
    (check-false
     "dude, maybe you should write some tests"))))

(define (greets)
  (display (banner))
  (printf "Finally!~%"))
(greets)
(exit (test/text-ui hmm-tests 'verbose))


(provide (all-defined-out))
