#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec  mzscheme -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang scheme
(require schemeunit schemeunit/text-ui)

(define (incubot-sentence input-sentence corpus)
  "dude, maybe you should write some tests")

(define (in-corpus? corpus sentence)
  #t)

(define (make-test-corpus)
  #f)

(define-test-suite incubot-sentence-tests
  (let ([corpus (make-test-corpus)])
    (check-true (in-corpus? corpus
                            (incubot-sentence "For Phillip Morris ... from Western Union"
                                              corpus)) )))

(define (main . args)
  (exit (run-tests incubot-sentence-tests 'verbose)))

(provide incubot-sentence main)
