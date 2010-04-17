#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec  mzscheme -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang scheme
(require schemeunit schemeunit/text-ui
         mzlib/trace)

(define (incubot-sentence input-sentence corpus)
  (car corpus))

(define (in-corpus? sentence corpus)
  (member sentence corpus))

(define (make-test-corpus)
  (list "Some thing"
        "Some thing else"))

(define-test-suite incubot-sentence-tests
  (let ([corpus (make-test-corpus)])
    (let* ([input-1 "For Phillip Morris ... from Western Union"]
           [output-1 (incubot-sentence input-1 corpus)]
           [input-2 "I have no words in common with input-1"]
           [output-2 (incubot-sentence input-2 corpus)])
    (check-not-false (in-corpus? output-1 corpus) )
    (check-not-false (in-corpus? output-2 corpus))

    ;; Since the two input sentences have nothing in common, we should
    ;; have come up with different outputs for each.
    (check-not-equal? output-1 output-2))))

(define (main . args)
  (exit (run-tests incubot-sentence-tests 'verbose)))

(provide incubot-sentence main)
