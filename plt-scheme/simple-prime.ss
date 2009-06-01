#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec  mzscheme --require "$0" --main -- ${1+"$@"}
|#

#lang scheme
(require (planet schematics/schemeunit:3)
         (planet schematics/schemeunit:3/text-ui))

(define/contract (prime? n)
  (-> (and/c (lambda (x) (< 1 x)) integer?) boolean?)
  (let/ec return
    (for ([x (in-range 2 (add1  (inexact->exact (floor (sqrt n)))))])
      (when (zero? (remainder n x))
        (return #f)))

    #t))

(define-test-suite prime-tests
  (check-exn exn:fail:contract? (lambda () (prime? 1)))
  (check-true  (prime? 2))
  (check-false (prime? 4))
  (check-false (prime? 6))
  (check-true  (prime? 101)))

(define (main . args)
  (exit (run-tests prime-tests 'verbose)))
(provide prime? main)
