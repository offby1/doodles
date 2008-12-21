#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id: v4-script-template.ss 5748 2008-11-17 01:57:34Z erich $
exec  mzscheme -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang scheme
(require (planet schematics/schemeunit:3)
         (planet schematics/schemeunit:3/text-ui))

(define (p thing)
  (printf "~s~%" thing)
  thing)

(define (treemap proc thing)
  (cond
   ((pair? thing)
    (append (treemap proc (car thing))
            (treemap proc (cdr thing))))
   ((null? thing)
    '())
   (else
    (list (proc thing)))))

(define treemap-tests

  (test-suite
   "loop"
   (check-equal? (treemap values '()) '())
   (check-equal? (treemap values '(a (b) c)) '(a b c))
   (check-equal? (treemap values '(a (b) ((c)) d)) '(a b c d))

   (check-equal? (treemap add1   '(1 (2) 3 (((4)))) ) '(2 3 4 5))))

(define (main . args)
  (exit (run-tests treemap-tests 'verbose)))
(provide (all-defined-out))
