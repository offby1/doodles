#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec racket -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang racket
(require rackunit rackunit/text-ui)

(provide flatten)
(define (flatten-inner thing)
  (let loop ([accumulator '()]
             [thing thing])
    (cond
     ((null? thing)
      accumulator)
     ((pair? (car thing))
      (loop (append (flatten-inner (car thing)) accumulator) (cdr thing)))
     (else
      (loop (cons (car thing) accumulator)
            (cdr thing)))
     ))
  )

(define (flatten thing)
  (reverse (flatten-inner thing)))

(define-test-suite flatten-tests
  (check-equal?
   (flatten '( #\s (#\r #\a #\b) #\space #\o #\o #\f))
   '( #\s #\r #\a #\b #\space #\o #\o #\f)))

(define-test-suite all-tests
  flatten-tests)

(provide main)
(define (main . args)
  (exit (run-tests all-tests 'verbose)))
