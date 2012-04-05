#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec racket -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang racket
(require rackunit rackunit/text-ui)

(provide (rename-out [my-flatten flatten]))
(define (flatten-backwards thing)
  (let loop ([accumulator '()]
             [thing thing])
    (cond
     ((null? thing)
      accumulator)
     ((empty? (car thing))
      (loop accumulator (cdr thing)))
     ((pair? (car thing))
      (loop (append (flatten-backwards (car thing)) accumulator) (cdr thing)))
     (else
      (loop (cons (car thing) accumulator)
            (cdr thing))))))

(define (my-flatten thing)
  (reverse (flatten-backwards thing)))

(define (test-data [how-many 10])
  (for/list ([i (in-range how-many)])
    (let ([r (random)])
      (cond
        ((< r .1) '())
        ((< r .2) (test-data (/ how-many 2)))
        (else #\f)))))

(define-test-suite flatten-tests
  (check-equal?
   (my-flatten '( #\s (#\r #\a #\b) #\space #\o #\o #\f))
   '( #\s #\r #\a #\b #\space #\o #\o #\f))
  (for ([trial (in-range 100)])
    (let ([td (test-data 10)])
      (check-equal? (my-flatten td)
                    (flatten td)))))

(define-test-suite all-tests
  flatten-tests)

(provide main)
(define (main . args)
  (exit (run-tests all-tests 'verbose)))
