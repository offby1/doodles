#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec racket --require "$0" --main -- ${1+"$@"}
|#

#lang racket

(define (square z)
  (* z z))

(define (sum-of-squares-of-two-largest a b c)

  (define smallest
    (cond
     ((<= a b c)
      a)
     ((<= b a c)
      b)
     (else
      c)))

  (- (+ (square a)
        (square b)
        (square c))
     (square smallest)))

(provide main)
(define (main . args)
  (define (demo a b c)
    (printf "~a ~a ~a => ~a~%" a b c (sum-of-squares-of-two-largest a b c)))

  (demo 1 2 3)
  (demo 3 2 1)
  (demo 9 9 9))
