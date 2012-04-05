#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec  mzscheme --require "$0" --main -- ${1+"$@"}
|#

;; From Eli Barzilay

#lang lazy

(define nats (cons 1 (map add1 nats)))
(define (divides? n m) (zero? (modulo m n)))
(define (sift n l) (filter (lambda (x) (not (divides? n x))) l))
(define (sieve l) (cons (car l) (sieve (sift (car l) (cdr l)))))
(define primes (sieve (cdr nats)))

(provide main)
(define (main)
  (!!  (take 10 primes)))
