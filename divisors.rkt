#!/usr/bin/env raco test

;; http://programmingpraxis.com/2010/11/26/divisors-and-totatives/

#lang racket
(require rackunit rackunit/text-ui
         (prefix-in math:
                    (only-in math/number-theory
                             divisors
                             coprime?
                             totient)))

(module+ test (require rackunit))

(provide divisors)
(define divisors math:divisors)

(module+ test (check-equal? (apply set (divisors 20)) (set 1 2 4 5 10 20)))

(provide sum-of-divisors)
(define (sum-of-divisors n)
  (apply + (divisors n)))

(define (number-of-divisors n)
  (length (divisors n)))

(provide totatives)
(define (totatives n)
  (filter
   (lambda (x) (math:coprime? n x))
   (build-list (sub1 n) add1)))

(module+ test (check-equal? (apply set (totatives 30)) (set 1 7 11 13 17 19 23 29)))

(define totient math:totient)

(module+ test (check-equal? (math:totient 30) 8))
