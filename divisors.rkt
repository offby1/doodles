#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec racket -l errortrace --require "$0" --main -- ${1+"$@"}
|#

;; http://programmingpraxis.com/2010/11/26/divisors-and-totatives/

#lang racket
(require rackunit rackunit/text-ui
         (prefix-in math:
                    (only-in (planet soegaard/math/math)
                             positive-divisors
                             coprime?
                             totient)))

(provide divisors)
(define divisors math:positive-divisors)

(define-test-suite divisors-tests
  (check-equal? (apply set (divisors 20)) (set 1 2 4 5 10 20)))

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

(define-test-suite totatives-tests
  (check-equal? (apply set (totatives 30))
                (set 1 7 11 13 17 19 23 29)))

(define totient math:totient)

(define-test-suite totient-tests
  (check-equal? (math:totient 30) 8))

(define-test-suite all-tests
  divisors-tests
  totatives-tests
  totient-tests)

(provide main)
(define (main . args)
  (exit (run-tests all-tests 'verbose)))
