#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec racket -l errortrace --require "$0" --main -- ${1+"$@"}
|#

;; http://programmingpraxis.com/2011/01/28/population-count/

#lang racket
(require rackunit rackunit/text-ui)

(define our-contract
  (natural-number/c . -> . natural-number/c))

(provide integer->hamming-weight)
(define/contract (integer->hamming-weight n)
  our-contract
  (let loop ([n n]
             [result 0])
    (if (positive? n)
        (let-values ([(q r) (quotient/remainder n 2)])
          (loop q (+ result r)))
        result)))

(define/contract (alternate n)
  our-contract
  (count (curry eq? #\1) (string->list (number->string n 2))))

(define-test-suite integer->hamming-weight-tests
  (for ([input           `(0 1 2 3 4 8 16 ,(sub1 (expt 2 8)))]
        [expected-output `(0 1 1 2 1 1  1  8)])
    (check-equal? (integer->hamming-weight input)
                  expected-output))

  (let loop ([trials 100])
    (when (positive? trials)
      (let ([n (expt (random 4294967087) 10)])
        (let ([one-way (integer->hamming-weight n)]
              [another-way (alternate n)])
          (check-equal? one-way another-way)))
      (loop (sub1 trials)))))


(define-test-suite all-tests
  integer->hamming-weight-tests)

(provide main)
(define (main . args)
  (exit (run-tests all-tests 'verbose)))
