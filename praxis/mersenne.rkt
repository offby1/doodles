#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec racket -l errortrace --require "$0" --main -- ${1+"$@"}
|#

;; http://programmingpraxis.com/2011/06/03/mersenne-primes/

#lang racket
(require racket/trace
         (planet soegaard/math/math)

         ;; An old version, but I'm forced to use it, since this is
         ;; what soegaard/math/math uses, and when I try to require
         ;; any other version, I get an error about it being
         ;; incompatible
         (planet "memoize.ss" ("dherman" "memoize.plt" 2 1))
         )

(define/memo* (S n)
  (if (= n 1)
      4
      (- (expt (S (sub1 n)) 2) 2)))

(define (mersenne-prime? mn)
  (divides? mn (S (sub1 mn))))
(trace mersenne-prime?)

(provide main)
(define (main . args)
  (let loop ([p (next-prime 0)]
             [mersenne-primes-found 0])
    (when (< mersenne-primes-found 256)
      (when (mersenne-prime? (mersenne p))
        (fprintf (current-error-port)
                 "prime ~a => mersenne prime ~a~%" p (mersenne p))
        (loop (next-prime p)
            (add1 mersenne-primes-found)))
      (loop (next-prime p)
            mersenne-primes-found)))
)
