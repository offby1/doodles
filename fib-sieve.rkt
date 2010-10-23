#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec  mzscheme -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang scheme

(require (planet soegaard/math/math))

(define (generate-fibs sink)
  (thread
   (lambda ()
     (let loop ([i 1]
                [j 0])
       (channel-put sink i)
       (loop (+ i j) i)))))

(define (filter-channel predicate source sink)
  (thread
   (lambda ()
     (let loop ()
       (let ([datum (channel-get source)])
         (when (predicate datum)
           (channel-put sink datum)))
       (loop)))))

(define (big-fibs sink)
  (define fib-source (make-channel))
  (generate-fibs fib-source)
  (filter-channel
   (lambda (x)
     (and (< 227000 x)
          (prime? x)))
   fib-source sink))

(provide main)
(define (main)
  (define big-fibonacci-primes (make-channel))
  (big-fibs big-fibonacci-primes)
  (let ([first-big-prime-fib (channel-get big-fibonacci-primes)])
    ;; (factorize 10)  => '((2 1) (5 1)) ... i.e., 2^1 * 5^1
    (apply + (map (curry apply expt)
                  (factorize (add1 first-big-prime-fib))))))

