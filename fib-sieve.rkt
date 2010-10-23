#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec  mzscheme -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang scheme

(require (planet soegaard/math/math))

(define (generate-fibs sink-ch)
  (thread
   (lambda ()
     (let loop ([i 1]
                [j 1])
       (channel-put sink-ch i)
       (loop (+ i j) i)))))

(define (filter-channel predicate source-ch sink-ch)
  (thread
   (lambda ()
     (let loop ()
       (let ([datum (channel-get source-ch)])
         (when (predicate datum)
           (channel-put sink-ch datum)))
       (loop)))))

(define (big-fibs sink-ch)
  (define fib-source-ch (make-channel))
  (generate-fibs fib-source-ch)
  (filter-channel
   (lambda (x)
     (and (< 227000 x)
          (prime? x)))
   fib-source-ch sink-ch))

(provide main)
(define (main)
  (define big-fibonacci-primes (make-channel))
  (big-fibs big-fibonacci-primes)
  (let ([first-big-prime-fib (channel-get big-fibonacci-primes)])
    ;; (factorize 10)  => '((2 1) (5 1)) ... i.e., 2^1 * 5^1
    (apply + (map (curry apply expt)
                  (factorize (add1 first-big-prime-fib))))))

