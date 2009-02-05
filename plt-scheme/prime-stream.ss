#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec mzscheme -qr "$0" ${1+"$@"}
|#

#lang scheme

(display "$Id$")
(newline)

(require srfi/40
         srfi/1
         "compact.ss")

(define (integers-starting-from n)
  (stream-cons n (integers-starting-from (add1 n))))

(define (without-multiples-of-first-element s)
  (stream-filter (lambda (n)
                   (positive? (remainder n (stream-car s))))
                 (stream-cdr s)))

(define prime-generator (integers-starting-from 2))
(define next-prime stream-car)

;; factor n by brute force.
(define (factor n)
  (let loop ((n n)
             (prime-generator prime-generator)
             (factors '()))
    (let ((candidate-prime (next-prime prime-generator)))
      (if (< n candidate-prime)
          (reverse factors)
        (let-values (((q r)
                      (quotient/remainder n candidate-prime)))
          (if (zero? r)
              (loop q
                    prime-generator
                    (cons candidate-prime factors))
            (loop n
                  (without-multiples-of-first-element prime-generator)
                  factors)))))))

(for ([n (in-range 20)])
  (printf
   "(= ~a ~a)~%"
   n
   (cons
    '*
    (map
     (lambda (thing)
       (if (number? thing)
           thing
           (cons 'expt thing)))
     (compact-sequence (factor n))))))
