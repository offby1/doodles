#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec mzscheme -qr "$0" ${1+"$@"}
|#

(display "$Id$")
(newline)

(require (lib "40.ss" "srfi")
         (lib "1.ss" "srfi")
         "compact.ss")

(define (integers-starting-from n)
  (stream-cons n (integers-starting-from (add1 n))))

(define (without-multiples-of-first-element s)
  (stream-filter (lambda (n)
                   (positive? (remainder n (stream-car s))))
                 (stream-cdr s)))

;; factor n by brute force.
(define (factor n)
  (let loop ((n n)
             (semi-filtered-primes (integers-starting-from 2))
             (factors '()))
    (let ((candidate-prime (stream-car semi-filtered-primes)))
      (if (< n candidate-prime)
          (reverse factors)
        (let-values (((q r)
                      (quotient/remainder n candidate-prime)))
          (if (zero? r)
              (loop q
                    semi-filtered-primes
                    (cons candidate-prime factors))
            (loop n
                  (without-multiples-of-first-element semi-filtered-primes)
                  factors)))))))

(for-each
 (lambda (n)
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
 (iota 20 1))
