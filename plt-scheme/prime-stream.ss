;; $Id$
(require (lib "40.ss" "srfi"))

(define (integers-starting-from n)
  (stream-cons n (integers-starting-from (add1 n))))

(define positive-integers (integers-starting-from 1))

(define (without-multiples-of-first-element s)
  (stream-filter (lambda (n)
                   (positive? (remainder n (stream-car s))))
                 (stream-cdr s)))

;; factor n by brute force.
(define (factor n)
  (define (loop n semi-filtered-primes factors)
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
                  factors))))))
  (loop n (stream-cdr positive-integers) '()))
