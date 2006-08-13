(require (lib "40.ss" "srfi")
         (lib "trace.ss"))

(define (integers-starting-from n)
  (stream-cons n (integers-starting-from (add1 n))))

(define positive-integers (integers-starting-from 1))

(define (without-multiples-of-first-element s)
  (stream-filter (lambda (n)
                   (not (zero? (remainder n (stream-car s)))))
                 s))

;; the first prime -- 2 -- is number 0.

;; unfortunately, the stream impl seems not to memoize -- it takes
;; just as long to calculate (nth-prime 1000) the second time as it
;; did the first.
(define (nth-prime n)
  (let loop ((n n)
             (s (stream-cdr positive-integers)))
    (if (zero? n)
        (stream-car s)
      (loop (sub1 n)
            (without-multiples-of-first-element s)))))

;; factor n by brute force.
(define (factor n)
  (define (loop n candidate-prime num-tried factors)
    (if (< n candidate-prime)
        factors
      (let-values (((q r )
                    (quotient/remainder n candidate-prime)))
        (let ((next-prime (nth-prime (add1 num-tried))))
          (if (zero? r)
              (loop q
                    candidate-prime
                    num-tried
                    (cons candidate-prime factors))
            (loop n
                next-prime
                (add1 num-tried)
                factors))
          ))))
  (trace loop)
  (loop  n  (nth-prime 0) 0 '()))
