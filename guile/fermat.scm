(require 'primes)
(require 'multiply)
(require 'interval)

(let ()
  (define (has-fermat-property? p n)
    (define (positive-integer? x) (and  (integer? x) (positive? x)))
    (and (positive-integer? n)
         (positive-integer? p)
         (probably-prime? p)
         (zero? (remainder (- (expt n p) n) p))))

  (define (all-true? seq)
    (or (null? seq)
        (and (car seq)
             (all-true? (cdr seq)))))

  (all-true?
   (map (lambda (pair) (apply has-fermat-property? pair)) 
        (multiply (primes> 1 10)
                  (enumerate-interval 1 10)))))