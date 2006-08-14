(require 'interval)
(require 'filter)

(define (primes< n)

  (define filter-out-multiples-of-first-element
    (lambda (seq)
      (filter (lambda (n)
                (not (zero? (remainder n (car seq)))))
              (cdr seq))))

  (let loop ((x (enumerate-interval 2 (- n 2)))
             (primes '()))
    (if (null? x)
        (reverse primes)
      (loop (filter-out-multiples-of-first-element x)
            (cons (car x)
                  primes)))))
