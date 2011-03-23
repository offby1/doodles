#lang racket
(require (planet soegaard/math/math))

;; http://programmingpraxis.com/2011/03/22/two-kaprekar-exercises/

(define (prepend-zeroes d)
  (let loop ([d d])
    (if (< (length d) 4)
        (loop (cons 0 d))
        d)))

(define (kaprekar-chain-from n)
  (let ([d (digits n)])
    (when (< 4 (length d))
      (error "Too many digits"))
    (when (= 1 (set-count  (apply set d)))
      (error "Too few distinct digits"))
    (let loop ([d (prepend-zeroes d)]
               [result (list (digits->number d))])
      (printf "d is ~a~%" d)
      (let* ([asc  (sort d <)]
             [desc (reverse asc)]
             [diff (abs (- (digits->number asc)
                           (digits->number desc)))])
        (if (= 6174 diff)
            (cons diff result)
            (loop (prepend-zeroes (digits diff))
                  (cons diff result)))))))

;; Kaprekar numbers
(for/fold ([result '()])
    ([x (in-range 1000)])
    (let ([d (digits (* x x))])
      (let-values ([(left right) (split-at d (quotient (length d) 2))])
        (if (equal? x (+ (digits->number left)
                         (digits->number right)))
            (cons x result)
            result))))
