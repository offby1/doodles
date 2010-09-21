#lang racket
(require (planet soegaard/math/math))

(for/fold ([result '()])
    ([x (in-range 1000)])
    (let ([d (digits (* x x))])
      (let-values ([(left right) (split-at d (quotient (length d) 2))])
        (if (equal? x (+ (digits->number left)
                         (digits->number right)))
            (cons x result)
            result))))
