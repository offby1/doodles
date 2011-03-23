#lang racket
(require (planet soegaard/math/math))

;; http://programmingpraxis.com/2011/03/22/two-kaprekar-exercises/

(define (prepend-zeroes d)
  (let loop ([d d])
    (if (< (length d) 4)
        (loop (cons 0 d))
        d)))

;; This could be memoized, but when I try, I get an error that says
;; that I'm trying to load two incompatible versions of memoize
;; (math.ss is loading the older version)
(define (kaprekar-chain-from n)
  (let ([d (prepend-zeroes (digits n))])
    (when (< 4 (length d))
      (printf "~a: Too many digits~%" n)
      #f)
    (when (= 1 (set-count  (apply set d)))
      (printf "~a: Too few distinct digits~%" n)
      #f)
    (let loop ([d  d]
               [result (list (digits->number d))])
      (let* ([asc  (sort d <)]
             [desc (reverse asc)]
             [diff (abs (- (digits->number asc)
                           (digits->number desc)))])
        (cond
         ((zero? diff)
          #f)
         ((= 6174 diff)
          (cons diff result))
         (else
          (loop (prepend-zeroes (digits diff))
                (cons diff result)))
         )))))

;; longest possible chain
(for/fold ([result #f])
    ([x (in-range 1 10000)])

    (let ([this-chain (kaprekar-chain-from x)])
      (if (and this-chain
               (or (not result)
                   (< (length (cdr result))
                      (length this-chain))))

          (cons x this-chain)
          result)))

;; Kaprekar numbers
(for/fold ([result '()])
    ([x (in-range 1000)])
    (let ([d (digits (* x x))])
      (let-values ([(left right) (split-at d (quotient (length d) 2))])
        (if (equal? x (+ (digits->number left)
                         (digits->number right)))
            (cons x result)
            result))))
