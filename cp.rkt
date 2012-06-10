#lang racket

(module+ test
(require rackunit))

;; http://en.wikipedia.org/wiki/Cartesian_product

(define (extend accumlated elt)
  (for*/fold ([result (set)])
      ([old-set accumlated]
       [new-item elt])
      (set-add result (set-add old-set new-item))))
(module+ test
(check-equal? (extend (set (set)) (set 1 2 3))
              (set (set 1) (set 2) (set 3)))
(check-equal? (extend (set (set 1 2) (set 3 4) (set 5 6)) (set 'cat 'dog))
              (set
               (set 1 2 'cat)
               (set 1 2 'dog)
               (set 3 4 'cat)
               (set 3 4 'dog)
               (set 5 6 'cat)
               (set 5 6 'dog)
               )))

(define (cartesian-product . sets)
  (for/fold ([result (set (set))])
      ([elt sets])
      (extend result elt)))

(module+ test
(check-equal? (cartesian-product)
              (set (set)))

(check-equal? (cartesian-product (set 1 2))
              (set (set 1) (set 2)))

(check-equal? (cartesian-product (set 1 2) (set 'red 'blue) (set 'fish))

              (set
               (set 1 'red 'fish)
               (set 2 'red 'fish)
               (set 1 'blue 'fish)
               (set 2 'blue 'fish)))

(check-equal? (cartesian-product
               (set 1 2 3)
               (set 'two)
               (set 'three)
               (set 'four)
               (set 'five)
               (set 'six))
              (set
               (set 1 'two 'three 'four 'five 'six)
               (set 2 'two 'three 'four 'five 'six)
               (set 3 'two 'three 'four 'five 'six))))
