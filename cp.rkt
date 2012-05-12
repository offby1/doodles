#lang racket

(require rackunit rackunit/text-ui)

;; http://en.wikipedia.org/wiki/Cartesian_product

(define (cartesian-product . sets)
  (cond
   ((null? sets)
    (set))
   ((null? (cdr sets))
    (set (car sets)))
   (else
    (for/fold ([rv (set (car sets))])
        ([s (cdr sets)])
        (apply
         set-union
         (for/list ([old  rv])
           (apply
            set
            (for*/list ([one old]
                        [two s])
                       (set one two)))))))))

(define-test-suite nary-tests
  (check-equal? (cartesian-product)
                (set))

  (check-equal? (cartesian-product (set 1 2))
                (set (set 1 2)))

  (check-equal? (cartesian-product (set 1 2) (set 'red 'blue) (set 'fish))

                (set
                 (set 1 'fish)
                 (set 2 'fish)
                 (set 'red 'fish)
                 (set 'blue 'fish)))

  (check-equal? (cartesian-product
                 (set 1 2 3)
                 (set 'two)
                 (set 'three))
                (set
                 (set 1 'two 'three)
                 (set 2 'two 'three)
                 (set 3 'two 'three))
                )
  )
(run-tests nary-tests)
