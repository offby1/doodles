#lang racket

(require rackunit rackunit/text-ui unstable/debug)

;; http://en.wikipedia.org/wiki/Cartesian_product

(define (cp-helper s1 s2)
  (apply
   set
   (for*/list ([one s1]
               [two s2])
              (set one two))))

(define-test-suite cartesian-product-tests
  (check-equal? (cp-helper (set) (set 1 2))
                (set))

  (check-equal? (cp-helper (set 1) (set 2))
                (set (set 1 2)))

  (check-equal? (cp-helper (set 1 2) (set 'dog 'cat))
                (set (set 1 'dog)
                     (set 1 'cat)
                     (set 2 'dog)
                     (set 2 'cat))))

(define (embiggen prod new)
  (apply
   set-union
   (for/list ([old  prod])
     (cp-helper old new))))

(define-test-suite embiggen-tests
  (check-equal?
   (embiggen  (set (set 1 2)) (set 'frotz))

   (set
    (set 1 'frotz)
    (set 2 'frotz)))

  (check-equal?
   (embiggen (cp-helper (set 1 2) (set 'dog 'cat)) (set 'salad))

   (set
    (set 1 'salad)
    (set 2 'salad)
    (set 'dog 'salad)
    (set 'cat 'salad))))

(define (cartesian-product . sets)
  (cond
   ((null? sets)
    (set))
   ((null? (cdr sets))
    (set (car sets)))
   (else
    (for/fold ([rv (set (car sets))])
        ([s (cdr sets)])
        (embiggen rv s)))))

(define-test-suite nary-tests
  (check-equal? (cartesian-product)
                (set))

  (check-equal? (cartesian-product (set 1 2))
                (set (set 1 2)))

  (check-equal? (cartesian-product (set 1 2) (set 'frotz))
                (cp-helper (set 1 2)
                           (set 'frotz)))

  (check-equal? (cartesian-product (set 1 2) (set 'red 'blue) (set 'fish))

                (set
                 (set 1 'fish)
                 (set 2 'fish)
                 (set 'red 'fish)
                 (set 'blue 'fish))))

(run-tests
 (test-suite
  "all"
  cartesian-product-tests
  embiggen-tests
  nary-tests))
