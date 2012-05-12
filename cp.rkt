#lang racket

(require rackunit rackunit/text-ui unstable/debug)

(struct cp (set-of-sets)
        #:transparent
        #:guard
        (lambda (arg name)
          (when (not (set? arg))
            (error name "~s is not a set" arg))
          (for ([item arg])
            (when (not (set? item))
              (error name "~s is not a set" item)))
          arg))

;; http://en.wikipedia.org/wiki/Cartesian_product

(define/contract (cartesian-product s1 s2)
  (set? set? . -> . cp?)
  (cp
   (apply
    set
    (for*/list ([one s1]
                [two s2])
               (set one two)))))

(define-test-suite cartesian-product-tests
  (check-equal? (cartesian-product (set) (set 1 2))
                (cp (set)))

  (check-equal? (cartesian-product (set 1) (set 2))
                (cp (set (set 1 2))))

  (check-equal? (cartesian-product (set 1 2) (set 'dog 'cat))
                (cp (set (set 1 'dog)
                         (set 1 'cat)
                         (set 2 'dog)
                         (set 2 'cat)))))

(define/contract (embiggen prod new)
  (cp? set? . -> . cp?)
  (cp
   (apply
    set-union
    (for/list ([old (cp-set-of-sets prod)])
      (cp-set-of-sets (cartesian-product old new))))))

(define-test-suite embiggen-tests
  (check-equal?
   (embiggen (cp (set (set 1 2))) (set 'frotz))
   (cp
    (set
     (set 1 'frotz)
     (set 2 'frotz))))

  (check-equal?
   (embiggen (cartesian-product (set 1 2) (set 'dog 'cat)) (set 'salad))
   (cp
    (set
     (set 1 'salad)
     (set 2 'salad)
     (set 'dog 'salad)
     (set 'cat 'salad)))))

(define/contract (nary-cartesian-product . sets)
  (->* () #:rest (listof set?) cp?)
  (cond
   ((null? sets)
    (cp (set)))
   ((null? (cdr sets))
    (cp (set (car sets))))
   (else
    (for/fold ([rv (cp (set (car sets)))])
        ([s (cdr sets)])
        (embiggen rv s)))))

(define-test-suite nary-tests
  (check-equal? (nary-cartesian-product)
                (cp (set)))

  (check-equal? (nary-cartesian-product (set 1 2))
                (cp (set (set 1 2))))

  (check-equal? (nary-cartesian-product (set 1 2) (set 'frotz))
                (cartesian-product (set 1 2)
                                   (set 'frotz)))

  (check-equal? (nary-cartesian-product (set 1 2) (set 'red 'blue) (set 'fish))
                (cp
                 (set
                  (set 1 'fish)
                  (set 2 'fish)
                  (set 'red 'fish)
                  (set 'blue 'fish)))))

(run-tests
 (test-suite
  "all"
  cartesian-product-tests
  embiggen-tests
  nary-tests))
