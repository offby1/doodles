#lang racket

;; http://en.wikipedia.org/wiki/Cartesian_product

(define (cartesian-product s1 s2)
  (apply set
         (for*/list ([one s1]
                     [two s2])
                    (list one two))))

(define (nary-cartesian-product . sets)
  (cond
   ((null? sets)
    (set))
   ((null? (rest sets))
    (first sets))
   (else
    (for/fold ([result (first sets)])
        ([s (rest sets)])
        (cartesian-product s result)))))
