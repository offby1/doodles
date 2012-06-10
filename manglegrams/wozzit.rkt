#lang racket

(require "cp.rkt"
         "partition.rkt")

(define (wozzit char number)
  (apply
   set
   (set-map
    (all-partitions number)
    (lambda (v)
      (map
       (lambda (n) (make-string n char))
       (vector->list v))))))

(module+ main

(pretty-print
(cartesian-product  (set '("aa") '("a" "a"))
                    (set '("b"))
                    (set '("nn") '("n" "n"))) ))
