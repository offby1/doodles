#lang racket

(require "cp.rkt"
         "partition.rkt")

(define (wozzit char number)
  (apply set
  (set-map
   (all-partitions number)
   (lambda (v)
     (map
      (lambda (n) (make-string n char))
      (vector->list v))))))

(module+ main

(pretty-print
(apply cartesian-product (list (wozzit #\b 1) (wozzit #\a 2) (wozzit #\n 2))) ))
