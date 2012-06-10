#lang racket

(require "cp.rkt"
         "partition.rkt")

(define (bagify str)
  (for/fold ([r (make-immutable-hash)])
      ([ch (map char-downcase (filter char-alphabetic? (string->list str)))])
      (hash-update r ch add1 0)))

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
(apply cartesian-product (dict-map (bagify "banana") wozzit)) ))
