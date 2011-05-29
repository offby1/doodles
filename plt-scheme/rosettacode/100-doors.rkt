;; http://rosettacode.org/wiki/100_doors

#lang racket

(define *n-doors* 100)
(define *doors* (make-vector *n-doors* #f))

(for ([pass (in-range *n-doors*)])
  (for ([door (in-range *n-doors*)])
    (when (zero? (remainder door (add1 pass)))
      (dict-update! *doors* door not))))

(for ([(door index) (in-indexed *doors*)])
  (when door
    (displayln index)))
