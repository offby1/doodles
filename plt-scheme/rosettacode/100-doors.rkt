;; http://rosettacode.org/wiki/100_doors

#lang racket

(require racket/trace)

(define *n-doors* 100)
(define *doors* (make-vector *n-doors* #f))

(define (toggle-slot! vec i)
  (dict-update! vec i not)
  vec)

(for ([pass (in-range *n-doors*)])
  (for ([slot (in-range *n-doors*)])
    (when (zero? (remainder slot (add1 pass)))
      (toggle-slot! *doors* slot))))

(for ([(door index) (in-indexed *doors*)])
  (when door
    (displayln index)))
