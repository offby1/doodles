#lang racket

(require unstable/debug)

(define (something items tuples)
  (for/fold ([result '()])
      ([item items])
      (append (map (curry cons item) tuples)
              result)))

(define (cartesian-product . seqs)
  (for/fold ([result '()])
      ([seq seqs])
      (something seq result)))
