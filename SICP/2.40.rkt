#lang racket

(define (build-list n proc)
  (for/list ([i (in-range n)])
    (proc i)))

(define (upto n) (build-list (add1 n) values))

(map (lambda (n) (map (curryr cons n) (upto (sub1 n)))) (upto 10))
