#lang racket

;; http://en.wikipedia.org/wiki/Partition_(number_theory)

;; Return a vector that is like V, except the INDEXth number is one
;; bigger.
(define (increment-at v index)
  (let ([v (vector-copy v)])
    (dict-update! v index add1)
    v))

;; Given a partition P which sums to n, return a set of all partitions
;; which sum to n + 1.
(define (more-partitions p)
  (define result (set (increment-at p 0)))

  (for ([index (in-range 1 (vector-length p))])
    (when (> (vector-ref p (sub1 index))
             (vector-ref p index))
      (set! result (set-add result (increment-at p index)))))

  (set-add result (vector-append p (vector 1))))

(define (all-partitions n)
  (if (= n 1)
      (set (vector 1))
      (for/fold
          ([result (set)])
          ([p (all-partitions (sub1 n))])
       (set-union result (more-partitions p)))))
