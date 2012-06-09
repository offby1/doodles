#lang racket

;; http://en.wikipedia.org/wiki/Partition_(number_theory)

;; Return a vector that is like V, except the INDEXth number is one
;; bigger.  As a special case, if INDEX is the size of V, then we
;; append a 1.
(define (increment-at v index)
  (if (= index (vector-length v))
      (vector-append v (vector 1))
      (for/vector
       ([(elt i) (in-indexed v)])
       ((if (= i index) add1 values) elt))))

;; Given a partition P which sums to n, return a set of all partitions
;; which sum to n + 1.
(define (more-partitions p)
  (define result (set (increment-at p 0)))

  (for ([index (in-range 1 (vector-length p))])
    (when (> (vector-ref p (sub1 index))
             (vector-ref p index))
      (set! result (set-add result (increment-at p index)))))

  (set-add result (increment-at p (vector-length p))))

(define (all-partitions n)
  (if (= n 1)
      (set (vector 1))
      (for/fold
          ([result (set)])
          ([p (all-partitions (sub1 n))])
       (set-union result (more-partitions p)))))
