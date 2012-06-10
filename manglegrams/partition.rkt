#lang racket

;; http://en.wikipedia.org/wiki/Partition_(number_theory)

(module+ test (require rackunit))

;; Return a vector that is like V, except the INDEXth number is one
;; bigger.
(define (increment-at v index)
  (let ([v (vector-copy v)])
    (dict-update! v index add1)
    v))

(module+ test
(check-equal?
 (increment-at (vector 1 2 3) 1)
 (vector 1 3 3)))

;; Given a partition P which sums to n, return a set of all partitions
;; which sum to n + 1.
(define (more-partitions p)
  (define result (set))

  (for ([index (in-range (vector-length p))])
    (when (or (zero? index)
              (> (vector-ref p (sub1 index))
                 (vector-ref p index)))
      (set! result (set-add result (increment-at p index)))))

  (set-add result (vector-append p (vector 1))))

(provide all-partitions)
(define (all-partitions n)
  (if (= n 1)
      (set (vector 1))
      (for/fold
          ([result (set)])
          ([p (all-partitions (sub1 n))])
       (set-union result (more-partitions p)))))

(module+ test
(for ([(expected index) (in-indexed '[1 1 2 3 5 7 11 15 22 30 42 56])])
  (when (positive? index)
    (check-equal? (set-count (all-partitions index)) expected))))

(module+ main
(pretty-print (all-partitions 5)))
