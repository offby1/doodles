#lang racket

;; http://programmingpraxis.com/2011/04/15/partition-numbers/

(require rackunit
         (planet dherman/memoize:3:1))

(define (sum lower upper func)
  (for/fold ([s 0])
      ([k (in-range lower (add1 upper))])
      (+ s (func k))))

(define/memo* (p n)
  (cond
   ((negative? n) 0)
   ((zero? n) 1)
   (else
    (sum 1 n
         (lambda (k)
           (*
            (expt -1 (add1 k))
            (+
             (p (- n (/ (* k (sub1 (* 3 k))) 2)))
             (p (- n (/ (* k (add1 (* 3 k))) 2)))))
           )))))

(check-equal? '(1 2 3 5 7 11 15 22 30 42)
              (build-list 10 (compose p add1)))

(p 1000)