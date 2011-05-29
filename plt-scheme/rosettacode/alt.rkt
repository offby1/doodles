#lang racket

(define (map-step proc step lst)
    (for/list ((i lst) (n (in-naturals 1)))
      (if (equal? (modulo n step) 0)
          (proc i)
          i)))

(define (map-times-step proc times step lst)
  (if (equal? times 1)
      lst
      (map-times-step proc (sub1 times) (add1 step) (map-step proc step lst))))

(sequence-for-each (lambda (x n)
            (when x
            (printf "~a is open\n" (add1 n))))
          (in-indexed (map-times-step (lambda (x) (not x)) 101 1 (make-list 100 #f))))