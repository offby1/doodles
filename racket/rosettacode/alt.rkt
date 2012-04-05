#lang racket

;; Like "map", but the proc must take an index as well as the element.
(define (map-index proc seq)
  (for/list ([(elt i) (in-indexed seq)])
    (proc elt i)))

(define (map-step proc step lst)
  (map-index
   (lambda (elt i)
     (if (zero? (remainder i step) )
         (proc elt)
         elt))
   lst))

(define (map-times-step proc times step lst)
  (if (zero? times)
      lst
      (map-times-step proc (sub1 times) (add1 step) (map-step proc step lst))))

(sequence-for-each
 (lambda (x n)
   (when x
     (printf "~a is open\n" n)))
 (in-indexed (map-times-step not 100 1 (make-list 100 #f))))
