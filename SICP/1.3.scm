#lang planet neil/sicp

(define (square z)
  (* z z))

(define (sum-of-squares-of-two-largest a b c)

  (- (+ (square a)
        (square b)
        (square c))
     (square (min a b c))))


(define (demo a b c)
  (for-each (lambda (thing )
              (display thing)
              (display " "))
            (list a b c (sum-of-squares-of-two-largest a b c)))
  (newline))

(demo 1 2 3)
(demo 3 2 1)
(demo 9 9 9)
