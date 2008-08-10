#lang scheme

;; http://mathworld.wolfram.com/PiFormulas.html

(define (sum init stop func)
  (for/fold ([accum 0])
            ((n (in-range init stop)))
            (+ accum (func n))))

(sum 0 10 (lambda (n)
           (* (- (/ 4 (+ 1 (* 8 n)))
                 (/ 2 (+ 4 (* 8 n)))
                 (/ 1 (+ 5 (* 8 n)))
                 (/ 1 (+ 6 (* 8 n))))
              (expt 1/16 n))))

(* 4 (sum 0 1000 (lambda (k)
                   ( /
                     (expt -1 k)
                     (add1 (* 2 k))))))

(* (/ (expt 2 6))
   (sum 0 10 (lambda (n)
               (* (/ (expt -1 n)
                     (expt 2 (* 10 n)))
                  (+
                   (- (/ (expt 2 5)
                         (+ (* 4 n) 1)))
                   (- (/ 1
                         (+ (* 4 n)
                            3)))
                   (/ (expt 2 8)
                      (+ (* 10 n) 1))
                   (- (/ (expt 2 6)
                         (+ (* 10 n)
                            3)))
                   (- (/ (expt 2 2)
                         (+ (* 10 n)
                            5)))
                   (- (/ (expt 2 2)
                         (+ (* 10 n)
                            7)))
                   (/ (+ (* 10 n)
                         9)))))))

(define (simple-continued-fraction . numbers)
  (if (null? (cdr numbers))
      (car numbers)
      (+ (car numbers)
         (/ (apply simple-continued-fraction (cdr numbers))))))

(exact->inexact (simple-continued-fraction 3 7 15 1 292 1 1 1))
