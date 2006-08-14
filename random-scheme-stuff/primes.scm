(require 'factor)
(require 'sort)
(require 'primes)
;;(load "c:/scheme/doodles/time.scm")

;; Here are some numbers from a Pentium 90 running Windows NT 4.0:

;; (time '(sort (factor (- (expt 2 50) 1)) <))
;; run time : 5.45800018310547 seconds
;; real time: 5.45800018310547 seconds
;; (3 11 31 251 601 1801 4051)

;; (time '(sort (factor (- (expt 2 51) 1)) <))
;; run time : 168.292007446289 seconds
;; real time: 168.302001953125 seconds
;; (7 103 2143 11119 131071)

;; Here's an algorithm from
;; http://www.math.grin.edu/~stone/events/scheme-workshop/factoring.html

;; (time '(sort (another-factor (- (expt 2 57) 1)) <))
;; run time : 18.0559997558594 seconds
;; real time: 18.0559997558594 seconds
;; (7 32377 524287 1212847)

(define another-factor
  (let ((extract-twos
         (lambda (n)
           (let loop ((two-list '())
                      (rest n))
             (if (even? rest)
                 (loop (cons 2 two-list) (quotient rest 2))
               (cons rest two-list)))))

        (extract-odd-factors
         (lambda (partial-factorization)
           (let loop ((so-far (cdr partial-factorization))
                      (odd-product (car partial-factorization))
                      (trial-divisor 3))
             (cond ((< odd-product (* trial-divisor trial-divisor))
                    (reverse (cons odd-product so-far)))
                   ((zero? (remainder odd-product trial-divisor))
                    (loop (cons trial-divisor so-far)
                          (quotient odd-product trial-divisor)
                          trial-divisor))
                   (else
                    (loop so-far
                          odd-product
                          (+ trial-divisor 2))))))))

    (lambda (n)
      (let ((partial-factorization (extract-twos n)))
        (if (= (car partial-factorization) 1)
            (cdr partial-factorization)
          (extract-odd-factors partial-factorization))))))