#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec  mzscheme -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang scheme
(require schemeunit schemeunit/text-ui)

;; Concrete Abstractions, Chapter 5

(define (digits n)
  (let loop ([n n]
             [result '()])
    (if (< n 10)
        (cons n result)
        (loop (quotient n 10)
              (cons (remainder n 10) result)))))

(define-test-suite digits-tests
  (check-equal? (digits 0)
                `(0))
  (check-equal? (digits 2)
                `(2))
  (check-equal? (digits 12)
                `(1 2))
  (check-equal? (digits 112)
                `(1 1 2))
  (check-equal? (digits 10102)
                `(1 0 1 0 2))
  (check-equal? (digits 1)
                `(1)))

(define (digit-fold func number)
  (for/fold ([sum 0])
      ([(d i) (in-indexed (reverse (digits number)))])
      (let ([i (add1 i)])
        (+ sum (func i d)))))

(define-test-suite digit-fold-tests
  (check-equal? (digit-fold (lambda (_ x) x) 123) 6)
  (check-equal? (digit-fold (lambda (_ x) (add1 x)) 123) 9)
  (check-equal? (digit-fold * 123) 10))

(define (verify number function divisor)
  (zero?
   (remainder
    (digit-fold function number)
    divisor)))

(define (upc-function index digit)
  (if (odd? index)
      digit
      (* 3 digit)))

(define (credit-card-function index digit)
  (cond
   ((odd? index)
    digit)
   ((< digit 5)
    (* 2 digit))
   (else
    (add1 (* 2 digit)))))

(define (make-verifier twonary-function divisor)
  (lambda (number)
    (verify number twonary-function divisor)))

(define-test-suite verify-tests
  (check-true  ((make-verifier (lambda (_ x) x) 17) 123123122))
  (check-true  ((make-verifier * 11) 0262010771)) ;isbn
  (check-true  ((make-verifier upc-function 10) 036000283006)) ; The code
                                        ;is for a box of Kleenex.

  ;; Insert your credit-card number here :)
  (check-true ((make-verifier credit-card-function 10) 0))
  )

(define-test-suite all-tests
  digits-tests
  digit-fold-tests
  verify-tests)

(define (main . args)
  (exit (run-tests all-tests 'verbose)))

(provide verify main)
