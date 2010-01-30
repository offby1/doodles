#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id: v4-script-template.ss 6182 2009-11-10 04:59:27Z erich $
exec  mzscheme -l errortrace --require "$0" --main -- ${1+"$@"}
|#

;; http://programmingpraxis.com/2010/01/05/the-sum-of-two-squares/

#lang scheme
(require (planet schematics/schemeunit:3)
         (planet schematics/schemeunit:3/text-ui))

(define (break-em-down n)
  (for*/fold ([pairs '()])
      ([x (in-range n)]
       [y (in-range (add1 x))])
      (if (= n (+ (sqr x)
                  (sqr y)))
          (begin
            (printf "~a^2 + ~a^2 = ~a~%" x y n)
            (cons (list x y) pairs))
          pairs)))

(define-test-suite break-em-down-tests

  (check-equal? '((3 1)) (break-em-down 10))
  (check-equal? '((7 1) (5 5)) (break-em-down 50))
  (check-equal? '() (break-em-down 999)))

(define (main . args)
  (let ([test-result (run-tests break-em-down-tests 'verbose)])
    (when (not (zero? test-result))
      (exit test-result))
    (break-em-down 48612265)))

(provide break-em-down main)
