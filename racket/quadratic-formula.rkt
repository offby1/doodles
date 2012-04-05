#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec racket -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang racket
(require rackunit rackunit/text-ui)

(define (plus-or-minus a b)
  (set (+ a b)
       (- a b)))

(define-test-suite plus-or-minus-tests
  (check-equal? (plus-or-minus 3 10)
                (set -7 13)))

(provide quad)
(define (quad a b c)
  (apply set
         (set-map
          (plus-or-minus (- b) (sqrt (- (* b b) (* 4 a c))))
          (curryr / 2 a))))

(define-test-suite quad-tests
  ;; x^2 + 2x + 1 = 0 => x = -1
  (check-equal? (set -1)   (quad 1 2 1))
  (check-equal? (set  1)   (quad 1 -2 1))

  ;; (x - 1)×(x + 1) = 0 => x = 1 or x = -1
  (check-equal? (set -1 1) (quad 1 0 -1)))

(define-test-suite all-tests
  plus-or-minus-tests
  quad-tests)

(provide main)
(define (main . args)
  (exit (run-tests all-tests 'verbose)))
