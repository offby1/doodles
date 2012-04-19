#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec racket --require "$0" --main -- ${1+"$@"}
|#

#lang racket
(require rackunit rackunit/text-ui)

(struct funhash (h)
        #:transparent
        #:property prop:procedure
        (lambda (self key)
          (hash-ref (funhash-h self) key)))

(provide make-functional-hash)
(define (make-functional-hash . stuff)
  (funhash (apply make-immutable-hash stuff)))

(define-test-suite make-functional-hash-tests
  (let ([h (make-functional-hash '((one . 1) (two . 2)))])
    (check-equal? (h 'one) 1)
    (check-equal? (h 'two) 2)
    (check-exn exn:fail:contract? (thunk (h 'three)))))

(define-test-suite all-tests
  make-functional-hash-tests)

(provide main)
(define (main . args)
  (exit (run-tests all-tests 'verbose)))
