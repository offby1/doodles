#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec racket -l errortrace --require "$0" --main -- ${1+"$@"}
|#

;; Mimic the Clojure built-in "juxt"
;; http://richhickey.github.com/clojure/clojure.core-api.html

#lang racket
(require rackunit rackunit/text-ui)

(provide juxt)
(define (juxt . procs)
  (lambda args
    (map (lambda (p)
           (apply p args))
         procs)))

(define-test-suite juxt-tests
  (let ()
    (define a (curry list 'a))
    (define b (curry list 'b))
    (define c (curry list 'c))
    (define x 'x)

    (check-equal? ((juxt a b c) x) '((a x) (b x) (c x)) )))

(define-test-suite all-tests
  juxt-tests)

(provide main)
(define (main . args)
  (exit (run-tests all-tests 'verbose)))
