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
    (reverse
     (for/fold ([result '()])
         ([p procs])
         (cons (apply p args)
               result)))))

(define-test-suite juxt-tests
  (let ()
    (define (a thing) (list 'a thing))
    (define (b thing) (list 'b thing))
    (define (c thing) (list 'c thing))
    (define x 'x)

    (check-equal? ((juxt a b c) x) '((a x) (b x) (c x)) )))

(define-test-suite all-tests
  juxt-tests)

(provide main)
(define (main . args)
  (exit (run-tests all-tests 'verbose)))
