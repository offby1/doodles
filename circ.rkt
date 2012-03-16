#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec racket -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang racket
(require rackunit rackunit/text-ui)

(provide is-circular?)
(define (is-circular? seq)
  (cond
   ((not (pair? seq))
    #f)
   ((not (pair? (cdr seq)))
    #f)
   (else
    (let loop ([tortoise seq]
               [hare (cdr seq)])
      (cond
       ((null? tortoise)
        #f)
       ((null? hare)
        #f)
       ((null? (cdr hare))
        #f)
       ((eq? tortoise hare)
        #t)
       (else
        (loop (cdr tortoise)
              (cddr hare)))
       )))))

(define-test-suite is-circular?-tests
  (check-false (is-circular? '()))
  (check-false (is-circular? '(1 2 3 2 1)))
  (check-true (is-circular? (with-input-from-string "#0=(1 . #0#)"  read)))

  ;; Not sure if this example is truly circular.
  ;;(check-true (is-circular?  (with-input-from-string "#0=(1 2 3 #0# 4 5 6)"  read)))
  )

(define-test-suite all-tests
  is-circular?-tests)

(provide main)
(define (main . args)
  (exit (run-tests all-tests 'verbose)))
