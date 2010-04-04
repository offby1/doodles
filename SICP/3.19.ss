#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec  mzscheme -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang scheme
(require schemeunit schemeunit/text-ui
         (only-in srfi/1 circular-list))

(define/contract (has-cycle? seq)
  ((or/c cons? null?) . -> . boolean?)
  (let loop ([tortoise seq]
             [hare seq]
             [initial #t])
    (cond
     ((null? hare)
      #f)
     ((null? (cdr hare))
      #f)
     ((and (not initial)
           (eq? tortoise hare))
      #t)
     (else
      (loop (cdr tortoise)
            (cddr hare)
            #f)))))

(define-test-suite has-cycle?-tests

  (check-true  (has-cycle? (circular-list 1 2 3)))
  (check-false (has-cycle? (list 1 2 3)))
  (check-false (has-cycle? (list))))

(define (main . args)
  (exit (run-tests has-cycle?-tests 'verbose)))
(provide has-cycle? main)
