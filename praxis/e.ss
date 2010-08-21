#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec  mzscheme -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang scheme
;; http://programmingpraxis.com/2010/08/13/e/

(define (trial)
  (let loop ([number-of-numbers 0]
             [sum 0])
    (if (<= 1 sum)
        number-of-numbers
        (loop (add1 number-of-numbers)
              (+ (random) sum)))))

(define (main)
  (let loop ([num-trials 0]
             [sum 0.0])
    (if (= 10000000 num-trials)
        (/ sum num-trials)
        (loop (add1 num-trials)
              (+ sum (trial))))))

(provide main)
