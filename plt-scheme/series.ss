#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id: v4-script-template.ss 6086 2009-06-14 20:14:28Z erich $
exec  mzscheme  --require "$0" --main -- ${1+"$@"}
|#

#lang scheme

;; From Robby Finder
(define-syntax-rule (first-of-two-values e)
  (let-values ([(x y) e]) x))

(define (series x)
  (exact->inexact
   (first-of-two-values
    (for/fold ([sum 0]
               [factor 1])
        ([n (in-range 20)])
        (values
         (+ sum factor)
         (* factor (/ x (add1 n))))))))

(define (main . args)
  (for ([arg args])
    (printf "~a => ~a~%" arg (series (string->number arg)))))

(provide main)
