#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id: v4-script-template.ss 6086 2009-06-14 20:14:28Z erich $
exec  mzscheme -l errortrace --require "$0" --main -- ${1+"$@"}
|#

;;  http://programmingpraxis.com/2009/10/09/calculating-pi/

#lang scheme

(define (random-point)
  (+ (random)
     (* +i (random))))

(define (is-in-circle? p)
  (<= (magnitude p)
      1))

(define (main . args)
  (let loop ((number-inside-circle 0)
             (trials 0)
             (p (random-point)))
    (if (< trials 10000)
      (loop
       ((if (is-in-circle? p) add1 values) number-inside-circle)
       (add1 trials)
       (random-point))
      (* 4.0 (/ number-inside-circle trials)))))

(provide main)
