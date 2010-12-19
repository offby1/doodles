#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id: v4-script-template.ss 6086 2009-06-14 20:14:28Z erich $
exec  mzscheme -l errortrace --require "$0" --main -- ${1+"$@"}
|#

;;  http://programmingpraxis.com/2009/10/09/calculating-pi/

#lang scheme

(define (random-point)
  (make-rectangular (random) (random)))

(define (is-in-circle? p)
  (<= (magnitude p)
      1))

(define *trials* 1000000)

(define (main . args)
  (* 4.0 (/
          (for/fold ([number-inside-circle 0])
              ([trial (in-range *trials*)])

              ((if (is-in-circle? (random-point)) add1 values) number-inside-circle))
          (add1 *trials*))))

(provide main)
