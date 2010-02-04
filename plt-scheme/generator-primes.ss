#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id: v4-script-template.ss 6182 2009-11-10 04:59:27Z erich $
exec  mzscheme -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang scheme
(require (planet schematics/schemeunit:3)
         (planet schematics/schemeunit:3/text-ui)
         scheme/generator)

(define (naturals)
  (for ([i (in-naturals)])
    (yield i)))

(define (sans-factors g n)
  (for ([i (in-generator (g))])
    (when (positive? (remainder i n))
      (yield i))))

(define (main)
  (for ([i (in-generator (sans-factors naturals 3))])
    (if (= i 10)
        (exit 0)
        (printf "It's ~a~%" i))))

(provide main)
