#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec  mzscheme -l errortrace --require "$0" --main -- ${1+"$@"}
|#

;; http://programmingpraxis.com/2010/08/27/chinese-remainder-theorem/

#lang scheme
(require schemeunit schemeunit/text-ui)

(define (main . args)

  (define (win x)
    (and
     (equal? 10 (modulo x 11))
     (equal?  4 (modulo x 12))
     (equal? 12 (modulo x 13))))

  (for/first ([candidate (in-naturals)]
              #:when (win candidate))
      candidate))

(provide main)
