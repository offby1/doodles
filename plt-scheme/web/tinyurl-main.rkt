#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec racket -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang web-server

(require
 web-server/servlet-env
 (only-in "tinyurl.rkt" go))

(provide interface-version)
(define interface-version 'v2)

(provide main)
(define (main . args)

  (go))
