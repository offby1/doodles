#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module vprintf mzscheme
(require "globals.ss"
         (only (planet "zdate.ss" ("offby1" "offby1.plt")) zdate)
         (only (lib "19.ss" "srfi")
               current-date))

(define (vprintf . args)
  (when (*verbose*)
    (apply printf args)))
(define (vtprintf . args)
  (apply vprintf
         (string-append "~a: " (car args))
         (zdate (current-date))
         (cdr args)))
(provide (all-defined))
)