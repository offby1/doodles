#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -qu "$0" ${1+"$@"}
|#

(module trick mzscheme
(print-struct #t)
(require (lib "assert.ss" "offby1"))
(provide (all-defined))

(define-struct trick (cards) #f)
(define (trick-complete? t)
  (= (length (trick-cards) 4)))

)