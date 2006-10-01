#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -qu "$0" ${1+"$@"}
|#

(module zprintf mzscheme

(provide (all-defined))
(define *recursion-level* (make-parameter 0))
(define (zprintf . args)
  (when (zero? (*recursion-level*))
    (apply printf args))))