#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -qu "$0" ${1+"$@"}
|#

(module max mzscheme
(provide (rename my-max max))
(require (lib "trace.ss")
         (only (lib "1.ss" "srfi") reduce))

(define (my-max > item . items)
  (reduce (lambda (a b)
            (if (> a b) a b)) item (cons item items)))

)
