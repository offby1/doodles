#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module quotes mzscheme
(require (only (planet "memoize.ss" ("dherman" "memoize.plt" )) define/memo*)
         "globals.ss")
(provide all-quotes
         one-quote)

;; no need to memoize this, but what the hell.
(define/memo* (all-quotes)
  (call-with-input-file (*quotes-file-name*) read))

(define (one-quote)
  (list-ref
   (all-quotes)
   (random (length (all-quotes))))))
