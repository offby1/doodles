#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module jordanb mzscheme
(require (only (planet "memoize.ss" ("dherman" "memoize.plt" )) define/memo*)
         "globals.ss")
(provide all-jordanb-quotes
         one-jordanb-quote)

;; no need to memoize this, but what the hell.
(define/memo* (all-jordanb-quotes)
  (call-with-input-file (*jordanb-quotes-file-name*) read))

(define (one-jordanb-quote)
  (list-ref
   (all-jordanb-quotes)
   (random (length (all-jordanb-quotes))))))
