#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -qu "$0" ${1+"$@"}
|#

(module max mzscheme
(provide (rename my-max max))
(define (my-max > item . items)
  (let loop ((items items)
             (so-far item))
    (if (null? items)
        so-far
      (loop (cdr items)
            (if (> item (car items))
                item
              (car items))))))
)