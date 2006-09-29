#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -qu "$0" ${1+"$@"}
|#

(module deck mzscheme
(require "card.ss")
(define *deck*
  (let loop ((suits *suits*)
             (result '()))
    (if (null? suits)
        result
      (loop (cdr suits)
            (cons
             (let loop ((ranks (iota 13 2))
                        (result '()))
               (if (null? ranks)
                   result
                 (loop (cdr ranks)
                       (cons (make-card (car suits)
                                   (car ranks))
                             result))))
             result)))))

)