#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module deck mzscheme
(require "card.ss"
         (only (lib "1.ss" "srfi") iota take))
(define *deck*
  (let loop ((suits *suits*)
             (result '()))
    (if (null? suits)
        result
      (loop (cdr suits)
            (append
             (let loop ((ranks (iota 13 2))
                        (result '()))
               (if (null? ranks)
                   result
                 (loop (cdr ranks)
                       (cons (make-card (car suits)
                                   (car ranks))
                             result))))
             result)))))

(define east (take *deck* 13))
(set! *deck* (list-tail *deck* 13))

(define south (take *deck* 13))
(set! *deck* (list-tail *deck* 13))

(define west (take *deck* 13))
(set! *deck* (list-tail *deck* 13))

(define north (take *deck* 13))



)