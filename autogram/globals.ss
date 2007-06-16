#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module globals mzscheme
(provide (all-defined))
(define *min* 1)
(define *max* 48)

(define *tries*
  (let ((value 0))
    ;; you could wrap this in a call-with-semaphore if you wanted, but
    ;; I don't think there's any need
    (lambda args
      (if (not (null? args))
          (set! value (car args)))
      value
      )))

(port-count-lines! (current-error-port))
(port-count-lines! (current-output-port))
(define (nl)
  (let-values (((line col pos)
                (port-next-location (current-output-port))))
    (unless (zero? col)
      (newline))))
)