 #! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module fys mzscheme
(provide (all-defined))
(define (fisher-yates-shuffle! v)
  (define (swap! i1 i2)
    (let ((tmp (vector-ref v i1)))
      (vector-set! v i1 (vector-ref v i2))
      (vector-set! v i2 tmp)))
  (let ((l (vector-length v)))
    (do ((top-index (sub1 l) (sub1 top-index)))
        ((zero? top-index) v)
      (let ((bottom-index (random top-index)))
        (swap! bottom-index top-index)))))
)