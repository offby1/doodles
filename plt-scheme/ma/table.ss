#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module table "modarith.ss"
(require (only (lib "1.ss" "srfi") iota)
         (lib "pretty.ss")
         )
(define (zee-pee-star p)
   (map add1 (iota (sub1 p))))

(define (power-table m)
  (with-arithmetic-modulo
   m
   (map (lambda (n)
          (map (lambda (i) (expt i n)) (zee-pee-star m)))
        (zee-pee-star m))))
(pretty-print-columns 150)
(pretty-display (power-table 5))
(printf "~a~%" (make-string 80 #\-))
(pretty-display (power-table 47))

)