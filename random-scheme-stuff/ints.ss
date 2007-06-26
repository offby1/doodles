#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module ints mzscheme
(require (lib "assert.ss" "offby1"))

(define (integer->digits i base)
  (for-each (lambda (number)
              (check-type 'integer->digits integer?  number)
              (check-type 'integer->digits positive? number)
              (check-type 'integer->digits exact?    number))
            (list i base))
  (assert (< 1 base))
  (let loop ((i i)
             (result '()))
    (if (positive? i)
        (loop (quotient i base)
              (cons (remainder i base) result))
      result)))

(let ((b 2))
  (let loop ((i 1))
    (when (<= i 100)
      (printf "~a, base ~a: ~a~%"
              i b
              (integer->digits i b))
      (loop (add1 i)))
    ))
)