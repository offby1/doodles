#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec mzscheme -qr "$0" ${1+"$@"}
|#

(require "memoize.scm")

(define memoize
  (make-memoizer)

  ;; uncomment this to disable memoization, and see how much slower it
  ;; is

  ;;values
  )

(define fib
  (memoize
   (lambda (x)
     (if (< x 2)
         x
       (+
        (fib (- x 1))
        (fib (- x 2)))))))

(let ((n 35))
  (printf "Fib ~a: ~a~%" n (fib n)))
