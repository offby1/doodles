#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id: v4-script-template.ss 6086 2009-06-14 20:14:28Z erich $
exec  mzscheme -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang scheme
(require (planet schematics/schemeunit:3)
         (planet schematics/schemeunit:3/text-ui))

(define (qsort seq less?)
  (vector->list
   (qsort-vector (list->vector seq) less?)))

(define (qsort-vector v less?)
  (define (find-pivot v)
    (vector-ref v 0))
  (if (zero? (vector-length v))
      v
      (let ([pivot-value (find-pivot v)])
        (printf "~s~%" v)
        (printf "Pivot value is ~a~%" pivot-value)
        (let ([opposite-index (sub1 (vector-length v))])
          (for ([(value index) (in-indexed v)])
            (printf "index: ~a; value: ~a~%" index value)
            (when (< pivot-value value)
              (printf
               "Swapping [~a]: ~a with [~a]: ~a ..."
               index value opposite-index (vector-ref v opposite-index))
              (vector-set! v index (vector-ref v opposite-index))
              (vector-set! v opposite-index value)
              (printf " => ~s~%" v)
              (set! opposite-index (sub1 opposite-index)))
            ))
        v)))

(define-test-suite qsort-tests

  (check-equal? (qsort '() <) '())
  (check-equal? (qsort '(1) <) '(1))
  (check-equal? (qsort '(1 2) <) '(1 2))
  (check-equal? (qsort '(2 1) <) '(1 2))
  (check-equal? (qsort '(2 2) <) '(2 2))

  )

(define (main . args)
  (exit (run-tests qsort-tests 'verbose)))
(provide qsort main)
