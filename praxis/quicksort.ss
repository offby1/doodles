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

(define (partition! v pivot-value)
  (printf "~s~%" v)
  (printf "Pivot value is ~a~%" pivot-value)
  (let loop ([number-unknown (vector-length v)]
             [number-small 0])
    (when (positive? number-unknown)
      (let ((candidate (vector-ref v number-small)))
        (printf "Candidate is ~a~%" candidate)
        (if (<= candidate pivot-value)
            (set! number-small (add1 number-small))
            (let ((dest (sub1 (+ number-small number-unknown))))
              (printf
               "Swapping [~a]: ~a with [~a]: ~a ..."
               number-small candidate dest (vector-ref v dest))
              (vector-set! v number-small (vector-ref v dest))
              (vector-set! v dest candidate)))
        (set! number-unknown (sub1 number-unknown))
        (loop (sub1 number-unknown)
              number-small)))
    v))

(define (qsort-vector v less?)
  (define (find-pivot v)
    ;; todo -- supposedly, choosing an element at random improves the
    ;; worst-case performance
    (let ([index 0])
      (values index
              (vector-ref v index))))
  (case  (vector-length v)
    ((0 1) v)
    (else
     (let-values ([(pivot-index pivot-value) (find-pivot v)])
       (partition! v pivot-value)
       ;; todo -- recursive calls
       v))))

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
