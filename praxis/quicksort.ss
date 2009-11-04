#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id: v4-script-template.ss 6086 2009-06-14 20:14:28Z erich $
exec  mzscheme -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang scheme
(require (planet schematics/schemeunit:3)
         (planet schematics/schemeunit:3/text-ui))

(define (qsort seq less?)
  (let-values ([(v first-index length)
                (qsort-vector (list->vector seq) 0 (length seq) less?)])
     (vector->list v)))

(define (partition! v first-index length pivot-value)
  (printf "~s~%" v)
  (printf "Pivot value is ~a~%" pivot-value)
  (let loop ([number-unknown length]
             [number-small 0])
    (when (positive? number-unknown)
      (let ((candidate (vector-ref v (+ first-index number-small))))
        (printf "Candidate is ~a~%" candidate)
        (if (<= candidate pivot-value)
            (set! number-small (add1 number-small))
            (let ((dest (sub1 (+ first-index number-small number-unknown))))
              (printf
               "Swapping [~a]: ~a with [~a]: ~a ..."
               number-small candidate dest (vector-ref v dest))
              (vector-set! v number-small (vector-ref v dest))
              (vector-set! v dest candidate)))
        (set! number-unknown (sub1 number-unknown))
        (loop (sub1 number-unknown)
              number-small)))))

(define (qsort-vector v first-index length less?)
  (define (find-pivot)
    ;; todo -- supposedly, choosing an element at random improves the
    ;; worst-case performance
    (let ([index first-index])
      (values index
              (vector-ref v index))))
  (case length
    ((0 1) (values v first-index length))
    (else
     (let-values ([(pivot-index pivot-value) (find-pivot)])
       (partition! v first-index length pivot-value)
       ;; todo -- recursive calls
       (values v first-index length)))))

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
