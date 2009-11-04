#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id: v4-script-template.ss 6086 2009-06-14 20:14:28Z erich $
exec  mzscheme -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang scheme
(require (planet schematics/schemeunit:3)
         (planet schematics/schemeunit:3/text-ui))

(define (qsort seq )
  (let-values ([(v first-index length)
                (qsort-vector! (list->vector seq) 0 (length seq) )])
     (vector->list v)))

(define (partition! v first-index length pivot-value)

  (define (swap! i1 i2)
    (define tmp (vector-ref v i2))
    (printf
     "Swapping [~a]: ~a with [~a]: ~a ..."
     i1 (vector-ref v i1) i2 tmp)
    (vector-set! v i2 (vector-ref v i1))
    (vector-set! v i1 tmp))

  (printf "~s~%" v)
  (printf "Pivot value is ~a~%" pivot-value)

  (let loop ([number-unknown length]
             [num-equal 0]
             [num-smaller 0])
    (when (positive? number-unknown)
      (let* ((c-index (+ first-index num-smaller num-equal))
             (candidate (vector-ref v c-index)))
        (printf "~a unknown elements; candidate is ~a~%" number-unknown
                candidate)
        (cond
         ((< candidate pivot-value)
          (swap! c-index num-smaller)
          (set! num-smaller (add1 num-smaller)))

         ((= candidate pivot-value)
          (set! num-equal (add1 num-equal)))

         (else
          (swap! c-index (- length number-unknown)))))

      (loop (sub1 number-unknown)
            num-equal
            num-smaller))))

(define (qsort-vector! v first-index length )
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
       (qsort-vector! v first-index pivot-index )
       (qsort-vector! v (add1 pivot-index) (- length pivot-index) )
       (values v first-index length)))))

(define (p-test input-vector expected-result)
  (let ([actual-result (apply vector (vector->list input-vector))])
    (partition! actual-result 0 (vector-length actual-result) (vector-ref actual-result 0))
    (check-equal? actual-result expected-result)))

(define-test-suite qsort-tests

  (p-test #(1) #(1))
  (p-test #(-1 1) #(-1 1))
  (p-test #(1 -1) #(-1 1))
  (p-test #(10 9 8 7 6) #(9 8 7 6 10))

  (check-equal? (qsort '()) '())
  (check-equal? (qsort '(1)) '(1))
  (check-equal? (qsort '(1 2)) '(1 2))
  (check-equal? (qsort '(9 8 7 6 5 4 3 2 1)) '(1 2 3 4 5 6 7 8 9))
  (check-equal? (qsort '(2 2)) '(2 2))
  )

(define (main . args)
  (exit (run-tests qsort-tests 'verbose)))
(provide qsort main)
