#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id: v4-script-template.ss 6086 2009-06-14 20:14:28Z erich $
exec  mzscheme -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang scheme
(require (planet schematics/schemeunit:3)
         (planet schematics/schemeunit:3/text-ui)
         (except-in "subvector.ss" main)
         mzlib/trace)

(define (qsort seq )
  (subvector->list (qsort-subvector! (list->subvector seq))))

(define (partition! sv pivot-index)

  (define (swap! i1 i2)
    (define tmp (subvector-ref sv i2))
    (subvector-set! sv i2 (subvector-ref sv i1))
    (subvector-set! sv i1 tmp))

  (let ([pivot-value (subvector-ref sv pivot-index)])
    (let loop ([number-unknown (subvector-length sv)]
               [num-equal 0]
               [num-smaller 0])
      (when (positive? number-unknown)
        (let* ((candidate-index (+ num-smaller num-equal))
               (candidate (subvector-ref sv candidate-index)))
          (cond
           ((< candidate pivot-value)
            (swap! candidate-index num-smaller)
            (set! num-smaller (add1 num-smaller)))

           ((= candidate pivot-value)
            (set! num-equal (add1 num-equal)))

           (else
            (swap! candidate-index (- (subvector-length sv) number-unknown)))))

        (loop (sub1 number-unknown)
              num-equal
              num-smaller))
      (subvector-find-first sv pivot-value))))

(define (qsort-subvector! sv)
  (case (subvector-length sv)
    ((0 1) sv)
    (else
     ;; We choose the index of an initial pivot, partition, then
     ;; update the index, since the partitioning will likely have
     ;; moved that value.

     ;; TODO -- supposedly, choosing an element at random improves the
     ;; worst-case performance
     (let ([pivot-index (partition! sv 0)])
       (qsort-subvector! (make-subvector sv 0 pivot-index))
       (when (< pivot-index (sub1 (subvector-length sv)))
         (qsort-subvector! (make-subvector sv (add1 pivot-index)
                                           (- (subvector-length sv)
                                              pivot-index
                                              1)))))
     sv)))
(trace qsort-subvector!)
(define (p-test input-vector expected-result)
  (let ([actual-result (apply subvector (vector->list input-vector))]
        [expected-result (make-subvector expected-result 0 (vector-length expected-result))])
    (partition! actual-result 0)
    (check-equal? actual-result expected-result)))

(define-test-suite parition-tests

  (p-test #(1) #(1))
  (p-test #(-1 1) #(-1 1))
  (p-test #(1 -1) #(-1 1))
  (p-test #(10 9 8 7 6) #(9 8 7 6 10))
  )

(define-test-suite qsort-tests

  (check-equal? (qsort '()) '())
  (check-equal? (qsort '(1)) '(1))
  (check-equal? (qsort '(1 2)) '(1 2))
  (check-equal? (qsort '(2 2)) '(2 2))
  (check-equal? (qsort '(9 8 7 6 5 4 3 2 1)) '(1 2 3 4 5 6 7 8 9))
  )

(define-test-suite all-tests
  parition-tests
  qsort-tests
  )

(define (main . args)
  (exit (run-tests all-tests 'verbose)))
(provide qsort main)
