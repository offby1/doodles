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
  (define num-swaps 0)

  (define (swap! i)
    (define dest (- (subvector-length sv) num-swaps 1))
    (define tmp (subvector-ref sv dest))
    (printf "Swapping ~a with ~a~%" (subvector-ref sv i) tmp)
    (subvector-set! sv dest (subvector-ref sv i))
    (subvector-set! sv i tmp)
    (set! num-swaps (add1 num-swaps))
    (printf "~a~%" sv)
    tmp)

  (trace swap!)

  (let ([pivot-value (subvector-ref sv pivot-index)])
    (let loop ([slots-examined 0])
      (printf "slots-examined: ~a~%" slots-examined)
      (when (< slots-examined (subvector-length sv))
        (let ((candidate-index slots-examined))
          (let loop ()
            (let ((candidate (subvector-ref sv candidate-index)))
              (printf "candidate: ~a~%" candidate)
              (when (> candidate pivot-value)
                (printf "candidate ~a > pivot ~a~%" candidate pivot-value)
                (swap! candidate-index)
                (loop)))))

        (loop (add1 slots-examined))))

    (display sv) (newline)
    ;; Seems dumb to search for the pivot after we've finished;
    ;; perhaps keeping track of it as we go would be more
    ;; efficient.  It'd surely be more complicated, though.
    (subvector-find-first sv pivot-value)))
(trace partition!)

(define (qsort-subvector! sv)
  (case (subvector-length sv)
    ((0 1) sv)
    (else
     ;; We choose the index of an initial pivot, partition, then
     ;; update the index, since the partitioning will likely have
     ;; moved that value.
     (let ([pivot-index (partition! sv (random (subvector-length sv)))])
       (qsort-subvector! (make-subvector sv 0 pivot-index))
       (when (< pivot-index (sub1 (subvector-length sv)))
         (qsort-subvector! (make-subvector sv (add1 pivot-index)
                                           (- (subvector-length sv)
                                              pivot-index
                                              1)))))
     sv)))
(trace qsort-subvector!)
(define (p-test input-vector expected-result [pivot-index 0])
  (let ([actual-result (apply subvector (vector->list input-vector))]
        [expected-result
         (make-subvector expected-result pivot-index (vector-length expected-result))])
    (partition! actual-result pivot-index)
    (check-equal? actual-result expected-result)))

(define-test-suite parition-tests

  ;; (p-test #(1) #(1))
  ;; (p-test #(-1 1) #(-1 1))
  ;; (p-test #(1 -1) #(-1 1))
  ;; (p-test #(10 9 8 7 6) #(9 8 7 6 10))
  (p-test #(10 9 8 7 6 5 4 3 2 1) #(6 5 4 3 2 1 10 9 8 7) 4)
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
  ;; qsort-tests
  )

(define (main . args)
  (exit (run-tests all-tests 'verbose)))
(provide qsort main)
