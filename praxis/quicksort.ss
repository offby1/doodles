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
  (define DEBUG-original (subvector->list sv))
  (define num-> 0)
  (define num-<= 0)

  (define (swap-high! i)
    (define dest (- (subvector-length sv) num-> 1))
    (define tmp (subvector-ref sv dest))
    (subvector-set! sv dest (subvector-ref sv i))
    (subvector-set! sv i tmp)
    (set! num-> (add1 num->))
    tmp)

  (define (keep-low! i)
    (set! num-<= (add1 num-<=)))

  (let* ([pivot-value (subvector-ref sv pivot-index)]
         [already-sorted?
          (let loop ([MAX (subvector-ref sv 0)]
                     [MIN (subvector-ref sv 0)])
            (if (< (+ num-> num-<=) (subvector-length sv))
                (let ((candidate-index num-<=))
                  (let ((candidate (subvector-ref sv candidate-index)))
                    (if (< pivot-value candidate)
                        (swap-high! candidate-index)
                        (keep-low! candidate-index))
                    (loop (max MAX candidate)
                          (min MIN candidate))))
                (equal? MAX MIN)))])


    (let ([new-pivot-index
           ;; Seems dumb to search for the pivot after we've finished;
           ;; perhaps keeping track of it as we go would be more
           ;; efficient.  It'd surely be more complicated, though.
           (subvector-find-first sv pivot-value)])

      (when #f
        (let ([sv-<= (subvector->list (make-subvector sv 0 new-pivot-index))]
              [sv->  (subvector->list (make-subvector sv new-pivot-index))])
          (printf "partitioned ~a into ~a ~a~%" DEBUG-original sv-<= sv->))
        (when (not (is-partitioned? sv pivot-value))
          (error 'partition! "Oops, I failed ~a" sv)))

      (values already-sorted? new-pivot-index))))

(define-test-suite partition-tests

  (p-test #(1) 0)
  (p-test #(-1 1) 0)
  (p-test #(1 -1) 0)
  (p-test #(-1 1) 1)
  (p-test #(1 -1) 1)
  (p-test #(1 3 2)  1)
  (p-test #(10 9 8 7 6 5 4 3 2 1) 4)
  (p-test #(4 5 6 8 9 7) 3)
  (p-test #(2 4 6 8 9 7 5 3 1) 6)
  )

(define (qsort-subvector! sv)
  (case (subvector-length sv)
    ((0 1)
     sv)
    (else
     ;; We choose the index of an initial pivot, partition, then
     ;; update the index, since the partitioning will likely have
     ;; moved that value.
     (let ([orig-index (add1 (random (sub1 (subvector-length sv))))])
       (let-values ([(already-sorted? pivot-index) (partition! sv orig-index)])
         (when (not already-sorted?)
           (qsort-subvector! (make-subvector sv 0 pivot-index))
           (qsort-subvector! (make-subvector sv pivot-index)))))
     (let ([l (subvector->list sv)])
       (when (not (apply <= l))
         (error 'qsort-subvector! "Crap, it's not sorted: ~a" l)))
     sv)))

(define (is-partitioned? sv value)
  (let/ec return
    (let* ((as-list (subvector->list sv))
           (num-less-or-equal (apply + (map (lambda (x) (if (<= x value) 1 0)) as-list))))
      (let loop ((index 0)
                 (as-list as-list))
        (if (null? as-list)
            #t
            (let ((candidate (car as-list)))
              (if (xor  (<= candidate value)
                        (< index num-less-or-equal))
                  (return #f)
                  (loop (add1 index)
                        (cdr as-list)))))))))

(define (p-test input-vector [pivot-index 0])
  (let* ([sv (apply subvector (vector->list input-vector))]
         [pivot-value (subvector-ref sv pivot-index)])
    (partition! sv pivot-index)
    (check-not-false (is-partitioned? sv pivot-value))))

(define-test-suite qsort-tests

  (check-equal? (qsort '()) '())
  (check-equal? (qsort '(1)) '(1))
  (check-equal? (qsort '(1 2)) '(1 2))
  (check-equal? (qsort '(2 2)) '(2 2))
  (check-equal? (qsort '(9 8 7 6 5 4 3 2 1)) '(1 2 3 4 5 6 7 8 9))
  )

(define (xor . seq)
  (odd?
   (for/fold ([num-true 0])
       ([item (in-list seq)])
       (+ num-true (if item 1 0)))))

(define-test-suite xor-tests
  (check-false (xor))
  (check-not-false (xor 3))
  (check-false     (xor 6 3))
  (check-not-false (xor 3 6 7))
  )

(define-test-suite all-tests
  xor-tests
  partition-tests
  qsort-tests
  )

(define (main . args)
  (random-seed 0)
  (exit (run-tests all-tests 'verbose)))
(provide qsort main)
