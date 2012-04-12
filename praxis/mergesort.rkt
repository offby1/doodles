#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec racket -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang racket
(require rackunit rackunit/text-ui)

(define (mergesort v [< <])
  (let ([size (vector-length v)])
    (case size
      ((0 1) v)
      (else
       (let-values ([(l r) (vector-split-at v (quotient size 2))])
         (merge-vectors (mergesort l)
                        (mergesort r)
                        <))))))

(define (merge-vectors l r [< <])
  (let ([result (make-vector (+ (vector-length l)
                                (vector-length r)))])
    (let loop (
               [result-size 0]
               [l-consumed 0]
               [r-consumed 0])
      (cond
       ((equal? l-consumed (vector-length l))
        (vector-copy! result result-size r r-consumed))
       ((equal? r-consumed (vector-length r))
        (vector-copy! result result-size l l-consumed))
       ((< (vector-ref l l-consumed)
           (vector-ref r r-consumed))
        (vector-set! result result-size (vector-ref l l-consumed))
        (loop (add1 result-size)
              (add1 l-consumed)
              r-consumed))
       (else
        (vector-set! result result-size (vector-ref r r-consumed))
        (loop (add1 result-size)
              l-consumed
              (add1 r-consumed)))))

    result))

(define (sorted? v)
  ;; <= demands two arguments.
  (or (< (vector-length v) 2)
      (apply <= (vector->list v))))

(define-simple-check (check-sorted? thing)
  (sorted? thing))

(define-test-suite mergesort-tests
  (check-sorted? (mergesort '#()))
  (check-sorted? (mergesort '#(99)))
  (check-sorted? (mergesort '#(1 99)))
  (check-sorted? (mergesort '#(99 1)))
  (check-sorted? (mergesort (build-vector 10 (lambda _ (random)))))
  )

(define-test-suite all-tests
  mergesort-tests)

(provide main)
(define (main . args)
  (exit (run-tests all-tests 'verbose)))
