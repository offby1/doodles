#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec racket --require "$0" --main -- ${1+"$@"}
|#

;; Guts stolen from http://en.wikipedia.org/wiki/Quicksort

#lang racket
(require
 rackunit
 rackunit/text-ui)

(define (vector-swap! v x y)
  (when (not (= x y))
    (let ([tmp (vector-ref v x)])
      (vector-set! v x (vector-ref v y))
      (vector-set! v y tmp))))

(define (partition! array left right pivotIndex [< <])
  (let ([pivotValue (vector-ref array pivotIndex)])
    (vector-swap! array pivotIndex right)
    (let ([storeIndex left])
      (for ([i (in-range left right)])
        (when (< (vector-ref array i)
                 pivotValue)
          (vector-swap! array i storeIndex)
          (set! storeIndex (add1 storeIndex))))
      (vector-swap! array storeIndex right)
      storeIndex)))

(define (choose-pivot array left right)
  (quotient (+ left right) 2))

(define (quicksort-vector! array
                           [left 0]
                           [right (sub1 (vector-length array))]
                           #:< [data-< <])
  (when (< left right)
    (let ([pivotIndex (choose-pivot array left right)])
      (let ([pivotNewIndex (partition! array left right pivotIndex data-<)])
        (quicksort-vector! array left (sub1 pivotNewIndex)  #:< data-<)
        (quicksort-vector! array (add1 pivotNewIndex) right #:< data-<)))))

(define (sorted? v)
  (or (< (vector-length v) 2)
      (for/fold ([sorted? #t])
          ([i (in-vector v 0 (sub1 (vector-length v)))]
           [j (in-vector v 1)])
          (and sorted? (not (< j i))))))

(define-simple-check (check-sorted? thing)
  (sorted? thing))

(define (qsort l [< <])
  (define v (list->vector l))
  (quicksort-vector! v #:< <)
  v)

(define-test-suite quicksort-tests
  (check-sorted? (qsort '()))
  (check-sorted? (qsort '(1)))
  (check-sorted? (qsort '(1 2)))
  (check-sorted? (qsort '(2 2)))
  (check-sorted? (qsort '(9 8 7 6 5 4 3 2 1)))
  (check-sorted? (qsort (build-list 10 (lambda ignored (random)))))
  )

(define-test-suite all-tests
  quicksort-tests)

(provide main)
(define (main . args)
  (exit (run-tests all-tests 'verbose)))
