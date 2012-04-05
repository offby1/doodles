#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec racket -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang racket

(require (only-in srfi/43 vector-swap!))

;; root element is smallest.

(define (left-child index)
  (add1 (* index 2)))

(define (right-child index)
  (add1 (left-child index)))

(define (parent-of index)
  (floor (/ (sub1 index) 2)))

(define (insert-item item heap-vector slots-used)

  (define (out-of-order? item-index parent-index)
    (< (vector-ref heap-vector item-index)
       (vector-ref heap-vector parent-index)))

  ;; stick item into next available slot
  (vector-set! heap-vector slots-used item)

  ;; while this item has a parent (i.e., isn't the first element)

  ;;   p = this item's parent
  ;;   maybe swap item/parent
  ;;   if we didn't swap, exit this loop
  ;;   otherwise item <- p and p <- its own parent

  (let loop ([item-index slots-used]
             [parent-index (parent-of slots-used)])
    (when (positive? item-index)
      (when (out-of-order? item-index parent-index)
        (vector-swap! heap-vector item-index parent-index)
        (loop parent-index (parent-of parent-index)))))
  )

(define (delete-root! heap-vector slots-used)
  (when (zero? slots-used)
    (error 'delete-root! "Can't delete root from empty heap"))

  (set! slots-used  (sub1 slots-used))
  (vector-swap! heap-vector 0 slots-used)

  (let loop ([item-index 0])
    (define index-of-smallest-child-smaller-than-us #f)
    (for ([c (list (left-child item-index)
                   (right-child item-index))])

      (when (and (< c slots-used)
                 (< (vector-ref heap-vector c)
                    (vector-ref heap-vector item-index))
                 (or (not index-of-smallest-child-smaller-than-us)
                     (< (vector-ref heap-vector c)
                        (vector-ref heap-vector index-of-smallest-child-smaller-than-us))))
        (set! index-of-smallest-child-smaller-than-us c)))

    (when index-of-smallest-child-smaller-than-us
      (vector-swap! heap-vector
                    index-of-smallest-child-smaller-than-us
                    item-index)
      (loop index-of-smallest-child-smaller-than-us))))

(define (seq->heap seq)
  (define v (make-vector (length seq) #f))
  (for ([elt seq]
        [index (in-naturals)])
    (insert-item elt v index))
  v)

(define (heapsort-heap! v)
  (let loop ([size (vector-length v)])
    (when (positive? size)
      (delete-root! v size)
      (loop (sub1 size))))

  ;; one more pass to invert the order.
  (for ([index      (in-range 0                        (vector-length v) 1)]
        [reflection (in-range (sub1 (vector-length v)) -1                -1)]
        #:when (< index reflection))
    (vector-swap! v index reflection)))

(define (heapsort-seq seq)
  (define h (seq->heap seq))
  (heapsort-heap! h)
  h)

(define (sorted? lst)
  (cond
   ((null? lst)
    #t)
   ((null? (cdr lst))
    #t)
   (else
    (and (<= (first lst)
             (second lst))
         (sorted? (cdr lst))))))

(random-seed 0)
(provide main)
(define (main . args)
  (for ([log-length (in-range 0 16 2)])
    (let ([l (shuffle (build-list (expt 2 log-length) values))])
      (displayln log-length)
      (when (not (sorted? (vector->list (time (heapsort-seq l)))))
        (error 'oops "I suck"))
      (time (sort l <)))))
