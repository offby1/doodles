#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec racket -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang racket

;; http://en.wikipedia.org/wiki/Partition_(number_theory)

(require rackunit
         rackunit/text-ui
         racket/trace)

(define vi vector-immutable)

(define (counting-number/c thing)
  (and/c (natural-number/c thing)
         (positive? thing)))

(define numbers/c (vectorof counting-number/c #:immutable #t #:flat? #t))

(define (increasing? seq)
  (if (< (vector-length seq) 2)
      #f
      (not (apply >= (vector->list seq)))))

(define (partition/c thing)
  (and/c (numbers/c thing)
         (not/c (increasing? thing))))

;; Return a sequence that is like SEQUENCE, except the INDEXth number
;; is one bigger.  As a special case, if INDEX is the size of
;; SEQUENCE, then we append a 1.
(define/contract (increment-at sequence index)
  (numbers/c natural-number/c . -> . numbers/c)
  (vector->immutable-vector
   (if (= index (vector-length sequence))
       (vector-append sequence (vi 1))
       (for/vector
        #:length (vector-length sequence)
        ([(elt i) (in-indexed sequence)])
        (if (= i index)
            (add1 elt)
            elt)))))

(define/contract (more-partitions p)
  (partition/c . -> . (set/c partition/c))
  (define result (set))

  (set! result (set-union result (set (increment-at p 0))))

  (for ([index (in-range 1 (vector-length p))])
    (when (> (vector-ref p (sub1 index))
             (vector-ref p index))
      (set! result (set-union result (set (increment-at p index))))))

  (set! result (set-union result (set (increment-at p (vector-length p)))))
  result)

(define/contract (all-partitions n)
  (counting-number/c . -> . (set/c partition/c))
  (if (= n 1)
      (set (vi 1))
      (apply set-union
             (set-map
              (all-partitions (sub1 n))
              (lambda (p)
                (more-partitions p))))))


(define-test-suite all-tests
  (check-equal? (increment-at (vi 1) 0) (vi 2))
  (check-equal? (increment-at (vi 1) 1) (vi 1 1))
  (check-equal? (increment-at (vi 1 2) 1) (vi 1 3))

  (check-equal? (all-partitions 1) (set (vi 1)))

  (check-equal? (all-partitions 2) (set (vi 1 1) (vi 2)))

  (for ([(expected index) (in-indexed '[1 1 2 3 5 7 11 15 22 30 42])])
    (when (positive? index)
      (check-equal? (set-count (all-partitions index)) expected))))

(provide main)
(define (main . args)
  (exit (run-tests all-tests 'verbose)))
