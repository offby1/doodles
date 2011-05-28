#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec racket -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang racket
(require racket/generator
         rackunit
         rackunit/text-ui)

(provide group)
(define/contract (group seq group-size)
  (sequence? natural-number/c . -> . sequence?)
  (in-generator
   (let ([this-chunk '()])
     (for ([(elt i) (in-indexed seq)])
       (set! this-chunk (cons elt this-chunk))
       (when (zero? (remainder (length this-chunk) group-size))
         (yield (reverse this-chunk))
         (set! this-chunk '())))
     (when (not (null? this-chunk))
       (yield this-chunk)))))

(define-test-suite group-tests
  (check-equal? (for/list ([i  (group (list 1 2 3 4) 1)])
                  i)
                '((1)
                  (2)
                  (3)
                  (4)))
  (check-equal? (for/list ([i  (group (list 1 2 3 4) 2)])
                  i)
                (list
                 (list 1 2)
                 (list 3 4)))
  (check-equal? (for/list ([i  (group (list 1 2 3 4) 3)])
                  i)
                (list
                 (list 1 2 3)
                 (list 4))))

(define-test-suite all-tests
  group-tests)

(provide main)
(define (main . args)
  (when (positive? (run-tests all-tests 'verbose))
    (exit 1))
  (for/list ([i  (group (in-range 0 100) 17)])
    i))
