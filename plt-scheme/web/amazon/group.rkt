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
   (let-values ([(l final-chunk)
                 (for/fold ([l 0] [this-chunk null])
                     ([(elt i) (in-indexed seq)])
                     (cond [(= l group-size)
                            (yield (reverse this-chunk))
                            (values 1 (list elt))]
                           [else (values (add1 l) (cons elt this-chunk))]))])
     (when (not (null? final-chunk))
       (yield (reverse final-chunk))))))

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
