#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id: v4-script-template.ss 6086 2009-06-14 20:14:28Z erich $
exec  mzscheme -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang scheme
(require srfi/1
         (planet schematics/schemeunit:3)
         (planet schematics/schemeunit:3/text-ui))

(define (all-rotations s)
  (let* ([s  (regexp-split #px" +" s)]
         [l (length s)])
    (let loop ([counter (length s)]
               [s (apply circular-list s)]
               [result '()])
      (if (positive? counter)
          (loop
           (sub1 counter)
           (cdr s)
           (cons (take s l) result))
          (reverse result)))))


(define-test-suite all-rotations-tests
  (let ([ s  "This sentence has some words  in it"])
    (check-equal? 7 (length (all-rotations s)))))

(define (main . args)
  (exit (run-tests all-rotations-tests 'verbose)))
(provide all-rotations-tests main)
