#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id: v4-script-template.ss 6086 2009-06-14 20:14:28Z erich $
exec  mzscheme -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang scheme
(require (planet schematics/schemeunit:3)
         (planet schematics/schemeunit:3/text-ui))

(define-struct subvector (v first-index length) #:transparent)
(define public-make-subvector make-subvector)
(define (public-subvector-ref sv index)
  (vector-ref (subvector-v sv) (+ index (subvector-first-index sv))))
(define (public-subvector-set! sv index value)
  (vector-set! (subvector-v sv) (+ index (subvector-first-index sv)) value))
(define-test-suite subvector-tests

  (let* ((source (vector 0 1 2 3))
         (sv (public-make-subvector source 1 2)))
    (check-equal? (subvector-length sv) 2)
    (check-equal? (public-subvector-ref sv 0) 1)
    (check-equal? (public-subvector-ref sv 1) 2)

    (public-subvector-set! sv 0 'frotz)
    (check-equal? (public-subvector-ref sv 0) 'frotz)
    (check-equal? (vector-ref source 1) 'frotz)))

(define (main . args)
  (exit (run-tests subvector-tests 'verbose)))

(provide
 (rename-out
  [public-make-subvector make-subvector]
  [public-subvector-ref  subvector-ref]
  [public-subvector-set! subvector-set!])
 subvector-length
 main)
