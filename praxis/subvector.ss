#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id: v4-script-template.ss 6086 2009-06-14 20:14:28Z erich $
exec  mzscheme -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang scheme
(require (planet schematics/schemeunit:3)
         (planet schematics/schemeunit:3/text-ui))

(define-struct subvector (v first-index length) #:transparent)
(define (public-make-subvector v first-index length)
  (cond
   ((vector? v)
    (when (> length (vector-length v))
      (error 'public-make-subvector "length ~a is too long for vector ~a" length v))
    (make-subvector v first-index length))
   ((subvector? v)
    (when (> length (subvector-length v))
      (error 'public-make-subvector "length ~a is too long for subvector ~a" length v))
    (make-subvector
     v
     (subvector-v v)
     (+ first-index (subvector-first-index v))
     length
     ))))
(define (public-subvector . values)
  (apply list->subvector values))
(define (list->subvector seq)
  (let ([v (list->vector seq)])
    (make-subvector v 0 (vector-length v))))
(define (subvector->list sv)
  (reverse
   (for/fold ([result '()])
       ([index (in-range (subvector-length sv))])
       (cons (public-subvector-ref sv index)
             result))))
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
    (check-equal? (vector-ref source 1) 'frotz)

    (check-equal? (subvector->list sv) (list 'frotz 2))
    ))

(define (main . args)
  (exit (run-tests subvector-tests 'verbose)))

(provide
 (rename-out
  [public-make-subvector make-subvector]
  [public-subvector-ref  subvector-ref]
  [public-subvector-set! subvector-set!]
  [public-subvector subvector])

 subvector-length
 list->subvector
 subvector->list
 main)
