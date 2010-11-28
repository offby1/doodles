#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec racket -l errortrace --require "$0" --main -- ${1+"$@"}
|#

;; http://en.wikipedia.org/wiki/Multiplicative_group_of_integers_modulo_n
;; http://en.wikipedia.org/wiki/Cyclic_group

#lang racket
(require (only-in (planet soegaard/math/math) with-modulus))

(define (powers-of g modulus)
  (with-modulus
   modulus
   (let loop ([last (+ g 0)]
              [result '()]
              [emergency-brake modulus])
     (cond
      ((zero? emergency-brake)
       'uh-oh)
      ((< 1 last)
       (loop (* last g)
             (cons last result)
             (sub1 emergency-brake)))
      (else
       (cons 1 (reverse result)))))))

(provide main)
(define (main . args)
  (powers-of 23 47))
