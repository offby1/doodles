#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec racket -l errortrace --require "$0" --main -- ${1+"$@"}
|#

;; http://programmingpraxis.com/2010/11/16/rsa-cryptography/ but of
;; course also http://en.wikipedia.org/wiki/Rsa

#lang racket
(require rackunit rackunit/text-ui
         (planet soegaard/math/math)
         srfi/27)

;; This indirectly affects big-random.  Feh.
(random-source-randomize! default-random-source)

(define (random-prime [bit-length 10])

  (let* ([min (expt 2 (sub1 bit-length))]
         [r (+ min (big-random min))])
    (next-prime r)))

(define p (random-prime))
(define q (random-prime))
(define n (* p q))
(define φ (* (sub1 p) (sub1 q)))
(define e 65537)
(define d (inverse e φ))

(printf "Public key: ~a~%" (cons n e))
(printf "Private key: ~a~%" d)
