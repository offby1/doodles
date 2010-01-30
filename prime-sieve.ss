#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec  mzscheme -l errortrace --require "$0" --main -- ${1+"$@"}
|#

;; Fairly direct translation of the example at
;; http://golang.org/doc/go_spec.html

#lang scheme

;; Crude copy of the "go" language's "go" statement.
(define-syntax-rule (go body ...) (begin (thread (lambda () body ...)) (void)))

;; Send the sequence 2, 3, 4, ... to channel 'ch'.
(define (generate ch)
  (for ([i (in-naturals 2)])
    (channel-put ch i)))

;; Copy the values from channel 'src' to 'dst', removing those
;; divisible by 'prime'.
(define (filter src dst prime)
  (let loop ()
    (let ([i (channel-get src)])
      (when (not (zero? (remainder i prime)))
        (channel-put dst i)))
    (loop)))

;; The prime sieve: Daisy-chain filter processes together.
(define (main)
  (let ([ch (make-channel)])
    (go (generate ch))
    (let loop ([ch ch])
      (let ([ch1 (make-channel)]
            [prime (channel-get ch)])
        (printf "~a~%" prime)
        (go (filter ch ch1 prime))
        (loop ch1)))))

(provide main)
