#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec  mzscheme -l errortrace --require "$0" --main -- ${1+"$@"}
|#

;; Fairly direct translation of the example at
;; http://golang.org/doc/go_spec.html

#lang scheme

;; Crude copy of the "go" language's "go" statement.
(define-syntax-rule (go thing) (begin (thread (lambda () thing)) (void)))

;; Send the sequence 2, 3, 4, ... to channel 'ch'.
(define (generate ch)
  (for ([i (in-naturals 2)])
    (channel-put ch i)))

;; Copy the values from channel 'in' to 'out', removing those
;; divisible by 'prime'.
(define (filter prime in out)
  (let loop ()
    (let ([candidate (channel-get in)])
      (when (not (zero? (remainder candidate prime)))
        (channel-put out candidate)))
    (loop)))

;; The prime sieve: Daisy-chain filter processes together.
(define (main)
  (let ([ch (make-channel)])
    (go (generate ch))
    (let loop ([ch ch])
      (let ([ch1 (make-channel)]
            [p (channel-get ch)])
        (printf "~a~%" p)
        (go (filter p ch ch1))
        (loop ch1)))))

(provide main)
