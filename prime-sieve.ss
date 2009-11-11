#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec  mzscheme -l errortrace --require "$0" --main -- ${1+"$@"}
|#

;; Inspired by http://golang.org/doc/go_spec.html

#lang scheme
(require (planet schematics/schemeunit:3)
         (planet schematics/schemeunit:3/text-ui))

;; Send the sequence 2, 3, 4, ... to channel 'ch'.
(define (generator ch)
  (for ([i (in-naturals 2)])
    (channel-put ch i)))

;; Copy the values from channel 'in' to a new channel, removing those
;; divisible by 'prime'.  Return the new channel.
(define (filter prime in)
  (let ([new-ch (make-channel)])
    (thread
     (lambda ()
       (let loop ()
         (let ([candidate (channel-get in)])
           (when (not (zero? (remainder candidate prime)))
             (channel-put new-ch candidate)))
         (loop))))

    new-ch))

;; The prime sieve: Daisy-chain filter processes together.
(define (main)
  (let ([ch (make-channel)])
    (thread (lambda () (generator ch)))
    (let loop ([ch1 ch])
      (let ([p (channel-get ch1)])
        (printf "~a~%" p)
        (loop (filter p ch1))))))

(provide main)
