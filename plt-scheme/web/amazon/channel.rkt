#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec racket -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang racket
(require
 racket/async-channel
 (only-in racket/generator
          in-generator
          yield)
 rackunit
 rackunit/text-ui
 )

(provide channel->seq)
(define/contract (channel->seq ch [sentinel? eof-object?])
  ((async-channel?)  ((any/c . -> . boolean?)) . ->* . sequence?)
  (in-generator
   (let loop ()
     (let ([datum (async-channel-get ch)])
       (when (not (sentinel? datum))
         (yield datum)
         (loop))))))

(define-test-suite channel->seq-tests
  (let ([ch (make-async-channel)])
    (async-channel-put ch eof)
    (check-equal? (sequence->list (channel->seq ch)) '()))

  (let ([ch (make-async-channel)])
    (for ([datum (list 1 2 3 'frotz eof)])
      (async-channel-put ch datum))
    (check-equal?
     (sequence->list (channel->seq ch))
     '(1 2 3 frotz)))

  (let ([ch (make-async-channel)])
    (for ([datum (list 1 2 3 'frotz 'plotz)])
      (async-channel-put ch datum))
    (check-equal?
     (sequence->list (channel->seq ch (curry eq? 'plotz)))
     '(1 2 3 frotz)))  )

(define-test-suite all-tests
  channel->seq-tests)

(provide main)
(define (main . args)
  (exit (run-tests all-tests 'verbose)))
