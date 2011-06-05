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
  (let ()
    (define ch #f)
    (define (stuff! . data)
      (set! ch  (make-async-channel))
      (for-each (curry async-channel-put ch) data))

    (define-simple-check (check-stuff data)
      (apply stuff! data)
      (equal? (sequence->list (channel->seq ch)) (drop-right data 1)))

    (define-simple-check (check-stuff-eq-sentinel data)
      (apply stuff! data)
      (let ([sentinel (last data)])
        (equal?
         (sequence->list
          (channel->seq ch (curry eq? sentinel)))
         (drop-right data 1))))

    (check-stuff (list eof))
    (check-stuff (list 1 2 3 'frotz eof))
    (check-stuff-eq-sentinel (list 1 2 3 'frotz 'plotz))))

(define-test-suite all-tests
  channel->seq-tests)

(provide main)
(define (main . args)
  (exit (run-tests all-tests 'verbose)))
