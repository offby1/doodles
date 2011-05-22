#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec racket -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang racket
(require rackunit rackunit/text-ui)
(require racket/async-channel)

(define *num-to-produce* 10)

(define (consume q)
  (let loop ()
    (let ([datum (async-channel-get q)])
      (cond
       ((eof-object? datum)
        (printf "Consumer is done~%"))
       (else
        (begin
          (printf "                consumed ~a~%" datum)
          (sleep (random))
          (loop)))))))

(define (produce q)
  (let ([data (build-list *num-to-produce* (lambda (_) (random 100)))])
    (for ([datum data])
      (printf "Produced ~a~%" datum)
      (async-channel-put q datum))
    (async-channel-put q eof)
    (printf "Producer is done~%")))


(provide main)
(define (main . args)
  (define q (make-async-channel (/ *num-to-produce* 2)))
  (let ([p (thread (lambda ()
            (produce q)))]
        [c (thread (lambda ()
            (consume q)))])
    (for ([t (list p c)])
      (sync t))))
