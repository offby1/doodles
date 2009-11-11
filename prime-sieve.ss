#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec  mzscheme -l errortrace --require "$0" --main -- ${1+"$@"}
|#

;; Inspired by http://golang.org/doc/go_spec.html

#lang scheme
(require (planet schematics/schemeunit:3)
         (planet schematics/schemeunit:3/text-ui))

(define (generator ch)
  (for ([i (in-naturals 2)])
    (channel-put ch i)))

(define (filter num ch)
  (let ([new-ch (make-channel)])
    (thread
     (lambda ()
       (let loop ()
         (let ([candidate (channel-get ch)])
           (when (not (zero? (remainder candidate num)))
             (channel-put new-ch candidate)))
         (loop))))

    new-ch))

(define (main)
  (let ([ch (make-channel)])
    (thread (lambda () (generator ch)))
    (let loop ([ch1 ch])
      (let ([p (channel-get ch1)])
        (printf "~a~%" p)
        (loop (filter p ch1))))))

(provide main)
