#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec racket -l errortrace --require "$0" --main -- ${1+"$@"}
|#

;; http://programmingpraxis.com/2011/02/25/sieve-of-euler/

#lang racket
(define (next seq)
  (let ([top (apply set seq)]
        [bot (apply set (map (curry * (car seq)) seq))])
    (set-map (set-subtract top bot) values)))

(provide main)
(define (main . args)
  (let loop ([seq (next (build-list 30 (curry + 2)))])
    (displayln seq)
    (when (not (null? (cdr seq)))
      (loop  (next (cdr seq))))))

