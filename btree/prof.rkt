#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec racket --load "$0"
|#

(require errortrace profile)

(profiling-enabled #t)

(require "btree.rkt")

(define (fill-er-up dict seq)
  (for/fold ([dict dict])
      ([elt seq])
      (dict-set dict elt elt)))

(begin
  (random-seed 0)
  (time (fill-er-up (tree) (shuffle (build-list 1000 values))))
  (output-profile-results #t #f))
