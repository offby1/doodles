#!/usr/local/bin/scsh \
-o scheme-with-scsh -o tables -o primitives -o srfi-23 -o srfi-1 -o sort -l bag.scm -l dict.scm -e main -s
!#

(define (main args)
  (define b (bag "cat dog"))
  (define t (snarf-dictionary b))
  (display t)
  (newline)
  )
