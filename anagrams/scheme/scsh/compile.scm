#!/usr/local/bin/scshvm \
-o scshvm -h 28000000 -i /usr/lib/scsh-0.6/scsh.image -o scheme-with-scsh -o tables -o primitives -o srfi-23 -o srfi-1 -o sort -l bag.scm -l dict.scm -l anagrams.scm -e compile -s
!#

(define (compile args)
  (snarf-dictionary)
  (dump-scsh-program main "anagrams.scshvm"))

