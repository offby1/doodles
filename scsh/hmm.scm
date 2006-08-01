#!/usr/local/bin/scsh \
-lel dir-fold/load.scm -o dir-fold -s
!#

(define (dnl thing)
  (display thing)
  (newline))

(let ((arg (if (null? command-line-arguments)
               "."
             (car command-line-arguments))))
  (for-each dnl (pruning-tree-fold cons arg '())))

