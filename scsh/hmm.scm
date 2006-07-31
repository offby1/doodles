#!/usr/local/bin/scsh \
-lel exceptions/load.scm -o srfi-34 -s
!#

(guard (condition
        (else
         (display "condition: ")
         (write condition)
         (newline)
         'exception))
       (+ 1 (raise 'an-error)))
