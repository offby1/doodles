#!/bin/sh
exec guile -L . --debug -s $0 ${1+"$@"}
!#

(use-modules (anagrams))
(display (all-anagrams (cadr (command-line))))
(newline)