#!/bin/sh
exec guile --debug -s $0 ${1+"$@"}
!#

(use-modules (anagrams))
(all-anagrams (cadr (command-line)))
