#!/usr/bin/guile -s
!#
(use-modules (ice-9 getopt-gnu-style))

(begin
  (display "Parsed options: ")
  (write (getopt-gnu-style (cdr (command-line))))
  (newline))
