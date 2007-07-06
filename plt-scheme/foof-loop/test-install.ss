#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module test-install mzscheme
(require  (planet "foof-loop.ss" ("offby1" "foof-loop.plt")))
(loop ((for element (in-list (list 1 2 3 4))))
      (write element)
      (newline))
)