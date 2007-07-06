#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace --case-insens -qu "$0" ${1+"$@"}
|#

(module wrapper mzscheme
(require (lib "include.ss"))
(provide (all-defined))
(include/reader "syn-param.scm" (lambda args (read-case-sensitive #f)
                                             (apply read-syntax args)))
(include/reader "foof-loop.scm" (lambda args (read-case-sensitive #f)
                                             (apply read-syntax args)))
)