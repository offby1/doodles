#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module run-tests mzscheme
(require-for-syntax (lib "8.ss" "srfi"))
(require (lib "include.ss")
         (lib "8.ss" "srfi")
         (lib "9.ss" "srfi")
         "foof-loop.ss")
(include/reader "test" (lambda args (read-case-sensitive #f)
                                   (apply read-syntax args)))

(include/reader "test-foof-loop" (lambda args (read-case-sensitive #f)
                                             (apply read-syntax args)))
(include/reader "test-parameters" (lambda args (read-case-sensitive #f)
                                               (apply read-syntax args)))


(run-test-suite loop-tests)
)