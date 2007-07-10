#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace --case-insens -qu "$0" ${1+"$@"}
|#

(module foof-loop mzscheme
(require (lib "include.ss")
         (lib "8.ss" "srfi"))

(include/reader "syn-param" (lambda args (read-case-sensitive #f)
                                             (apply read-syntax args)))
(include/reader "foof-loop" (lambda args (read-case-sensitive #f)
                                             (apply read-syntax args)))

;; Riastradh says it's bad to (provide (all-defined)), and that
;; instead we should provide only those things which are documented.
(provide
 appending
 appending-reverse
 down-from
 in-list
 in-lists
 in-port
 in-string
 in-string-reverse
 in-vector
 in-vector-reverse
 listing
 listing!
 listing-into!
 listing-reverse
 loop
 loop-clause-error
 maximizing
 minimizing
 multiplying
 summing
 syntactic-error
 up-from
 )
)