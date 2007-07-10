#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module del mzscheme
(require (planet "delicious.ss" ("untyped" "delicious.plt" ))
         (only (lib "19.ss" "srfi") date->string))
(current-username "tucumcari")
(current-password (vector-ref (current-command-line-arguments) 0))
(printf "~a's bookmarks were last updated on ~a~%"
        (current-username)
        (date->string (last-updated) "~Y-~m-~dT~X~z"))
)