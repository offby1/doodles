#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module run-bot mzscheme
(require
 (rename (lib "19.ss" "srfi") 19:make-date make-date)

 )

)