#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec mzscheme -qr "$0" ${1+"$@"}
|#

(require "mebs.scm")
(printf "~a~%" (modular-exponent-by-successive-squares 2 23 5))
