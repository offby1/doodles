#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module flickr mzscheme
(provide (all-defined))

(define *flickr-API-key* "d964b85147ddd4082dc029f371fe28a8")
)