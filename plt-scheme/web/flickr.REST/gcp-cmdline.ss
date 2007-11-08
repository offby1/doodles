#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module gcp-cmdline mzscheme
(require "get-cat-pictures.ss"
         (only "flickr.ss" *verbose*))
(*verbose* #t)
(printf "Here's the URL to a cat picture: ~s~%"
        (url-for-one-interesting-cat-photo "ugly"))
)