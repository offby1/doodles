#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module get-test mzscheme
(require (lib "url.ss" "net")
         (lib "pretty.ss")
         (planet "htmlprag.ss" ("neil" "htmlprag.plt" )))
(define ip (get-pure-port (string->url "http://localhost/")))

(pretty-display (html->shtml ip))

)