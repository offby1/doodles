#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module google mzscheme
(require (planet "sxml.ss"      ("lizorkin"    "sxml.plt"))
         (planet "htmlprag.ss"  ("neil"        "htmlprag.plt" ))
         (planet "port.ss"      ("schematics"  "port.plt" ))
         (lib "pretty.ss")
         (lib "url.ss" "net"))
(let ((url (make-url "http"                     ;scheme
                     #f                         ;user
                     "www.google.com"           ;host
                     #f                         ;port
                     #t                         ;path-absolute?
                     (list (make-path/param "search" '())) ;path
                     (list'(q . "money"))                  ;query
                     #f                                    ;fragment
                     )))
  (printf "Url: ~s~%" (url->string url))
  (pretty-display (port->string (get-pure-port
                                 url
                                 (list))))
  )
)