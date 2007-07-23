#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module planet-emacsen mzscheme
(require (lib "trace.ss")
         (only (lib "uri-codec.ss" "net")
               current-alist-separator-mode)

         (only (planet "htmlprag.ss"  ("neil"        "htmlprag.plt" ))
               html->shtml)
         (only (planet "port.ss"      ("schematics"  "port.plt" ))
               port->string)

         (only (lib "url.ss" "net")
               get-pure-port
               string->url))

(provide planet-emacsen-news)
(define (trim str)
  (regexp-replace*
   (pregexp "(\r|\n)+")
   str
   ""))
;;(trace trim)
(define *the-url* (string->url "http://planet.emacsen.org/atom.xml"))
(define (planet-emacsen-news)
  (parameterize ((current-alist-separator-mode 'amp))
                (html->shtml
                 (port->string (get-pure-port
                                *the-url*
                                (list))))))
)
