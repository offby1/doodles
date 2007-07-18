#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

;; I've already forgotten what this entire file does.

(module google-rss mzscheme
(require (only (lib "uri-codec.ss" "net")
               current-alist-separator-mode)
         (only (planet "sxml.ss"      ("lizorkin"    "sxml.plt"))
               sxpath)
         (only (planet "htmlprag.ss"  ("neil"        "htmlprag.plt" ))
               html->shtml)
         (only (planet "port.ss"      ("schematics"  "port.plt" ))
               port->string)
         (only (lib "pretty.ss")
               pretty-print)
         (only (lib "url.ss" "net")
               make-url
               make-path/param
               get-pure-port
               url->string)
         (only (lib "13.ss" "srfi")
               string-join))

(parameterize ((current-alist-separator-mode 'amp))
              (let* ((url (make-url
                           "http"               ;scheme
                           #f                   ;user
                           "news.google.com"    ;host
                           #f                   ;port
                           #t                   ;path-absolute?
                           (list (make-path/param "news" '())) ;path
                           (list
                            '(q . "lemurs")
                            '(output . "rss")
                            )                          ;query
                           #f                          ;fragment
                           )))

                (write (url->string url))
                (newline)
                (pretty-print ((sxpath '(// item title *text*)) (html->shtml
                                 (port->string (get-pure-port
                                                url
                                                (list))))))
                ))
)
