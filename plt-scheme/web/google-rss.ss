#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

;; Display a few current news headlines about lemurs, just to prove we
;; can mess with RSS.

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
         (lib "url.ss" "net")
         (only (lib "13.ss" "srfi")
               string-join))

(parameterize ((current-alist-separator-mode 'amp))
              (let ((url (string->url "http://news.google.com/news")))
                (set-url-query! url (list (cons 'q "lemurs")
                                          (cons 'output "rss")))
                (write (url->string url))
                (newline)
                (pretty-print
                 ((sxpath '(// item title *text*))
                  (html->shtml
                   (port->string (get-pure-port
                                  url
                                  (list))))))))
)
