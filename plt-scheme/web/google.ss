#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module google mzscheme
(require (only (planet "sxml.ss"      ("lizorkin"    "sxml.plt"))
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
               get-pure-port)
         (only (lib "13.ss" "srfi")
               string-join))

(let* ((url (make-url "http"                    ;scheme
                      #f                        ;user
                      "www.google.com"          ;host
                      #f                        ;port
                      #t                        ;path-absolute?
                      (list (make-path/param "search" '())) ;path
                      (list
                       `(q .
                           ,(string-join (vector->list (current-command-line-arguments)) " ")
                           ))                               ;query
                      #f                                    ;fragment
                      ))
       (result  (html->shtml
                 (port->string (get-pure-port
                                url
                                (list)))))

       (links  ((sxpath '("//div[@class=\"g\"]" h2 a @ href)) result)))

  (pretty-print links)
  )
)