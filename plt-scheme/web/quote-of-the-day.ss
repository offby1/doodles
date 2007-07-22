#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module quote-of-the-day mzscheme
(require (only (lib "uri-codec.ss" "net")
               current-alist-separator-mode)
         (only (planet "sxml.ss"      ("lizorkin"    "sxml.plt"))
               sxpath)
         (only (planet "htmlprag.ss"  ("neil"        "htmlprag.plt" ))
               html->shtml)
         (only (planet "port.ss"      ("schematics"  "port.plt" ))
               port->string)
         (only (lib "pretty.ss")
               pretty-display
               pretty-print)
         (only (lib "url.ss" "net")
               make-url
               make-path/param
               get-pure-port
               url->string)
         (only (lib "13.ss" "srfi")
               string-join)
         (only (lib "1.ss" "srfi")
               append-map))

(parameterize ((current-alist-separator-mode 'amp))
              (let* ((url (make-url
                           "http"               ;scheme
                           #f                   ;user
                           "feeds.feedburner.com"    ;host
                           #f                   ;port
                           #t                   ;path-absolute?
                           (list (make-path/param "quotationspage" '())
                                 (make-path/param "qotd" '())) ;path
                           '()                         ;query
                           #f                          ;fragment
                           )))

                (write (url->string url))
                (newline)
                (pretty-display
                 (html->shtml
                  (port->string (get-pure-port
                                 url
                                 (list))))

                 ;;  (append-map (lambda (str)
                 ;;                                ((sxpath '(// *text*))
                 ;;                                 (html->shtml str)))
                 ;;                              ((sxpath '(// item description *text*))
                 ;;                               (html->shtml
                 ;;                                (port->string (get-pure-port
                 ;;                                               url
                 ;;                                               (list))))))
                 )))
)
