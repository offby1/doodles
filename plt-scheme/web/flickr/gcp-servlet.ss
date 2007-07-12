#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module gcp-servlet mzscheme
(require (only  (lib "url.ss" "net")
                string->url
                url->string
                set-url-query!)
         (lib "servlet.ss" "web-server")
         (file "/home/erich/doodles/plt-scheme/web/flickr/get-cat-pictures.ss"))

(provide interface-version timeout start)

(define interface-version 'v1)
(define timeout +inf.0)
(define (start initial-request)
  `(html (body (p "Not much yet.")
               (a ((href "http://photo.net")) "a link")
               (img ((src ,(url-for-one-interesting-cat-photo)))))))
)