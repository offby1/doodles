#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec  mzscheme --require "$0" --main -- ${1+"$@"}
|#

#lang scheme
(require (planet schematics/schemeunit:3)
         (planet schematics/schemeunit:3/text-ui)
         (planet lizorkin/sxml/sxml)
         net/url
         xml)

(define (query->url-strings query)
  (call/input-url
   (make-url "http" #f "search.yahooapis.com" #f #t
             (for/list ([p '("WebSearchService" "V1" "webSearch")]) (make-path/param p '()))
             `((appid . "QRrlhLPV34Ed3nypZaoUxrZsCa4xOvOArZwAYIyIx1c56rAtHu.xxiStPHapqb8kT79euQQ-")
               (query . ,query)
               )
             #f)
   get-pure-port
   (lambda (ip)
     (for/list ([url (in-list ((sxpath '(// Result Url *text*)) (xml->xexpr (document-element (read-xml ip)))))])
       url))))

(define (main . args)
  (for ([arg args])
    (printf "~s:~%" arg)
    (for ([url (query->url-strings arg)])
      (printf "~a~%" url))))

(provide (all-defined-out))
