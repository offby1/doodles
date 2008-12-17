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

(define (query->xml-input-port query)
  (call/input-url
   (make-url "http" #f "search.yahooapis.com" #f #t (list (make-path/param "WebSearchService" '())
                                                          (make-path/param "V1" '())
                                                          (make-path/param "webSearch" '()))
             `((appid . "QRrlhLPV34Ed3nypZaoUxrZsCa4xOvOArZwAYIyIx1c56rAtHu.xxiStPHapqb8kT79euQQ-")
               (query . ,query)
               )
             #f)
   get-pure-port
   (lambda (ip)
     (for ((url (in-list ((sxpath '(// Result Url *text*)) (xml->xexpr (document-element (read-xml ip)))))))
       (display url)
       (newline)))))

(define (main . args)
  (query->xml-input-port "kitty cats"))

(provide (all-defined-out))
