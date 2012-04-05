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
         (only-in (planet lizorkin/ssax/ssax) ssax:xml->sxml))

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
     ((sxpath '(// urn:yahoo:srch:Result
                   urn:yahoo:srch:Url *text*))
      (ssax:xml->sxml ip '())))))

(define (main . args)
  (for ([arg args])
    (printf "~s:~%" arg)
    (for ([url (query->url-strings arg)])
      (printf "~a~%" url))))

(provide (all-defined-out))
