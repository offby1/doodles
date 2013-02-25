#lang racket

(require (planet clements/sxml2:1:=3)
         racket/pretty)

(provide text-transformer)
(define (text-transformer tag-symbol text-transform-function)
    `((,tag-symbol . ,(lambda (tag . elements)
                        (cons tag (map
                                   (lambda (elt)
                                     (match elt
                                       [(list '*text* (? string? str))
                                        (list '*text* (text-transform-function str))]
                                       [#t elt])
                                     )
                                   elements))))
      (*default* . ,(lambda args args))))

(module+ main
  (define sample-doc
    (call-with-input-file "sample-rss-feed.xml"
      (curryr ssax:xml->sxml '((x . "http://purl.org/rss/1.0/")))))

  (define (string-reverse str)
    (list->string (reverse (string->list str))))

  (pretty-print
   (pre-post-order sample-doc (text-transformer 'x:link string-reverse))))
