#lang racket

(require (planet clements/sxml2:1:=3)
         racket/pretty)

(provide modify-RDF-item-links)
(define (modify-RDF-item-links doc xform)
  (for/list ([thing doc])
    (match thing
      [(list 'rdf:RDF rdf-things ...)
       (cons 'rdf:RDF
             (for/list ([thing rdf-things])
               (match thing
                 [(list 'x:item items ...)
                  (cons 'x:item
                        (for/list ([item items])
                          (match item
                            [(list 'x:link link) (list 'x:link (xform link))]
                            [_ item])))
                  ]
                 [_ thing])))]

      [_ thing])))

(module+ main

  (define (string-reverse s)
    (list->string (reverse (string->list s))))

  (define sample-doc
    (call-with-input-file "sample-rss-feed.xml"
      (curryr ssax:xml->sxml '(
                               (x . "http://purl.org/rss/1.0/")
                               (rdf . "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
                               ))))

  (pretty-print
   (modify-RDF-item-links sample-doc string-reverse)))
