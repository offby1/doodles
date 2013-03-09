#lang racket

(require (planet clements/sxml2:1:=3)
         racket/pretty)

(provide modify-RDF-item-links)
(define (modify-RDF-item-links doc xform)
  (for/list ([thing doc])
    (match thing
      [(list 'xmlns:rdf:RDF rdf-things ...)
       (cons 'xmlns:rdf:RDF
             (for/list ([thing rdf-things])
               (match thing
                 [(list 'xmlns:item items ...)
                  (cons 'xmlns:item
                        (for/list ([item items])
                          (match item
                            [(list 'xmlns:link link) (list 'xmlns:link (xform link))]
                            [_ item])))
                  ]
                 [_ thing])))]

      [_ thing])))
