#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module xmlrpc mzscheme
(require (planet "xmlrpc.ss" ("schematics" "xmlrpc.plt" ))
         (planet "ssax.ss" ("lizorkin" "ssax.plt"))
         (lib "pretty.ss"))

(define *flickr-API-key* "d964b85147ddd4082dc029f371fe28a8")

(define flickr (xmlrpc-server "api.flickr.com" 80 "/services/xmlrpc"))
(define flickr.photos.search (flickr "flickr.photos.search"))

;; convert a list of alternating symbols and otherthings into a hash
;; table, with the symbols as the keys.
(define (->ht . args)
  (let loop ((args args)
             (conses '()))
    (if (null? args)
        (let ((rv (make-hash-table)))
          (for-each (lambda (p)
                      (hash-table-put!
                       rv
                       (car p)
                       (cdr p)))
                    conses)
          rv)
      (if (null? (cdr args))
          (error "->ht called with an odd number of arguments")
        (let ((key (car args))
              (value (cadr args)))
          (loop (cddr args)
                (cons (cons key value)
                      conses))))
      )))

(define cat-photos-string
  (flickr.photos.search
   (->ht
    'api_key *flickr-API-key*
    'tags    "cats"
    )))

(define cat-photos-sxml
  (ssax:xml->sxml (open-input-string cat-photos-string)
                  '()
                  ))
(define first-photo
  (list-ref (list-ref cat-photos-sxml 1) 2))
(pretty-display first-photo)
)
