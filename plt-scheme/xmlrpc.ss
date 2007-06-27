#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module xmlrpc mzscheme
(require (planet "xmlrpc.ss" ("schematics" "xmlrpc.plt" ))
         (planet "ssax.ss" ("lizorkin" "ssax.plt"))
         (planet "sxml.ss" ("lizorkin" "sxml.plt"))
         (lib "pretty.ss"))

(define *flickr-API-key* "d964b85147ddd4082dc029f371fe28a8")

(define flickr (xmlrpc-server "api.flickr.com" 80 "/services/xmlrpc"))
(define flickr.photos.search  (flickr "flickr.photos.search" ))
(define flickr.photos.getInfo (flickr "flickr.photos.getInfo"))

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
    'tags    "orange cat"
    )))

(define cat-photos-sxml
  (ssax:xml->sxml (open-input-string cat-photos-string)
                  '()
                  ))
;;(pretty-display cat-photos-sxml)

(define fp ((sxpath '(photos (photo 1))) cat-photos-sxml))

(define fp-id ((sxpath '(@ id)) fp))

(define first-photo-info
  (flickr.photos.getInfo
   (->ht
    'api_key *flickr-API-key*
    'photo_id  (cadar fp-id))))

(define fpi (ssax:xml->sxml (open-input-string first-photo-info)
                            '()))

(define first-url ((sxpath '(photo urls (url 1))) fpi))

(printf "Look!  A URL for a cat picture: ~a~%"
         (list-ref (car first-url) 2))
)
