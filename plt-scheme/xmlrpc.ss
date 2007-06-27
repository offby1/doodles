#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module xmlrpc mzscheme
(require (planet "xmlrpc.ss" ("schematics" "xmlrpc.plt" ))
         (planet "ssax.ss" ("lizorkin" "ssax.plt"))
         (planet "sxml.ss" ("lizorkin" "sxml.plt"))
         (lib "pretty.ss")
         (lib "trace.ss")
         (only (lib "url.ss" "net")
               combine-url/relative
               string->url
               url->string)
         "flickr.ss")

(define flickr (xmlrpc-server "api.flickr.com" 80 "/services/xmlrpc"))
(define flickr.photos.search  (flickr "flickr.photos.search" ))
(define flickr.photos.getInfo (flickr "flickr.photos.getInfo"))

;; convert a list of alternating symbols and otherthings into a hash
;; table, with the symbols as the keys and the otherthings as the
;; values.  This hash table is what the various xmlrpc functions want.
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

(define cat-photos-sxml
  (ssax:xml->sxml
   (open-input-string
    (flickr.photos.search
     (->ht
      'api_key *flickr-API-key*
      'tags    "orange cat"
      )))
   '()
   ))

;;(pretty-display cat-photos-sxml)

(define fp-id (cadar ((sxpath '(photos (photo 1) @ id)) cat-photos-sxml)))
(define first-photo-info
  (flickr.photos.getInfo
   (->ht
    'api_key *flickr-API-key*
    'photo_id fp-id)))

(define fpi (ssax:xml->sxml (open-input-string first-photo-info)
                            '()))

(printf "First photo info:~%" )
(pretty-display fpi)

(define (@  attname)
  (list-ref (car ((sxpath `(photo @ ,attname)) fpi)) 1))
(trace @)
;; create the URL for the image itself, as opposed to the fancy flickr
;; page that showcases that image.

(define hacked-up-url
  (url->string
   (combine-url/relative
    (string->url
     (format "http://farm~a.static.flickr.com/" (@ 'farm)))
    (format "~a/~a_~a.jpg" (@ 'server) fp-id (@ 'secret))

    )))
(printf "Hacked-up URL: ~s~%" hacked-up-url)
(define url (list-ref (car ((sxpath '(photo urls (url 1))) fpi))
                      2))

(if (eq? (system-type 'os) 'windows)
    (shell-execute #f hacked-up-url  "" (current-directory) 'sw_shownormal)
  (printf "Look!  A URL for a cat picture: ~a~%" hacked-up-url)
  )
)
