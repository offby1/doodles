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
         (lib "sendurl.ss" "net")
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

(define (parse-xml string)
  (ssax:xml->sxml (open-input-string string) '()))

(define cat-photos-sxml
  (parse-xml
   (flickr.photos.search
    (->ht
     'api_key *flickr-API-key*
     'tags    "fat,lazy,orange,cat"
     'tag_mode "all"
     'sort "interestingness-desc"
     ))))

(define *num-photos-returned* (list-ref (car ((sxpath '(photos @ total)) cat-photos-sxml)) 1))

(if (string=? "0" *num-photos-returned*)
    (printf "Uh oh, no photos returned: ~a~%" cat-photos-sxml)
  (let ()
    (define (attribute-getter-from-sxml sxml path)
      (lambda (attname)
        (list-ref (car ((sxpath `(,@path @ ,attname)) sxml)) 1)))

    (define @ (attribute-getter-from-sxml cat-photos-sxml '(photos photo)))

    (define fp-id (@ 'id))

    (define first-photo-info
      (flickr.photos.getInfo
       (->ht
        'api_key *flickr-API-key*
        'photo_id fp-id)))

    (define fpi (parse-xml first-photo-info))

    (printf "First photo info:~%" )
    (pretty-display fpi)

    (let (( @ (attribute-getter-from-sxml fpi '(photo)))


          (url (list-ref (car ((sxpath '(photo urls (url 1))) fpi))
                         2))

          ;; the URL for the image itself, as opposed to the fancy
          ;; flickr page that showcases that image.
          (url-for-bare-image
           (url->string
            (combine-url/relative
             (string->url
              (format "http://farm~a.static.flickr.com/" (@ 'farm)))
             (format "~a/~a_~a.jpg" (@ 'server) fp-id (@ 'secret))

             ))))

      (printf "URL for the unadorned image: ~s~%" url-for-bare-image)

      (if (eq? (system-type 'os) 'windows)
          (shell-execute #f url  "" (current-directory) 'sw_shownormal)
        (parameterize ((external-browser '("remote-browse.sh " . "")))
                      (send-url url))
        ))
    ))
)
