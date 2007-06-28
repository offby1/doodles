#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module flickr mzscheme
(require (planet "xmlrpc.ss" ("schematics" "xmlrpc.plt" ))
         (planet "ssax.ss"   ("lizorkin"   "ssax.plt"))
         (planet "sxml.ss"   ("lizorkin"   "sxml.plt"))
         )
(provide
 flickr.photos.search
 flickr.photos.getInfo
 )

(define *flickr-API-key* "d964b85147ddd4082dc029f371fe28a8")
(define flickr (xmlrpc-server "api.flickr.com" 80 "/services/xmlrpc"))

(define flickr.photos.search (lambda keys-n-values
                               (parse-xml
                                ((flickr "flickr.photos.search" )

                                 ;; this is Bad.  If keys-n-values
                                 ;; _already_ contains 'api_key, then
                                 ;; it's not clear which value will
                                 ;; ultimately get used -- the one in
                                 ;; keys-n-values, or the
                                 ;; *flickr-API-key* that I'm
                                 ;; providing here.  I should first
                                 ;; ensure that api_key doesn't appear
                                 ;; in keys-n-values, and croak if it
                                 ;; does.
                                 (apply ->ht
                                   'api_key  *flickr-API-key*  keys-n-values)))
                               ))

(define flickr.photos.getInfo (lambda keys-n-values
                                (parse-xml
                                 ((flickr "flickr.photos.getInfo")
                                  (apply ->ht
                                   'api_key *flickr-API-key*  keys-n-values)))))

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
)