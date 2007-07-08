#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module flickr mzscheme
(require (planet "xmlrpc.ss" ("schematics" "xmlrpc.plt" ))
         (all-except (planet "ssax.ss"   ("lizorkin"   "ssax.plt")) assert)
         (lib "assert.ss" "offby1"))
(provide
 flickr.photos.search
 flickr.photos.getInfo
 flickr.people.findByUsername
 )

(define *flickr-API-key* "d964b85147ddd4082dc029f371fe28a8")
(define flickr (xmlrpc-server "api.flickr.com" 80 "/services/xmlrpc"))

(define-syntax (define-flickr-api stx)
  (syntax-case stx ()
    ((_ n)
     (syntax
      (define n
        (lambda keys-n-values
          (assert (not (memq 'api_key keys-n-values)))
          (parse-xml
           (
            (flickr (symbol->string (syntax-object->datum (syntax n))))
            (apply ->ht
                   'api_key *flickr-API-key*  keys-n-values)))))))))

(define-flickr-api flickr.photos.search)
(define-flickr-api flickr.photos.getInfo)
(define-flickr-api flickr.people.findByUsername)

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