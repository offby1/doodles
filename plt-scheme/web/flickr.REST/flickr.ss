#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

;; docs are at http://www.flickr.com/services/api/

(module flickr mzscheme
(require (lib "pretty.ss")
         (lib "url.ss" "net")
         (planet "xmlrpc.ss" ("schematics" "xmlrpc.plt" ))
         (all-except (planet "ssax.ss"   ("lizorkin"   "ssax.plt")) assert fold)
         (lib "trace.ss")
         (only (lib "1.ss" "srfi") fold)
         (only (lib "list.ss") sort)
         (lib "md5.ss")
         (planet "assert.ss" ("offby1" "offby1.plt")))
(provide
 flickr.auth.getFrob
 flickr.auth.checkToken
 flickr.auth.getToken
 flickr.photos.search
 flickr.photos.addTags
 flickr.photos.getExif
 flickr.photos.getInfo
 flickr.photos.getSizes
 flickr.people.findByUsername
 flickr.contacts.getPublicList
 flickr.people.getInfo
 REST-call
 get-login-url
 get-timings
 *verbose*)

(define *verbose* (make-parameter #f))
(define *flickr-API-key* "d964b85147ddd4082dc029f371fe28a8")
(define *flickr-API-secret* (or (getenv "FLICKR_SECRET") ""))
(define flickr (xmlrpc-server "api.flickr.com" 80 "/services/xmlrpc"))

(define *timings* (make-hash-table 'equal))

(define (get-timings)
  (let ((alist '()))
    (hash-table-for-each
     *timings*
     (lambda (k v)
       (set! alist (cons (cons k v) alist))))
  alist))

(define (hash-table-increment! ht key val)
  (hash-table-put! ht key (+ val (hash-table-get ht key 0))))

(define (sign-args arglist)
  (bytes->string/utf-8
   (md5
    (string->bytes/utf-8
     (apply
      string-append
      *flickr-API-secret*
      (map (lambda (p)
             (format "~a~a" (car p) (cdr p)))
           (sort

            (let loop ((arglist arglist)
                       (pairs '()))
              (if (null? arglist)
                  pairs
                  (loop (cddr arglist)
                        (cons (cons (car arglist)
                                    (cadr arglist))
                              pairs))))

            (lambda (p1 p2)
              (string<? (symbol->string (car p1))
                        (symbol->string (car p2)))))))))))
(trace sign-args)

(define (get-login-url frob perms)
  (format "http://flickr.com/services/auth/?api_key=~a&perms=~a&frob=~a&api_sig=~a"
          *flickr-API-key*
          perms
          frob
          (sign-args
           (list
            'api_key *flickr-API-key*
            'frob frob
            'perms perms))
          ))

(define (REST-call method-name . keys-n-values)
  (define REST-endpoint-URL "http://api.flickr.com/services/rest/")

  (check-type 'REST-call symbol? method-name)
  (check-type 'REST-call list? keys-n-values)

  (assert (not (memq 'api_key keys-n-values)))
  (assert (not (memq 'api_sig keys-n-values)))
  (assert (not (memq 'format  keys-n-values)))

  (let* ((most-args-list
          (append
           (list 'api_key *flickr-API-key*)
;;            (list 'format  "xmlrpc")
           keys-n-values))
         (all-args-list
          (if (positive? (string-length *flickr-API-secret*))
              (append
               (list 'api_sig (sign-args most-args-list))
               most-args-list)
              most-args-list))
         (as-conses
          (map (lambda (p)
                 (cons (car p)
                       (format "~a" (cdr p))))
               (hash-table-map (apply ->ht all-args-list) cons))))

    (fprintf (current-error-port)
             "most-args-list: ~s; all-args-list: ~s; as-conses: ~s~%"
             most-args-list all-args-list as-conses)

    (let ((url (string->url REST-endpoint-URL)))
      (set-url-query! url `((method . ,(symbol->string method-name))
                            ,@as-conses))
      (fprintf (current-error-port)
               "Calling ~s with ~s => ~s ..."
               method-name keys-n-values (url->string url))

      (let ((rv
             (let ((ip (get-pure-port url)))
               (dynamic-wind
                   void
                   (lambda ()
                     (ssax:xml->sxml ip '()))
                   (lambda () (close-input-port ip))))))
        (pretty-print rv (current-error-port))
        rv
      ))))

(define-syntax (define-flickr-api stx)
  (syntax-case stx ()
    ((_ api-name)
     (syntax
      (define api-name
        (lambda keys-n-values
          (assert (not (memq 'api_key keys-n-values)))
          (assert (not (memq 'api_sig keys-n-values)))
          (parse-xml
           (let* ((function-name (symbol->string 'api-name))
                  (the-function (flickr function-name))
                  (most-args-list
                   (append
                    (list 'api_key *flickr-API-key*)
                    keys-n-values))
                  (all-args-list
                   (if (positive? (string-length *flickr-API-secret*))
                       (append
                        (list 'api_sig (sign-args most-args-list))
                        most-args-list)
                       most-args-list))
                  (args-ht (list
                             (apply ->ht all-args-list))))
             (when (*verbose*)
               (parameterize ((print-hash-table #t))
                             (fprintf (current-error-port)
                                      "Calling ~a with ~s~%" function-name args-ht)))
             (let-values (((results cpu-ms wall-ms gc-cpu-ms)
                           (time-apply the-function
                                       args-ht)))
               (hash-table-increment! *timings* function-name wall-ms)
               (apply values results)
               )))))))))

(define-flickr-api flickr.auth.getFrob)
(define-flickr-api flickr.auth.checkToken)
(define-flickr-api flickr.auth.getToken)
(define-flickr-api flickr.contacts.getPublicList)
(define-flickr-api flickr.people.findByUsername)
(define-flickr-api flickr.photos.addTags)
(define-flickr-api flickr.photos.getExif)
(define-flickr-api flickr.people.getInfo)
(define-flickr-api flickr.photos.getInfo)
(define-flickr-api flickr.photos.getSizes)
(define-flickr-api flickr.photos.search)

;; convert a list of alternating symbols and otherthings into a hash
;; table, with the symbols as the keys and the otherthings as the
;; values.  This hash table is what the various xmlrpc functions want.
(define (->ht . args)
  (let loop ((args args)
             (conses '()))
    (cond
     ((null? args)
      (let ((rv (make-hash-table)))
        (for-each (lambda (p)
                    (hash-table-put!
                     rv
                     (car p)
                     (cdr p)))
                  conses)
        rv))
     ((null? (cdr args))
      (error "->ht called with an odd number of arguments"))
     (else
      (let ((key (car args))
            (value (cadr args)))
        (loop (cddr args)
              (cons (cons key value)
                    conses)))))))

(define (parse-xml string)
  (ssax:xml->sxml (open-input-string string) '()))
)