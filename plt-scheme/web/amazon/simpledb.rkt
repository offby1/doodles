#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec racket --require "$0" --main -- ${1+"$@"}
|#

#lang racket
(require (only-in "aws-common.ss" AWSAccessKeyId sign)
         (only-in (planet neil/htmlprag:1:6) html->shtml)
         (only-in (planet offby1/offby1/zdate) zdate)
         (only-in net/uri-codec alist->form-urlencoded form-urlencoded->alist)
         (only-in net/url url-host url-path call/input-url post-pure-port string->url)
         (only-in srfi/13 string-join)
         (only-in unstable/net/url url-path->string)
         rackunit
         rackunit/text-ui)

;; Attempt to follow the rules at
;; http://docs.amazonwebservices.com/AmazonSimpleDB/latest/DeveloperGuide/REST_RESTAuth.html

(define (sort-alist query-string-components)
  (define (thing->bytes/utf-8 t)
    (cond
     ((bytes? t) t)                     ;assume it's UTF-8 already --
                                        ;probably difficult to ensure

     ((string? t) (string->bytes/utf-8 t))
     ((symbol? t) (thing->bytes/utf-8 (symbol->string t)))
     ((number? t) (thing->bytes/utf-8 (number->string t)))))
  (sort query-string-components bytes<? #:key (compose thing->bytes/utf-8 car)))

(define (signed-POST-body url form-data)

  (when (string? url)
    (set! url (string->url url)))

  (let* ([boilerplate `((AWSAccessKeyId   . ,AWSAccessKeyId)
                        (SignatureMethod  . "HmacSHA1")
                        (SignatureVersion . "2")
                        (Timestamp        . ,(zdate))
                        (Version          . "2009-04-15")
                        )]
         [merged  (sort-alist (append boilerplate form-data))]
         [string-to-sign  (string->bytes/utf-8 (format "~a~%~a~%~a~%~a"
                                                       "POST"
                                                       (url-host url)
                                                       (url-path->string (url-path url))
                                                       (alist->form-urlencoded merged)))]
         [whole-enchilada-list (cons `(Signature . ,(sign string-to-sign)) merged)])

    (string->bytes/utf-8 (alist->form-urlencoded whole-enchilada-list))))

(define-test-suite sign-tests
  (let ([a (form-urlencoded->alist
            (bytes->string/utf-8
             (signed-POST-body
              "http://frotz"
              '((Hugger . "mugger")))))])

    (match-let ([(list (cons 'Signature        sig)
                       (cons 'AWSAccessKeyId   key)
                       (cons 'Hugger           hugger)
                       (cons 'SignatureMethod  meth)
                       (cons 'SignatureVersion sigver)
                       (cons 'Timestamp        time)
                       (cons 'Version          ver))
                 a])
      (check-true (string? sig))

      (check-equal? key    AWSAccessKeyId)
      (check-equal? meth   "HmacSHA1")
      (check-equal? sigver "2")
      (check-equal? hugger "mugger"))))

(define (post-with-signature url form-data)
  (call/input-url
   url
   (lambda (url headers)
     (post-pure-port
      url
      (signed-POST-body url form-data)
      headers))

   ;; actually the response is XML, but this works fine
   html->shtml

   `("Content-Type: application/x-www-form-urlencoded")))

(define (simpledb-post form-data)
  (post-with-signature
   (string->url "http://sdb.amazonaws.com/")
   form-data))

(define (list-domains)
  (simpledb-post
   `((Action . "ListDomains")
     (MaxNumberOfDomains . "100"))))

(define-test-suite all-tests
  sign-tests)

(define (main . args)
  (let ([failures (run-tests all-tests)])
    (when (positive? failures)
      (exit 1)))
  (printf "Domains: ~a~%" (list-domains)))

(provide main)
