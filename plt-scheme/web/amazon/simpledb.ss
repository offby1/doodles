#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec  mzscheme -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang scheme
(require "aws-common.ss"
         net/url
         (planet neil/htmlprag:1:6)
         (only-in srfi/13 string-join)
         (only-in net/uri-codec uri-encode))

;; Attempt to follow the rules at
;; http://docs.amazonwebservices.com/AmazonSimpleDB/latest/DeveloperGuide/REST_RESTAuth.html

;; Coulda swore I implemented this once before someplace ...
(define/contract (query-string-components->list query-string-components)
  (-> (listof (cons/c symbol? string?)) (listof string?))
  (define (thing->bytes/utf-8 t)
    (cond
     ((bytes? t) t)                     ;assume it's UTF-8 already --
                                        ;probably difficult to ensure

     ((string? t) (string->bytes/utf-8 t))
     ((symbol? t) (thing->bytes/utf-8 (symbol->string t)))
     ((number? t) (thing->bytes/utf-8 (number->string t)))))

  (define (sort-parameters query-string-components)
    (sort query-string-components bytes<? #:key (compose thing->bytes/utf-8 car)))

  (map (match-lambda
        [(cons name value)
         (format "~a=~a"
                 (uri-encode (symbol->string name))
                 (uri-encode value))])
       (sort-parameters query-string-components)))

(define (post url form-data)
  (let ([headers (query-string-components->list
                  `((AWSAccessKeyId   . ,AWSAccessKeyId)
                    (SignatureVersion . "2")
                    (SignatureMethod  . "HMAC-SHA1")
                    (Timestamp        . ,(rfc-2822-date))
                    (Signature        . ,(sign #"Sign Me"))
                    ))])

    (call/input-url
     url
     (lambda (url headers) (post-pure-port url form-data headers))
     html->shtml
     headers)))

(define (list-domains sdb)
  (let loop ([accum '()])
    (let ([response (post (string->url "http://sdb.amazonaws.com")
                          (string->bytes/utf-8
                           (string-join
                            (query-string-components->list
                             `((Action . "ListDomains") (MaxNumberOfDomains . "100")))
                            "&")))])
      response)))

(define sdb #f)

(define (main . args)
  (printf "Domains: ~a~%" (list-domains sdb)))

(provide main)
