#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec  mzscheme -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang scheme
(require "aws-common.ss"
         (only-in srfi/13 string-join)
         (only-in net/uri-codec uri-encode))

;; Attempt to follow the rules at
;; http://docs.amazonwebservices.com/AmazonSimpleDB/latest/DeveloperGuide/REST_RESTAuth.html

;; Coulda swore I implemented this once before someplace ...
(define/contract (canonicalize-query-string query-string-components)
  (-> (listof (cons/c symbol? string?)) string?)
  (define (thing->bytes/utf-8 t)
    (cond
     ((bytes? t) t)                     ;assume it's UTF-8 already --
                                        ;probably difficult to ensure

     ((string? t) (string->bytes/utf-8 t))
     ((symbol? t) (thing->bytes/utf-8 (symbol->string t)))
     ((number? t) (thing->bytes/utf-8 (number->string t)))))

  (define (sort-parameters query-string-components)
    (sort query-string-components bytes<? #:key (compose thing->bytes/utf-8 car)))

  (string-join
   (map (match-lambda
         [(cons name value)
          (format "~a=~a"
                  (uri-encode (symbol->string name))
                  (uri-encode value))])
        (sort-parameters query-string-components))
   "&"))

(display
 (regexp-replace*
  "&"
  (canonicalize-query-string

   `(
     (Action            . "PutAttributes")
     (DomainName        . "MyDomain")
     (ItemName          . "Item123")
     (Attribute.1.Name  . "Color")
     (Attribute.1.Value . "Blue")
     (Attribute.2.Name  . "Size")
     (Attribute.2.Value . "Med")
     (Attribute.3.Name  . "Price")
     (Attribute.3.Value . "0014.99")
     (Version           . "2009-04-15")
     (Timestamp         . "2010-01-25T15:01:28-07:00")
     (SignatureVersion  . "2")
     (SignatureMethod   . "HmacSHA256")
     (AWSAccessKeyId    . "<Your AWS Access Key ID>")
     ))
  "\n&"))
(newline)

(define (post url form-data)
  (let ([headers `((AWSAccessKeyId   . ,AWSAccessKeyId)
                   (SignatureVersion . 2)
                   (SignatureMethod  . HMAC-SHA1)
                   (Timestamp        . ,(rfc-2822-date))
                   (Signature        . ,(sign #"Sign Me"))
                   )])
    (list url headers)

    #;(call/input-url
    url
    (lambda (url headers) (put-pure-port url content headers))
    html->shtml
    headers)
    ))

(define (list-domains sdb)
  (let loop ([accum '()])
    (let ([response (post "http://simpledb.amazonaws.com" `((Action . "ListDomains") (MaxNumberOfDomains . 100)))])
      response))
  )
(define sdb #f)

(define (main . args)
  (printf "Domains: ~a~%" (list-domains sdb)))

(provide main)
