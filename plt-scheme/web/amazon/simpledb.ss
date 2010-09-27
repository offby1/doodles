#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec  mzscheme -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang scheme
(require "aws-common.ss"
         net/url
         (only-in (planet offby1/offby1/zdate) zdate)
         (planet neil/htmlprag:1:6)
         (only-in srfi/13 string-join))

;; Attempt to follow the rules at
;; http://docs.amazonwebservices.com/AmazonSimpleDB/latest/DeveloperGuide/REST_RESTAuth.html

;; Coulda swore I implemented this once before someplace ...
(define (query-string-components->list query-string-components)
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
                 (symbol->string name)
                 value)])
       (sort-parameters query-string-components)))

(define (stringlist->bytes strs)
  (string->bytes/utf-8
   (apply
    string-append
    (map (curryr string-append "\n") strs))))

(define (post url form-data)
  (let* ([boilerplate `((AWSAccessKeyId   . ,AWSAccessKeyId)
                        (SignatureMethod  . "HmacSHA1")
                        (SignatureVersion . "2")
                        (Timestamp        . ,(zdate))
                        (Version          . "2009-04-15")
                        )]
         [merged (append boilerplate form-data)]
         [string-to-sign (stringlist->bytes (query-string-components->list
                                             merged))]
         [w-e-list  (cons (cons 'Signature  (sign string-to-sign)) merged)]
         [whole-enchilada (stringlist->bytes (query-string-components->list w-e-list))])

    (fprintf (current-error-port)
             "URL: ~s~%form-data: ~s~%whole-enchilada: ~s~%"
             (url->string url)
             form-data
             whole-enchilada)


    (call/input-url
     url
     (curryr post-pure-port whole-enchilada)
     html->shtml)))

(define (list-domains sdb)
  (let loop ([accum '()])
    (let ([response (post (string->url "http://sdb.amazonaws.com")
                          `((Action . "ListDomains") (MaxNumberOfDomains . "100")))])
      response)))

(define sdb #f)

(define (main . args)
  (printf "Domains: ~a~%" (list-domains sdb)))

(provide main)
