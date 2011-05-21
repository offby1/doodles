#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec racket -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang racket
(require mzlib/trace
         rackunit
         rackunit/text-ui
         "aws-common.ss"
         net/url
         net/uri-codec
         (only-in (planet offby1/offby1/zdate) zdate)
         (only-in unstable/net/url url-path->string)
         (planet neil/htmlprag:1:6)
         (only-in srfi/13 string-join))

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

(define (post-with-signature url form-data)
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
                                                       (url-path->string
                                                        (url-path url))
                                                       (alist->form-urlencoded merged)))]
         [w-e-list  (cons (cons 'Signature  (sign string-to-sign)) merged)]
         [whole-enchilada (string->bytes/utf-8 (alist->form-urlencoded w-e-list))])

    (call/input-url
     url
     (lambda (url headers) (post-pure-port url whole-enchilada headers))
     html->shtml
     `("Content-Type: application/x-www-form-urlencoded"))))

(define (list-domains)
  (post-with-signature
   (string->url "http://sdb.amazonaws.com/")
   `((Action . "ListDomains")
     (MaxNumberOfDomains . "100"))))

(define-test-suite all-tests
  (check-not-equal? "I am a" "placeholder"))

(define (main . args)
  (let ([failures (run-tests all-tests)])
    (when (positive? failures)
      (exit 1)))
  (printf "Domains: ~a~%" (list-domains)))

(provide main)
