#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec  mzscheme -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang racket
(require rackunit
         rackunit/text-ui
         "aws-common.ss"
         net/url
         net/uri-codec
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

(define-test-suite query-string-components->list-tests
  (check-equal? (query-string-components->list '((foo . "bar") (baz . "ugh")))
                '("baz=ugh" "foo=bar")))

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
         [string-to-sign  (string->bytes/utf-8 (alist->form-urlencoded merged))]
         [w-e-list  (cons (cons 'Signature  (sign string-to-sign)) merged)]
         [whole-enchilada (string->bytes/utf-8 (alist->form-urlencoded w-e-list))])

    (fprintf (current-error-port)
             "URL: ~s~%form-data: ~s~%whole-enchilada: ~s~%"
             (url->string url)
             form-data
             whole-enchilada)


    (call/input-url
     url
     (lambda (url headers) (post-pure-port url whole-enchilada headers))
     html->shtml
     `("Content-Type: application/x-www-form-urlencoded"))))

(define (list-domains sdb)
  (let loop ([accum '()])
    (let ([response (post (string->url "http://sdb.amazonaws.com")
                          `((Action . "ListDomains") (MaxNumberOfDomains . "100")))])
      response)))

(define sdb #f)

(define-test-suite all-tests
  query-string-components->list-tests)

(define (main . args)
  (let ([failures (run-tests all-tests)])
    (when (positive? failures)
      (exit 1)))
  (printf "Domains: ~a~%" (list-domains sdb)))

(provide main)
