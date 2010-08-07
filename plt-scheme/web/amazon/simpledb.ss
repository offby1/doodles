#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec  mzscheme -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang scheme
(require "aws-common.ss")

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
