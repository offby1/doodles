#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec  mzscheme -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang scheme

(require (planet schematics/schemeunit:3)
         (planet schematics/schemeunit:3/text-ui)
         (planet williams/uuid:1:1/uuid))

(define (hex-string->integer hs)
  (call-with-input-string
   (string-append "#x" hs)
   read))

(define uuid->integer
  (match-lambda
   [(? string? u)
    (uuid->integer (string->uuid u))]
   [(? uuid? u)
    (hex-string->integer (uuid->hex-string u))]))

(define (guid->shard guid all-shards)
  (let ([n (uuid->integer guid)])
    (list-ref all-shards (remainder n (length all-shards)))))

(define-test-suite guid->shard-tests

  (check-equal? 'frotz (guid->shard "a776d3cd-2b43-43b3-aa40-85fe51f4103d" (list 'frotz)))
  (check-equal? 'snurk (guid->shard "a776d3cd-2b43-43b3-aa40-85fe51f4103d" (list 'snurk))))

(define (main . args)
  (printf "~a~%" (guid->shard "a776d3cd-2b43-43b3-aa40-85fe51f4103d" (build-list 10 values)))
  (exit (run-tests guid->shard-tests 'verbose)))
(provide guid->shard main)
