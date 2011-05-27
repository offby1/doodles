#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec racket -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang racket

(require "hmac-sha256.rkt"
         rackunit rackunit/text-ui)

(define/contract (hex-string->bytes s)
  (string? . -> . bytes?)

  (let loop ([remainders '()]
             [n  (read (open-input-string (format "#x~a" s)))])
    (cond
     ((zero? n)
      (apply bytes remainders))
     (else
      (let-values ([(q r) (quotient/remainder n 256)])
        (loop (cons r remainders) q)))))
  )

(define/contract (bytes->hex-string b)
  (bytes? . -> . string?)
  (string-join
   (map
    (curryr number->string 16)
    (bytes->list b))
   ""))

(define-check (check-hmac key-hex-string data-bytes expected-hex-string)
  (let* ([key-bytes      (hex-string->bytes key-hex-string)]
         [expected-bytes (hex-string->bytes expected-hex-string)]
         [actual-bytes   (HMAC-SHA256 key-bytes data-bytes)])
    (with-check-info (['expected-hex expected-hex-string]
                      ['actual-hex (bytes->hex-string actual-bytes)])
      (when (not (equal? actual-bytes expected-bytes))
        (fail-check)))))

;;http://www.rfc-archive.org/getrfc.php?rfc=4231
(define-test-suite tc1
  (check-hmac
   "0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b"
   #"Hi There"
   "b0344c61d8db38535ca8afceaf0bf12b881dc200c9833da726e9376c2e32cff7"))

(define-test-suite tc2
  (check-hmac
   "4a656665"
   #"what do ya want for nothing?"
   "5bdcc146bf60754e6a042426089575c75a003f089d2739839dec58b964ec3843"))

(define-test-suite tc3
  (check-hmac
   (make-string 40 #\a)
   (hex-string->bytes (make-string 100 #\d))
   "773ea91e36800e46854db8ebd09181a72959098b3ef8c122d9635514ced565fe"))

(define-test-suite tc4
  (check-hmac
   "0102030405060708090a0b0c0d0e0f10111213141516171819"
   (hex-string->bytes "cdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcd"  )
   "82558a389a443c0ea4cc819899f2083a85f0faa3e578f8077a2e3ff46729665b"))

(define-test-suite all-tests
  tc1 tc2 tc3 tc4)

(provide main)
(define (main . args)
  (let ([status (run-tests all-tests 'verbose)])
    (when (positive? status)
      (exit 1))))
