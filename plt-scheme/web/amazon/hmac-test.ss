#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec  mzscheme -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang scheme


(require (planet jaymccarthy/hmac-sha1:1:1/hmac-sha1)
         schemeunit schemeunit/text-ui)

(provide main)
(define (main . args)
  (let ([status (run-tests
                 (test-suite
                  "The one and only suite"
                  (test-equal?
                   "20 bytes"
                   (bytes-length (HMAC-SHA1
                                  #"--------------------"
                                  #"k\20\306z\310\22\271\4\353Q+\375\336x\232\217\4E\203x"))
                   20)

                  (test-false
                   "second call doesn't overwrite result of first"
                   (let* ((first  (HMAC-SHA1 #"foo" #"bar"))
                          (second (HMAC-SHA1 #"baz" #"ugh")))
                     (bytes=? first second)))

                  )
                 'verbose)])
    (when (positive? status)
      (exit 1))))
