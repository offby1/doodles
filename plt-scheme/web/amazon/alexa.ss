#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module alexa mzscheme
(require (lib "uri-codec.ss" "net")
         (lib "url.ss" "net")
         (lib "pretty.ss")
         (planet "htmlprag.ss"  ("neil"        "htmlprag.plt" ))
         (planet "port.ss"      ("schematics"  "port.plt" ))
         (only (lib "19.ss" "srfi") date->string current-date)
         "aws-common.ss"
         "secret-signing-data.ss")
;; the date->string procedure from date.ss _claims_ to support
;; iso-8601, but alexa complains it's the wrong format, so I'm rolling
;; my own here.
(define (iso-8601-date)
  (date->string (current-date) "~Y-~m-~dT~X~z"))

(define (alexa-call query)
  ;; without this, alexa complains "The requested version ( ) is not
  ;; valid."  So I guess that means it only recognizes ampersands as
  ;; separators.
  (parameterize ((current-alist-separator-mode 'amp))
                (let* ((version "2007-03-15")
                       (action "Search")
                       (date (iso-8601-date))
                       (url (make-url "http"
                                      #f          ;user
                                      "wsearch.amazonaws.com" ;host
                                      #f                      ;port
                                      #t ;path-absolute?
                                      (list (make-path/param "" '())) ;path
                                      `(
                                        (AWSAccessKeyId . ,AWSAccessKeyId)
                                        (Timestamp . ,date)
                                        (Signature
                                         .
                                         ,(sign (string->bytes/utf-8
                                                 (format "~a~a" action date)))
                                         )
                                        (Version . ,version)
                                        (Action . ,action)
                                        (ResponseGroup . "Context")
                                        (Query . ,query)
                                        ) ;query
                                      #f  ;fragment
                                      )))

                  (gack-on-error
                   (html->shtml (port->string (get-pure-port url
                                                             (list
                                                              (format "Date: ~a" date)))))
                   '(errorresponse error)))))

(printf "An Alexa call, which to be honest has nothing to do with s3:")
(pretty-print (alexa-call "kitty cats"))
)