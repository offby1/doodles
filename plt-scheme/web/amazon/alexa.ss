#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec  mzscheme -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang scheme

(require (lib "trace.ss")
         (lib "cmdline.ss")
         (lib "uri-codec.ss" "net")
         (lib "url.ss" "net")
         (lib "pretty.ss")
         (planet "htmlprag.ss"  ("neil"        "htmlprag.plt" ))
         (planet "zdate.ss"     ("offby1"      "offby1.plt"))
         (only-in (planet "sxml.ss"      ("lizorkin"    "sxml.plt"))
               sxpath)
         (only-in (lib "13.ss" "srfi")
               string-join)
         "aws-common.ss"
         )

(define (alexa-call query)
  ;; without this, alexa complains "The requested version ( ) is not
  ;; valid."  So I guess that means it only recognizes ampersands as
  ;; separators.
  (parameterize ((current-alist-separator-mode 'amp))
    (let* ((version "2007-03-15")
           (action "Search")
           (date (zdate (current-seconds)))
           (url (string->url "http://wsearch.amazonaws.com/")))

      (set-url-query! url `(
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
                            ))

      (gack-on-error
       (html->shtml
        (call/input-url
         url
         (lambda (url)
           (printf "Retrieving ~s...~%" (url->string url))
           (get-pure-port
            url
            (list
             (format "Date: ~a" date))))
         port->string))
       '(errorresponse error)))))

;;(trace alexa-call)

(parse-command-line
 "alexa" (current-command-line-arguments)
 *amazon-command-line-parsing-table*
 (lambda (flag-accum . the-query)

   (when (not (SecretAccessKey))
     (error "You must supply a secret access key with the -s option"))

   (when (null? the-query)
     (error "You must type some words to search for on the command line"))

   (let* ((result (alexa-call (string-join the-query " ")))
          (urls   ((sxpath '(// document url   *text*)) result))
          (titles ((sxpath '(// document title *text*)) result)))

     (for-each (lambda (caption link)
                 (printf "~s ~s~%" caption link))
               titles
               urls))   )
 '("query"))
