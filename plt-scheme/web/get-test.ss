#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

;; see
;; http://docs.amazonwebservices.com/AmazonS3/2006-03-01/RESTAuthentication.html
;; for the gritty details of authentication

(module get-test mzscheme
(require (lib "url.ss" "net")
         (lib "date.ss")
         (lib "pretty.ss")
         (planet "hmac-sha1.ss" ("jaymccarthy" "hmac-sha1.plt" ))
         (planet "htmlprag.ss" ("neil" "htmlprag.plt" ))
         "secret-signing-data.ss")
(define (rfc-2822-date)
  (parameterize ((date-display-format 'rfc2822))
                (date->string (seconds->date(current-seconds)) #t))
  )

(define AWSAccessKeyId "0CMD1HG61T92SFB969G2")
(define (sign bytes)
   (HMAC-SHA1 SecretAccessKey bytes))

(let ((bogus-data (string->bytes/utf-8 (rfc-2822-date))))
  (printf "Signature of ~s is ~s~%" bogus-data (sign bogus-data)))

(define auth-header (format "Authorization: AWS ~a:~a" AWSAccessKeyId (sign #"yeah, whatever")))
(define ip (get-pure-port (string->url "http://s3.amazonaws.com/")))
(define response-html-string
  (let loop ((accum '()))
    (let ((stuff (read-line ip)))
      (if  (eof-object? stuff)
          (apply string-append (reverse accum))
        (loop (cons stuff accum)))
      )))
(display response-html-string)
(newline)
(pretty-display (html->shtml response-html-string))

)