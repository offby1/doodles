#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

;; see
;; http://docs.amazonwebservices.com/AmazonS3/2006-03-01/RESTAuthentication.html
;; for the gritty details of authentication

;; http://developer.amazonwebservices.com/connect/servlet/KbServlet/download/133-102-1292/s3-example-perl-library.zip
;; might be an illuminating example

(module get-test mzscheme
(require (lib "kw.ss")
         (lib "url.ss" "net")
         (lib "date.ss")
         (lib "pretty.ss")
         (lib "trace.ss")
         (only (lib "base64.ss" "net") base64-encode-stream)
         (only (lib "13.ss" "srfi") string-join)
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

(define/kw (CanonicalizedResource #:key request-URI
                                  (bucket-name #f)
                                  (sub-resource #f)
                                  )
  (let* ((url (if (string? request-URI)
                  (string->url request-URI)
                request-URI))
         (truncated (make-url

                     #f
                     #f
                     #f
                     #f
                     #t
                     (url-path url)
                     '()
                     #f
                     )))

    (string-append
     (if bucket-name (string-append "/" bucket-name) "")
     (string-append (url->string truncated))
     (or sub-resource ""))

    ))
;;(trace CanonicalizedResource)
(CanonicalizedResource #:request-URI "http://johnsmith.s3.amazonaws.com/photos/puppy.jpg"
                       #:bucket-name "johnsmith")
(CanonicalizedResource #:request-URI "http://Geller/Uri" )
(CanonicalizedResource #:request-URI "ftp://Geller" #:bucket-name "Richard Leakey")
(CanonicalizedResource #:bucket-name "leaky" #:request-URI "svn+ssh://Uri Geller/")
(CanonicalizedResource #:bucket-name "leaky"
                       #:request-URI "telnet://Uri/Geller/"
                       #:sub-resource "?Polaris")

;; just like the one in the library, except it doesn't append a
;; carriage-return newline.
(define (base64-encode bytes)
  (let ((sop (open-output-string)))
    (base64-encode-stream (open-input-bytes bytes) sop "")
    (get-output-string sop)))

(let ((url (string->url "http://s3.amazonaws.com/")))
  (define auth-header
    (format "Authorization: AWS ~a:~a"
            AWSAccessKeyId
            (base64-encode (sign (string->bytes/utf-8 (CanonicalizedResource #:request-URI url))))))
  (printf "URL: ~s; auth-header: ~s~%"
          (url->string url)
          auth-header)
  (let ((ip (get-pure-port url (list auth-header))))
    (define response-html-string
      (let loop ((accum '()))
        (let ((stuff (read-line ip)))
          (if  (eof-object? stuff)
              (apply string-append (reverse accum))
            (loop (cons stuff accum)))
          )))
    (pretty-display (html->shtml response-html-string)))
  )
)