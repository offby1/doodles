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
         (lib "md5.ss")
         (only (lib "base64.ss" "net") base64-encode-stream)
         (only (lib "13.ss" "srfi") string-join)
         (planet "port.ss"      ("schematics"  "port.plt" ))
         (planet "hmac-sha1.ss" ("jaymccarthy" "hmac-sha1.plt" ))
         (planet "htmlprag.ss"  ("neil"        "htmlprag.plt" ))
         (planet "fmt.ss"       ("ashinn"      "fmt.plt"))
         (planet "sxml.ss"      ("lizorkin"    "sxml.plt"))
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


;; just like the one in the library, except it doesn't append a
;; carriage-return/newline.
(define (base64-encode bytes)
  (let ((sop (open-output-string)))
    (base64-encode-stream (open-input-bytes bytes) sop "")
    (get-output-string sop)))

(let ((url (string->url "http://s3.amazonaws.com/"))
      (date (rfc-2822-date))
      (Content-MD5 "")
      (Content-Type ""))

  (define (make-auth-header verb content Content-Type url)
    (let ((stringtosign  (format "~a\n~a\n~a\n~a\n~a"
                                 (if (eq? verb 'GET) "GET" "???")
                                 (if (eq? verb 'GET) "" (md5 content))
                                 (if (eq? verb 'GET) "" Content-Type)
                                 date
                                 (CanonicalizedResource #:request-URI url))))
      (printf "stringtosignbytes: ~s~%"
              (string-join
               (map (lambda (i)
                      (string-downcase (fmt #f (pad-char #\0 (pad 2 (num i 16))))))
                    (bytes->list (string->bytes/utf-8 stringtosign)))))
      (printf "stringtosign: ~a~%" stringtosign)
      (format "Authorization: AWS ~a:~a"
              AWSAccessKeyId
              (base64-encode (sign (string->bytes/utf-8 stringtosign))))))

  (let ((args (list url
                     (list (make-auth-header  'GET "" "text/schmext" url)
                           (format "Date: ~a" date)))))
    (printf "URL: ~s; auth-header: ~s~%"
            (car args)
            (cadr args))
    (let* ((ip (apply get-pure-port args))
           (response-html-string (port->string ip))
           (response-shtml (html->shtml response-html-string))
           (buckets ((sxpath '(listallmybucketsresult buckets)) response-shtml))
           (names (map (lambda (b) (cadar ((sxpath '(bucket name))  b))) buckets)))
      (printf "~a => ~a => ~a~%" response-shtml buckets names))
    )
  )
)
