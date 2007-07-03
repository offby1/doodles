#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

;; see
;; http://docs.amazonwebservices.com/AmazonS3/2006-03-01/RESTAuthentication.html
;; for the gritty details of authentication

(module amazon-s3 mzscheme
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
         (only "secret-signing-data.ss" SecretAccessKey))
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
     (url->string truncated)
     (or sub-resource ""))))

;; just like the one in the library, except it doesn't append a
;; carriage-return/newline.
(define (base64-encode bytes)
  (let ((sop (open-output-string)))
    (base64-encode-stream (open-input-bytes bytes) sop "")
    (get-output-string sop)))

(define (call verb content type url)

  (let ((date (rfc-2822-date)))
    (define (make-auth-header)
      (let ((stringtosign  (format "~a\n~a\n~a\n~a\n~a"
                                   (symbol->string verb)
                                   (if (eq? verb 'GET) "" (md5 content))
                                   (if (eq? verb 'GET) "" type)
                                   date
                                   (CanonicalizedResource #:request-URI url))))
        (format "Authorization: AWS ~a:~a"
                AWSAccessKeyId
                (base64-encode (sign (string->bytes/utf-8 stringtosign))))))

    (let ((args (list url
                      (list (make-auth-header)
                            (format "Date: ~a" date)))))
      (let* ((ip (apply get-pure-port args))
             (response-html-string (port->string ip)))
        (html->shtml response-html-string)))))


(let* ((host "s3.amazonaws.com")
       (root (make-url "http" #f host #f #t (list (make-path/param "" '())) '() #f)))
  (let* ((buckets ((sxpath '(listallmybucketsresult buckets))
                   (call 'GET "" "text/schmext" root)))
         (names (map (lambda (b) (cadar ((sxpath '(bucket name))  b))) buckets)))
    (printf "=> ~a => ~a~%" buckets names)
    (let ((something-else
           (call 'GET "" "" (make-url "http" #f host #f #t
                                      (list (make-path/param (car names) '()))
                                      '() #f))))
      (printf "~a => ~a~%" (url->string root) ((sxpath '(listbucketresult )) something-else))))
  )
)