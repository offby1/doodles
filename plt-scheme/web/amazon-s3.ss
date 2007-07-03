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
(trace sign)
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

(define (gpp url strings)
  (fprintf (current-error-port) "get-pure-port ~s ~s~%" (url->string url) strings)
  (get-pure-port url strings))

(define call
  (let ((host "s3.amazonaws.com"))
    (lambda (verb url-path-string content type)
      (define port-func
        (case verb
          ((GET) gpp)
          ((PUT)put-pure-port)
          (else
           (error "You know ... I just don't know how to deal with" verb))))

      (let* ((url (make-url "http" #f host #f #t
                            (list (make-path/param url-path-string '()))
                            '() #f))
             (date (rfc-2822-date))
             (sig (base64-encode (sign (string->bytes/utf-8
                                        (format "~a\n~a\n~a\n~a\n~a"
                                                (symbol->string verb)
                                                (if (eq? verb 'GET) "" (md5 content))
                                                (if (eq? verb 'GET) "" type)
                                                date
                                                (CanonicalizedResource #:request-URI url))))))
             (auth (format "Authorization: AWS ~a:~a" AWSAccessKeyId sig)))

        (html->shtml
         (port->string
          (port-func url (list auth (format "Date: ~a" date))))))
      )))

;;(trace call)

(let* ((buckets ((sxpath '(listallmybucketsresult buckets))
                 (call 'GET "" "" "text/schmext")))
       (names (map (lambda (b) (cadar ((sxpath '(bucket name))  b))) buckets)))

  (printf "=> ~a => ~a~%" buckets names)

  (pretty-display
   (map
    (lambda (name)
      (let ((something-else
             (call 'GET name "" "")))
        ((sxpath '(listbucketresult )) something-else)))
    names)))

;; let's create a bucket!!
;; (let ((stuff (call 'PUT "/foo" #"" "text/schmext")))
;;   (pretty-display stuff))
)