#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

;; see
;; http://docs.amazonwebservices.com/AmazonS3/2006-03-01/RESTAuthentication.html
;; for the gritty details of authentication

(module amazon-s3 mzscheme
(require (lib "url.ss" "net")
         (lib "date.ss")
         (lib "pretty.ss")
         (lib "trace.ss")
         (lib "md5.ss")
         (only (lib "base64.ss" "net") base64-encode-stream)
         (only (lib "13.ss" "srfi") string-join substring/shared)
         (planet "port.ss"      ("schematics"  "port.plt" ))
         (planet "htmlprag.ss"  ("neil"        "htmlprag.plt" ))
         (planet "fmt.ss"       ("ashinn"      "fmt.plt"))
         (planet "sxml.ss"      ("lizorkin"    "sxml.plt"))
         (only "secret-signing-data.ss" SecretAccessKey)

         ;; normally I'd use (planet "hmac-sha1.ss" ("jaymccarthy" "hmac-sha1.plt" ))
         ;; but I fear that version is buggy; this version has the fixes.
         "hmac-sha1.ss"
         )
(define (rfc-2822-date)
  (parameterize ((date-display-format 'rfc2822))
                (date->string (seconds->date(current-seconds)) #t))
  )

(define AWSAccessKeyId "0CMD1HG61T92SFB969G2")
(define *last-thing-signed* #f)         ;for debugging
(define (sign bytes)
  (set! *last-thing-signed* bytes)
  (let ((sigbytes (HMAC-SHA1 SecretAccessKey bytes)))

    ;; Warn about some flakiness in HMAC-SHA1
    (let* ((length (bytes-length sigbytes))
           (diff (- 20 length)))
      (when (positive? diff)
        (fprintf (current-error-port)
                 "Signature is ~a bytes short!  Expect chaos.~%"
                 diff)))

    (base64-encode sigbytes)))
;;(trace sign)

(define (just-the-path request-URI)
  (url->string  (make-url #f #f #f #f #t (url-path request-URI) '() #f)))

;; just like the one in the library, except it doesn't append a
;; carriage-return/newline.
(define (base64-encode bytes)
  (let ((sop (open-output-string)))
    (base64-encode-stream (open-input-bytes bytes) sop "")
    (get-output-string sop)))

(define *verbose* (make-parameter #f))
(define (gpp url headers)
  (when (*verbose*) (fprintf (current-error-port) "get-pure-port ~s ~s~%" (url->string url) headers))
  (get-pure-port url headers))

(define (ppp url content headers)
  (when (*verbose*) (fprintf (current-error-port) "put-pure-port ~s ~s ~s~%" (url->string url) headers content))
  (put-pure-port url content headers))

(define (hexdecode abc)
  (let loop  ((s (bytes->string/utf-8 abc))
              (result '()))
    (if (zero? (string-length s))
        (apply bytes (reverse result))
      (let* ((two-digits (substring/shared s 0 2))
             (number (read (open-input-string (string-append "#x" two-digits)))))
        (loop (substring/shared s 2)
              (cons number result))))))

(define (md5-b64 bytes)
  (base64-encode (hexdecode (md5 bytes))))

(define-struct (exn:fail:s3 exn:fail) (code message))

(define GET #f)
(define PUT #f)
(let ()
  (define (gack-on-error sxml)
    (when (not (null? ((sxpath '(error)) sxml)))
      (let ((code    (car ((sxpath '(error code    *text*)) sxml)))
            (message (car ((sxpath '(error message *text*)) sxml))))
        (raise (make-exn:fail:s3
                (format  "~a: ~a"
                         code
                         message)
                (current-continuation-marks)
                code
                message)
               )))
    sxml)
  (define host "s3.amazonaws.com")
  (define (call verb url-path-string content type)
    (let* ((url (make-url "http" #f host #f #t
                          (list (make-path/param url-path-string '()))
                          '() #f))
           (date (rfc-2822-date))
           (sig (sign (string->bytes/utf-8
                       (format "~a\n~a\n~a\n~a\n~a"
                               (symbol->string verb)
                               (if (eq? verb 'GET) "" (md5-b64 content))
                               (if (eq? verb 'GET) "" type)
                               date
                               (just-the-path url)))))
           (auth (format "Authorization: AWS ~a:~a" AWSAccessKeyId sig)))

      (html->shtml
       (port->string
        (case verb
          ((GET) (gpp url
                      (list auth (format "Date: ~a" date))))
          ((PUT) (ppp url content
                      (list auth
                            (format "Date: ~a" date)
                            (format "Content-Type: ~a" type)
                            (format "Content-MD5: ~a" (md5-b64 content)))))
          (else
           (error "You know ... I just don't know how to deal with" verb)))
        )))
    )
  (set! GET (lambda (thing)              (gack-on-error (call 'GET thing "" ""))))
  (set! PUT (lambda (thing content type) (gack-on-error (call 'PUT thing content type)))))

;;(trace call)

(let* ((response (GET ""))
       (names ((sxpath '(listallmybucketsresult buckets (bucket) name *text*)) response)))

  (when (null? names)
    (error response))

  (printf "Known buckets: ~s ~%" names)
  (printf "Creating a bucket: ")
  (pretty-print (PUT "squankulous" #"" "text/schmext"))
  (printf "Putting something into it: ")
  (pretty-print (PUT "squankulous/mankulous" #"So this is the stuff." "text/plain"))
  (printf "Seeing what's in it: ")
  (pretty-print ((sxpath '(listbucketresult contents key *text*)) (GET "squankulous")))
  (printf "Seeing what's in the object what's in the bucket: ")
  (pretty-print (GET "squankulous/mankulous"))

  (with-handlers (((lambda (e)
                     (and (exn:fail:s3? e)
                          (string=? "NoSuchBucket" (exn:fail:s3-code e))))
                   (lambda (e)
                     (printf "Just as we expected -- a NoSuchBucket error.~%")
                     )))
    (printf "Putting something into a bucket what don't exist: ")
    (pretty-print (PUT "oooooohhhhhhnooooooo/wozzup" #"Nobody expects the Portuguese Tribunal!!" "text/plain")))
  )

)