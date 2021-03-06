#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec racket -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang racket

;; see
;; http://docs.amazonwebservices.com/AmazonS3/2006-03-01/RESTAuthentication.html
;; for the gritty details of authentication


(require net/url scheme/pretty
 (planet neil/htmlprag:1:6/htmlprag)
 (planet lizorkin/sxml:2:1/sxml)
 "aws-common.ss"
 file/md5
 mzlib/trace
 unstable/debug)

(define (just-the-path request-URI)
  (url->string  (make-url #f #f #f #f #t (url-path request-URI) '() #f)))

(define (md5-b64 bytes)
   (base64-encode (md5 bytes #f)))

(define-values (GET PUT)
  (let ()
    (define (call verb url-path-string content type)
      (let* ((url (make-url "http"           ;scheme
                            #f               ;user
                            "s3.amazonaws.com" ;host
                            #f                 ;port
                            #t                 ;path-absolute?
                            (list (make-path/param url-path-string '())) ;path
                            '()         ;query
                            #f          ;fragment
                            ))
             (date (rfc-2822-date))
             (sig (sign (string->bytes/utf-8
                         (format "~a\n~a\n~a\n~a\n~a"
                                 (symbol->string verb)
                                 (if (eq? verb 'GET) "" (md5-b64 content))
                                 (if (eq? verb 'GET) "" type)
                                 date
                                 (just-the-path url)))))
           (auth (format "Authorization: AWS ~a:~a" (AWSAccessKeyId) sig)))

        (case verb
          ((GET) (call/input-url
                  url
                  (lambda (url . rest)
                    (debug (url->string url))
                    (debug rest)
                    (apply get-pure-port url rest))
                  html->shtml
                  (list auth (format "Date: ~a" date))))
          ((PUT) (call/input-url
                  url
                  (lambda (url headers)
                    (printf "Url: ~s; content ~s; headers: ~s~%"
                            (url->string url)
                            content headers)
                    (put-pure-port url content headers))
                  html->shtml
                  (list auth
                        (format "Date: ~a" date)
                        (format "Content-Type: ~a" type)
                        (format "Content-MD5: ~a" (md5-b64 content)))))

          (else
           (error "You know ... I just don't know how to deal with" verb)))))

    (values (lambda (thing)              (gack-on-error (call 'GET thing "" ""       ) '(error)))
            (lambda (thing content type) (gack-on-error (call 'PUT thing content type) '(error))))))

(provide main)
(define (main . args)
  (printf "Known buckets: ~a ~%"
          ((sxpath '(listallmybucketsresult buckets (bucket) name *text*)) (GET "")))
  (printf "Creating a bucket: ~a~%"
          (PUT "squankulous" #"" "text/schmext"))
  (printf "Putting something into it: ~a~%"
          (PUT "squankulous/mankulous" #"So this is the stuff." "text/plain"))
  (printf "Seeing what's in it: ~a~%"
          ((sxpath '(listbucketresult contents key *text*)) (GET "squankulous")))
  (printf "Seeing what's in the object what's in the bucket: ~a~%"
          ((sxpath '(*text*)) (GET "squankulous/mankulous")))

  (with-handlers (((lambda (e)
                     (and (exn:fail:aws? e)
                          (string=? "NoSuchBucket" (exn:fail:aws-code e))))
                   (lambda (e)
                     (printf "Just as we expected -- a NoSuchBucket error~%")
                     )))
    (printf "Putting something into a bucket what don't exist: ")
    (PUT "oooooohhhhhhnooooooo/wozzup" #"Nobody expects the Portuguese Tribunal!!" "text/plain")))
