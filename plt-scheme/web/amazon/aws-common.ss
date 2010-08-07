#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec  mzscheme -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang scheme

(require file/md5
         scheme/date
         (planet lizorkin/sxml:2:1/sxml)
         (only-in net/base64 base64-encode-stream)
         (planet jaymccarthy/hmac-sha1:1:1/hmac-sha1)
         (planet schematics/macro:1:2/macro)
         (only-in srfi/13 substring/shared))

(provide (all-defined-out))
(define AWSAccessKeyId "0CMD1HG61T92SFB969G2")
(define SecretAccessKey (get-preference '|AWS-secret-access-key|))

(define (rfc-2822-date)
  (parameterize ((date-display-format 'rfc2822))
    (date->string (seconds->date (current-seconds)) #t)))

;; just like the one in the library, except it doesn't append a
;; carriage-return/newline.
(define (base64-encode bytes)
  (let ((sop (open-output-string)))
    (base64-encode-stream (open-input-bytes bytes) sop "")
    (get-output-string sop)))

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

(define (sign bytes) (base64-encode (HMAC-SHA1 SecretAccessKey bytes)))

(define-struct (exn:fail:aws exn:fail) (code message complete-response))
(define (gack-on-error sxml error-path)
  (let ((sxml ((sxpath error-path) sxml)))
    (when (not (null? sxml))
      (let ((code    (car ((sxpath '(code    *text*)) sxml)))
            (message (car ((sxpath '(message *text*)) sxml))))
        (raise (make-exn:fail:aws
                (format  "~a: ~a"
                         code
                         message)
                (current-continuation-marks)
                code
                message
                sxml)
               ))))
  sxml)
