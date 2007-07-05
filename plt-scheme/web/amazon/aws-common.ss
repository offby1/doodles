#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module aws-common mzscheme
(require (lib "trace.ss")
         (planet "sxml.ss"      ("lizorkin"    "sxml.plt"))
         "secret-signing-data.ss"
         (only (lib "base64.ss" "net") base64-encode-stream)
         ;; normally I'd use (planet "hmac-sha1.ss" ("jaymccarthy" "hmac-sha1.plt" ))
         ;; but I fear that version is buggy; this version has the fixes.
         "hmac-sha1.ss"
         )
(provide (all-defined))
(define AWSAccessKeyId "0CMD1HG61T92SFB969G2")

;; just like the one in the library, except it doesn't append a
;; carriage-return/newline.
(define (base64-encode bytes)
  (let ((sop (open-output-string)))
    (base64-encode-stream (open-input-bytes bytes) sop "")
    (get-output-string sop)))

(define (sign bytes) (base64-encode (HMAC-SHA1 SecretAccessKey bytes)))

(define-struct (exn:fail:s3 exn:fail) (code message complete-response))
(define (gack-on-error sxml error-path)
  (let ((sxml ((sxpath error-path) sxml)))
    (when (not (null? sxml))
      (let ((code    (car ((sxpath '(code    *text*)) sxml)))
            (message (car ((sxpath '(message *text*)) sxml))))
        (raise (make-exn:fail:s3
                (format  "~a: ~a"
                         code
                         message)
                (current-continuation-marks)
                code
                message
                sxml)
               ))))
  sxml)
;;(trace gack-on-error)
)