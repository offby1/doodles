#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module aws-common mzscheme
(require (lib "trace.ss")
         (planet "sxml.ss"      ("lizorkin"    "sxml.plt"))
         (only (lib "base64.ss" "net") base64-encode-stream)
         (only (planet "port.ss" ("schematics" "port.plt" ))
               port->string)
         (planet "hmac-sha1.ss" ("jaymccarthy" "hmac-sha1.plt"))
         (planet "aif.ss" ("schematics" "macro.plt")))

(provide (all-defined))
(define AWSAccessKeyId "0CMD1HG61T92SFB969G2")
(define SecretAccessKey
  (make-parameter
   (aif key (getenv "AWS_SECRET_ACCESS_KEY")
        (string->bytes/utf-8 key)
        key)))

;; just like the one in the library, except it doesn't append a
;; carriage-return/newline.
(define (base64-encode bytes)
  (let ((sop (open-output-string)))
    (base64-encode-stream (open-input-bytes bytes) sop "")
    (get-output-string sop)))

(define (sign bytes) (base64-encode (HMAC-SHA1 (SecretAccessKey) bytes)))

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
;;(trace gack-on-error)

(define *amazon-command-line-parsing-table*
  `(
    (once-each
     (("-s" "--secret-access-key")
      ,(lambda (flag sac) (SecretAccessKey (string->bytes/utf-8 sac)))
      ("You should have gotten this from Amazon." "sekrit")))
    ))

;; TODO -- this could probably be replaced by "call/input-url"
(define (port->string/close ip)
  (begin0
    (port->string ip)
    (close-input-port ip)))
)