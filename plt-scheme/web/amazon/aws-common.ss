#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module aws-common mzscheme
(require "secret-signing-data.ss"
         (only (lib "base64.ss" "net") base64-encode-stream)
         ;; normally I'd use (planet "hmac-sha1.ss" ("jaymccarthy" "hmac-sha1.plt" ))
         ;; but I fear that version is buggy; this version has the fixes.
         "hmac-sha1.ss"
         )
(provide (all-defined))
;; just like the one in the library, except it doesn't append a
;; carriage-return/newline.
(define (base64-encode bytes)
  (let ((sop (open-output-string)))
    (base64-encode-stream (open-input-bytes bytes) sop "")
    (get-output-string sop)))

(define (sign bytes) (base64-encode (HMAC-SHA1 SecretAccessKey bytes)))
)