#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module get-test mzscheme
(require (lib "url.ss" "net")
         (lib "pretty.ss")
         (planet "hmac-sha1.ss" ("jaymccarthy" "hmac-sha1.plt" ))
         (planet "htmlprag.ss" ("neil" "htmlprag.plt" )))
(define AWSAccessKeyId "0CMD1HG61T92SFB969G2")
(define Signature (HMAC-SHA1 #"This is clearly a bogus key." #"This is in fact bogus data."))
(define auth-header (format "Authorization: AWS ~a:~a" AWSAccessKeyId Signature))
(define ip (get-pure-port (string->url "http://s3.amazonaws.com/")))
(define response-html-string
  (let loop ((accum '()))
    (let ((stuff (read-line ip)))
      (if  (eof-object? stuff)
          (apply string-append (reverse accum))
        (loop (cons stuff accum)))
      )))
(display response-html-string)
(newline)
(pretty-display (html->shtml response-html-string))

)