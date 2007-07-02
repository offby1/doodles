#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module get-test mzscheme
(require (lib "url.ss" "net"))
(define ip (get-pure-port (string->url "http://localhost/")))
(write
 (let loop ((accum '()))
   (let ((stuff (read-line ip)))
     (if  (eof-object? stuff)
         (reverse accum)
       (loop (cons stuff accum)))
     )))
(newline)
)