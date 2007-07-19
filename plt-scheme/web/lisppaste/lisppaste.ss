#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module lisppaste mzscheme
(require (planet "xmlrpc.ss" ("schematics" "xmlrpc.plt" ))
         (only (planet "zdate.ss"  ("offby1" "offby1.plt")) zdate)
         (only (lib "match.ss") match-lambda))

(display
 (let* ((result (((xmlrpc-server "common-lisp.net" 8185 "RPC2") "pasteheaders") 3)))
   (map
    (match-lambda ((number time username channel title num-annotations)
                   (format "Paste #~a at ~a from ~a on channel ~a, titled ~s, ~a annotations~%"
                          number
                          time
                          username
                          channel
                          title
                          num-annotations)))
    result)))
(newline)

;; debug me with something like
;; strace -s 200 -fe send -e signal=none mzscheme -qu lisppaste.ss
)