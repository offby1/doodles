#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module lisppaste mzscheme
(require (planet "xmlrpc.ss" ("schematics" "xmlrpc.plt" ))
         (only (planet "zdate.ss"  ("offby1" "offby1.plt")) zdate))

(display
 ;; the API is documented at http://common-lisp.net/project/lisppaste/xml-rpc.html
 (let* ((result (((xmlrpc-server "common-lisp.net" 8185 "RPC2") "pasteheaders") 3)))
   (map
    (lambda (args)
      (apply format  "Paste #~a at ~a from ~a on channel ~a, titled ~s, ~a annotations~%" args))

    result)))
(newline)

;; debug me with something like
;; strace -s 200 -fe send -e signal=none mzscheme -qu lisppaste.ss
)