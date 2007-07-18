#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module lisppaste mzscheme
(require (planet "xmlrpc.ss" ("schematics" "xmlrpc.plt" ))
         (planet "ssax.ss"   ("lizorkin"   "ssax.plt"))
         (planet "zdate.ss"  ("offby1" "offby1.plt"))
         (lib "trace.ss")
         (lib "date.ss")
         (only
          (lib "1.ss" "srfi")
          first second third fourth fifth sixth seventh))

(define pasteheaders ((xmlrpc-server "common-lisp.net" 8185 "RPC2") "pasteheaders"))

(display
 (let* ((result (pasteheaders 3)))
   (map
    (lambda (paste)
      (let ((number (first paste))
            (time (second paste))
            (username (third paste))
            (channel (fourth paste))
            (title (fifth paste))
            (num-annotations (sixth paste)))
        (format "Paste #~a at ~a from ~a on channel ~a, titled ~s, ~a annotations~%"
                number (zdate time) username channel title num-annotations))
      )
    result)
   ))

(newline)

;; debug me with something like
;; strace -s 200 -fe send -e signal=none mzscheme -qu lisppaste.ss
)