#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module xmlrpc mzscheme
(require (planet "xmlrpc.ss" ("schematics" "xmlrpc.plt" )))
(define betty (xmlrpc-server "betty.userland.com" 80 "RPC2"))
(define get-state-name (betty "examples.getStateName"))
(printf "State 42 is: ~s~%" (get-state-name 42))
)