#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

;; run this on *nix, and follow the directions: i.e., don't type
;; anything at the prompt.  It'll die in a second.  Once it does, type
;; something at your shell, and note that your keystrokes aren't
;; echoed: you need to type "reset" in order to restore echoing.

(module rl mzscheme
(dynamic-require '(lib "rep.ss" "readline") #f)
(printf "Don't type anything for a couple seconds: ")
(sync
 (thread read-line)
 (thread (lambda () (sleep 1) (car 'foo))))
)