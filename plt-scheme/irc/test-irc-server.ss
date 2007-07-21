#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module test-irc-server mzscheme
(require "globals.ss")
;; An IRC server that doesn't do very much at all.  It's for testing
;; the client.
(define (test-irc-server)
  (let-values (((readme writeme)
                (make-pipe)))
    (thread
     (lambda ()
       (define (PRIVMSG str)
         (fprintf  writeme (format "PRIVMSG ~a :~a~a~%" *my-nick* str #\return))
         (flush-output writeme))
       (PRIVMSG "what up")
       (PRIVMSG "\u0001VERSION\u0001")
       (PRIVMSG "OK, that's all.")
       (close-output-port writeme)
       ))
    (values readme (current-output-port))))
(provide (all-defined))
)