#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module globals mzscheme
(require (only (lib "etc.ss") this-expression-source-directory)
         (only (lib "1.ss" "srfi")
               second)
         (only (lib "13.ss" "srfi")
               string-tokenize))
(define *my-nick* (make-parameter "rudybot"))
(define *passive?* (make-parameter #f))
(define *timeout-seconds* (make-parameter #f))
(define *client-name* "Eric Hanchrow's bot")
(define *client-version* (make-parameter "unknown 0"))

;; *sigh*.  The version string with which we reply to CTCP can't have
;; a colon, but of course Subversion's keyword expansion inserted a
;; colon into *client-version*, so we have to parse out the number.
(define *client-version-number* (second (string-tokenize (*client-version*)) ))

(define *client-environment*
  (format "PLT scheme version ~a on ~a"
          (version)
          (system-type 'os)))

(define *verbose* (make-parameter #f))
(define *irc-server-name*
  (make-parameter
   "localhost"
   ;;"irc.freenode.net"
   )
  )
(define *initial-channel-names* (make-parameter '()))
(define *random?* (make-parameter #t))
(define *jordanb-quote-interval* (make-parameter (* 20 60)))
(define *jordanb-quotes-file-name* (make-parameter
                                    (build-path
                                     (this-expression-source-directory)
                                     "jordanb-quotes")))
(provide (all-defined))
)