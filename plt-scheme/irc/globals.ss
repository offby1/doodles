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
(define *client-name* "Eric Hanchrow (aka offby1)'s bot")
(define *client-version* (make-parameter "$Rev$")) ;better than nothing

;; *sigh*.  The version string with which we reply to CTCP can't have
;; a colon, but of course Subversion's keyword expansion inserted a
;; colon into *client-version*, so we have to parse out the number.
(define *client-version-number* (format "v2.~a" (second (string-tokenize (*client-version*)) )))

(define *client-environment*
  (format "PLT scheme version ~a on ~a"
          (version)
          (system-type 'os)))

(define *verbose* #f)
(define (verbose!) (set! *verbose* #t))

(define *irc-server-name*
  (make-parameter
   "localhost"
   ;;"irc.freenode.net"
   )
  )
(define *initial-channel-names* (make-parameter '("#bots" "#scheme-bots")))
(define *random?* (make-parameter #t))
(define *quote-and-headline-interval* (make-parameter (* 20 60)))
(define *quotes-file-name* (make-parameter
                                    (build-path
                                     (this-expression-source-directory)
                                     "quotes")))
(define *use-real-atom-feed?* (make-parameter #f))

;; setting this too low can cause the IRC server to hang up on us,
;; complaining "ERROR :Closing Link: rudybot (Excess Flood)".  I
;; haven't found anything in the crappy dancer-ircd docs to tellme
;; what the maximum allowable rate _is_, so I chose this default by
;; trial and error.

;; To be fair, this is unlikely to be a problem in practice, since the
;; bot only spews when the channel is quiet, and it also bumps this
;; to a much higher value when we pass the --planet command-line flag.

;; too slow: 1
(define *planet-task-spew-interval* (make-parameter 2))
(define *log-output-port* (make-parameter (current-output-port)))
(provide (all-defined))
)