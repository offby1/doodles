#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module task mzscheme
(require (lib "kw.ss")
         "vprintf.ss")
;; Calls THUNK every SECONDS seconds.  Calling the return value with
;; the symbol POSTPONE postpones the next call (i.e., it resets the
;; timer).  Calling the return value with any other value kills the
;; task permanently.

;; TODO -- maybe make a wrapper for this called
;; "do-when-channel-idle", which saves the task someplace out of the
;; way, monitors the named channel, and cancels the task as needed.
;; Right now those things are being done in "respond".
;; sit around and wait a while, then do the thunk, then start over.

;; we'll cut the wait short, and do the thunk, if someone shoves an #f
;; at us.

;; we'll cut the wait short, and _not_ do the thunk, if someone shoves
;; 'POSTPONE at us.

;; we'll respond with #t or #f if we're asked if we're 'running?

;; we'll go away entirely if we receive any other value.

;; TODO -- for debugging, perhaps make this return a structure that
;; prints nicely (i.e., it could include a name like "jordanb-quotes"
;; or "planet-emacs-headlines" or something)
(define/kw (do-in-loop seconds thunk #:key [name "unknown task"])
  (let* ((c (make-channel))
         (t (thread (lambda ()
                      (let loop ()
                        (let ((reason (sync/timeout seconds c)))
                          (when (or (not reason) ;timed out
                                    (not (channel-get c)))
                            (thunk)))
                        (loop)
                        )))))
    (lambda (command)
      (vtprintf "do-in-loop: got command ~s for task ~s~%"
                command name)
      (case  command
       ((#f postpone POSTPONE)
        (channel-put c command))
       ((running?)
        (not (thread-dead? t)))
       ((die-damn-you-die)
        (kill-thread t))
       (else
        (error 'do-in-loop "I don't know how to deal with ~s" command))))))

(provide (all-defined))
)