#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module task mzscheme
(require (lib "kw.ss")
         (lib "trace.ss")
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
;; prints nicely (i.e., it could include a name like "quotes"
;; or "planet-emacs-headlines" or something)
(define/kw (do-in-loop seconds thunk #:key [name "unknown task"])
  (let* ((c (make-channel))
         (t (delay
              (thread (lambda ()
                        (let loop ()
                          (printf "thread for task ~s: syncing~%"
                                     name)
                          (let ((reason (sync/timeout seconds c)))
                            (printf "thread for task ~s: woke up because ~s~%"
                                     name reason)
                            (when (or (not reason) ;timed out
                                      (not (channel-get c)))
                              (thunk)))
                          (loop)
                          ))))))

    (define task-controller
      (lambda (command)

        (let ((t (force t)))
          (printf "task ~s, thread ~s: got command ~s~%"
                  name (eq-hash-code t) command)
          (case  command
            ((#f postpone POSTPONE)
             (thread-resume t)
             (channel-put c command))
            ((running?)
             (thread-running? t))
            ((die-damn-you-die)
             (kill-thread t))
            (else
             (error 'do-in-loop "I don't know how to deal with ~s" command))))))
    (vtprintf "do-in-loop: creating task ~s~%" name)
    (trace task-controller)
    task-controller))

(define-struct task (name-symbol interval message-generator-thunk controller)
  (make-inspector))

(define/kw (public-make-task name-symbol interval message-generator-thunk
                             #:key [verbose #f])
  (make-task name-symbol interval message-generator-thunk
             (do-in-loop interval (lambda ()
                                    (when verbose
                                      (vtprintf "task ~s about to do its thang~%" name-symbol))
                                    (message-generator-thunk)) #:name name-symbol)))

;; for testing, so that a test can start from a known state
(define (kill task)
  ((task-controller task) 'die-damn-you-die)
)

(define (task-unsuspend task)
  (unless ((task-controller task) 'running?)
    ((task-controller task) #f)))

(define (do-it-now! task)
  ((task-controller task) #f))

(define (postpone task)
  ((task-controller task) 'postpone))

(provide (all-defined-except do-in-loop make-task))
(provide (rename public-make-task make-task))
)