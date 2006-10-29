#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module run-for-a-while mzscheme
(require (lib "async-channel.ss"))
(provide run-for-a-while)

(define (->list ch)
  (unfold not
          values
          (lambda ignored (async-channel-try-get ch))
          (async-channel-try-get ch)
          (lambda ignored '())))

;; in a separate thread, calls PROC once, passing it an async-channel
;; to write to while it runs.  Kills the thread after SECONDS seconds.

;;(call/timeout (lambda (q) 6) .1) => ()
;;(call/timeout (lambda (q) (async-channel-put q 6)) .1) => (6)
;;(call/timeout (lambda (q) (let l () (async-channel-put q 6) (l))) .1) => (6 6 6 6 6 6 ...)
;;(call/timeout (lambda (q) (sleep .2)) .1) => ()
;;(call/timeout (lambda (q) (let l () (async-channel-put q (random)) (l))) .01);; => lotsa random numbers
(define (call/timeout proc seconds)
  (let* ((queue (make-async-channel #f))
         (t (thread (lambda ()
                      (proc queue)))))
    (sync/timeout seconds t)
    (kill-thread t)
    (->list queue)))

;; a specialization of the above: calls the thunk repeatedly,
;; collecting the values that it returns into the channel.

;;(run-for-a-while (lambda ignored 6) .1) => ((6)(6)(6)(6)(6)(6) ...)
;;(run-for-a-while (lambda ignored (sleep .2)) .1) => ()
;;(run-for-a-while (lambda ignored (values (random) (random))) .1) => lotsa pairs o' random numbers

(define (run-for-a-while thunk seconds)
  (call/timeout
   (lambda (queue)
     (let loop ()
       (async-channel-put queue (call-with-values thunk list))
       (loop)))
   seconds))
)