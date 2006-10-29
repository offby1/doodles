#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module run-for-a-while mzscheme
(require (lib "async-channel.ss"))
(provide run-for-a-while)
;;(run-for-a-while (lambda ignored 6) .1) => ((6)(6)(6)(6)(6)(6) ...)
;;(run-for-a-while (lambda ignored (sleep .2)) .1) => ()
;;(run-for-a-while (lambda ignored (values (random) (random))) .1) => lotsa pairs o' random numbers

(define (run-for-a-while thunk seconds)
  (let* ((queue (make-async-channel #f) )
         (t (thread (lambda ()
                      (let loop ()
                        (async-channel-put queue (call-with-values thunk list))
                        (loop))))))
    (sync/timeout seconds t)
    (kill-thread t)
    (unfold not
            values
            (lambda ignored (async-channel-try-get queue))
            (async-channel-try-get queue)
            (lambda ignored '()))))


)