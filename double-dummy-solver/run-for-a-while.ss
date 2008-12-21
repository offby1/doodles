#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

#lang scheme
(require (only-in (lib "1.ss" "srfi")
               every
               unfold
               )
         (lib "async-channel.ss")
         (only-in rnrs/base-6 assert)
         (planet "test.ss"     ("schematics" "schemeunit.plt" 2))
         (planet "text-ui.ss"  ("schematics" "schemeunit.plt" 2))
         (planet "util.ss"     ("schematics" "schemeunit.plt" 2)))
(provide
 call/timeout
 run-for-a-while
 run-until-N-values
 )

(define (->list ch)
  (unfold not
          values
          (lambda ignored (async-channel-try-get ch))
          (async-channel-try-get ch)
          (lambda ignored '())))

;; in a separate thread, calls PROC once, passing it an async-channel
;; to write to while it runs.  Kills the thread after SECONDS seconds.
;; Returns a list of all value written to the async-channel.

(define (call/timeout proc seconds . progress-callback)
  (let* ((queue (make-async-channel #f))
         (t (thread (lambda () (proc queue))))
         (monitor (and (not (null? progress-callback))
                       (thread
                        (lambda ()
                          (let ((time-of-death (+ seconds (current-seconds))))
                            (let loop ()
                              (sleep 1)
                              ((car progress-callback)
                               (- time-of-death (current-seconds)))
                              (loop))))))))
    (sync/timeout seconds t)
    (kill-thread t)
    (when monitor (kill-thread monitor))
    (->list queue)))

;; a specialization of the above: calls the thunk repeatedly,
;; collecting the values that it returns into the channel.  Note that
;; each _set_ of values that thunk returns gets a single list in the
;; return value.

(define (run-for-a-while thunk seconds . progress-callback)
  (apply
   call/timeout
   (lambda (queue)
     (let loop ()
       (async-channel-put queue (call-with-values thunk list))
       (loop)))
   seconds
   progress-callback))

;; like the above, but stops after the thunk has generated N values,
;; rather than after a particular amount of time.  More
;; determinisitic, and hence useful for testing.  Ignores
;; PROGRESS-CALLBACK for now, since I'm too lazy to figure out what to
;; do with it.
(define (run-until-N-values thunk N . progress-callback)
  (let loop ((N N)
             (result '()))
    (if (positive? N)
        (loop (sub1 N)
              (cons (call-with-values thunk list) result))
      (reverse result))))

(let ((lotsa-random-numbers? (lambda (thing)
                               ;; THING should be a list of random
                               ;; floats.  I'm too lazy to check
                               ;; carefuly.
                               (or (null? thing)
                                   (and (< 10 (length thing))
                                        (not (= (list-ref thing 0)
                                                (list-ref thing 1)))
                                        (not (exact? (list-ref thing 0)))
                                        (not (exact? (list-ref thing 1))))))))
  (test/text-ui
   (test-suite
    "run-for-a-while"
    (test-equal? "call/timeout 1" (call/timeout (lambda (q) 6) #e.1) '())
    (test-equal? "call/timeout 2" (call/timeout (lambda (q) (async-channel-put q 6)) #e.1) '(6))
    (test-pred   "call/timeout 3" (lambda (act)
                                    (< 10 (length act)))
                 (call/timeout (lambda (q) (let l () (async-channel-put q 6) (l))) #e.1))
    (test-equal? "call/timeout 4" (call/timeout (lambda (q) (sleep .2)) #e.1) '())
    (test-pred   "call/timeout 5" lotsa-random-numbers?
                 (call/timeout (lambda (q) (let l () (async-channel-put q (random)) (l))) #e.01))
    (test-pred "run-for-a-while 1" (lambda (act)
                                     (and (< 10 (length act))
                                          (every (lambda (thing)
                                                   (equal? thing '(6)))
                                                 act)))
               (run-for-a-while (lambda ignored 6) #e.01))
    (test-equal? "run-for-a-while 2" (run-for-a-while (lambda ignored (sleep .2)) #e.1)'() )
    (test-pred "run-for-a-while 3" (lambda (act)
                                     (lotsa-random-numbers? (apply append act)))
               (run-for-a-while random #e.1))
    )))
