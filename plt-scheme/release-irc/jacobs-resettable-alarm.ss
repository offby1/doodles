;; ---
;; make-resettable-alarm : nat -> (values evt (-> void))
;; WARNING: not well-tested

(define (make-resettable-alarm delta)
  (let ([internal-comm-channel (make-channel)]
        [external-evt (make-channel)])
    (begin
      (thread
       (lambda ()
         (let loop ()
           (sync

            ;; the first event is for the case where the alarm goes off
            (handle-evt (alarm-evt (+ (current-inexact-milliseconds) delta))
                        (lambda (_) (channel-put external-evt external-evt)))

            ;; the second event handles the case where someone resets the alarm.
            ;; the strategy is to listen on an internal communications channel
            ;; for reset requests; if they come in then abandon the original
            ;; alarm and restart the loop with a new one
            (handle-evt internal-comm-channel (lambda (_) (loop))))))))
    (values
     external-evt
     (lambda ()
       ;; asynchronously register a reset request. If we don't do this asynchronously
       ;; then there's a race condition: if the alarm goes off and then somebody
       ;; tries to reset the counter then the reset attempt will block forever.
       (thread (lambda () (channel-put internal-comm-channel #t)))))))

;; example usage
(begin
  (define the-time (current-seconds))
  (define-values (c r) (make-resettable-alarm (* 5 1000)))
  (sleep 4)
  (r)
  (sync c)
  (printf "done! elapsed time: ~a seconds\n" (- (current-seconds) the-time)))

;; after 9 seconds, prints: done! elapsed time: 9 seconds

;; ---
#|
make-resettable-alarm consumes a timeout (some number of milliseconds)
and produces two values: an event that becomes enabled when the
timeout is passed (barring resets) and a function that you call to
reset the timer. When you call (make-resettable-alarm delta), it
spawns a new thread that syncs on two separate events: an alarm event
set to go off delta milliseconds in the future, and the event of
something being put onto an internal channel called
internal-comm-channel. If the alarm event goes off, we're done: enable
the external-evt channel, which the rest of the world will interpret
as meaning that the alarm has gone off. On the other hand, if the
internal-comm-channel event goes off, it means somebody wants to reset
us; we implement that just by trying the same thing again. Now the
alarm event is just external-evt, and resetting is just putting any
old value (I chose #t arbitrarily) on the internal-comm-channel.

CML-style primitives (sync, channels, etc) are very good for building
up new concurrency abstractions. Generally speaking you can use them
to build just about any kind of concurrency widget you want, and in my
experience the implementation often looks pretty much like what I've
got above: a main loop that does some task in its own thread, and
other accessors that use channels to communicate that main loop.
|#
