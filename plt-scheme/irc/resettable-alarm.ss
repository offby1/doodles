#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace --no-init-file --mute-banner --version --require "$0" -p "text-ui.ss" "schematics" "schemeunit.plt" -e "(exit (test/text-ui resettable-alarm-tests 'verbose))"
|#
(module resettable-alarm mzscheme
(require (lib "kw.ss")
         (lib "trace.ss")
         (planet "test.ss"    ("schematics" "schemeunit.plt" 2))
         (planet "util.ss"    ("schematics" "schemeunit.plt" 2)))

(define-values (struct:resettable-alarm make-resettable-alarm resettable-alarm? resettable-alarm-ref resettable-alarm-set!)
    (make-struct-type 'resettable-alarm #f 3 0
                      #f (list (cons prop:evt 0))
                      (make-inspector) #f '(0 1 2)))

(define (resettable-alarm-snooze-button r)
  (resettable-alarm-ref r 1))

(define (resettable-alarm-id r)
  (resettable-alarm-ref r 2))

(define/kw (public-make-resettable-alarm interval #:key [id 'unknown])
  (let* ((s (make-semaphore))
         (sleeper (lambda ()
                    (sleep interval)
                    (printf "~a says RINNGGGGGG!!! Time to wake up!!~%"
                            id)
                    (semaphore-post s))))
    (let ((t (thread sleeper)))
      (make-resettable-alarm
       s
       (lambda ()
         (kill-thread t)
         (printf "~a says you may snoooze for ~a seconds.~%"
                 id
                 interval)
         (set! t (thread sleeper)))
       id))))

(define resettable-alarm-tests

  (test-suite
   "resettable-alarm"
   (test-case
    "triggers like any alarm"
    (check-not-false
     (let ((ra (public-make-resettable-alarm 1/10)))
       (sync/timeout 2/10 ra))
     "damn, it didn't get triggered."))
   (test-case
    "doesn't trigger if we tickle it"
    (let ((ra (public-make-resettable-alarm 1/10)))
      (check-false (sync/timeout 9/100 ra))
      ((resettable-alarm-snooze-button ra))
      (check-false (sync/timeout 9/100 ra))
      ((resettable-alarm-snooze-button ra))
      (check-false (sync/timeout 9/100 ra))
      ((resettable-alarm-snooze-button ra))
      (check-false (sync/timeout 9/100 ra))
      (check-not-false
       (sync/timeout 2/100 ra)
       "it didn't")))
   ))

(provide (all-defined-except
          make-resettable-alarm
          resettable-alarm-ref
          resettable-alarm-set!)
         (rename public-make-resettable-alarm make-resettable-alarm ))
)
