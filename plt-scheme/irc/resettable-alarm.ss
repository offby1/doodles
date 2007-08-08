#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace --no-init-file --mute-banner --version --require "$0" -p "text-ui.ss" "schematics" "schemeunit.plt" -e "(exit (test/text-ui resettable-alarm-tests 'verbose))"
|#
(module resettable-alarm mzscheme
(require (lib "trace.ss")
         (planet "test.ss"    ("schematics" "schemeunit.plt" 2))
         (planet "util.ss"    ("schematics" "schemeunit.plt" 2)))

(define (make-resettable-alarm interval)
  (let* ((s (make-semaphore))
         (id (current-milliseconds))
         (sleeper (lambda ()
                    (sleep interval)
                    (printf "~a says RINNGGGGGG!!! Time to wake up!!~%"
                            id)
                    (semaphore-post s))))
    (let ((t (thread sleeper)))
      (values s (lambda ()
                  (kill-thread t)
                  (printf "~a says you may snoooze for ~a seconds.~%"
                          id
                          interval)
                  (set! t (thread sleeper)))))))

(define resettable-alarm-tests

  (test-suite
   "resettable-alarm"
   (test-case
    "triggers like any alarm"
    (check-not-false
     (let-values (((sync-on-me-baby reset!) (make-resettable-alarm 1/10)))
       (sync/timeout 2/10 sync-on-me-baby)
       )
     "damn, it didn't get triggered."))
   (test-case
    "doesn't trigger if we tickle it"
    (let-values (((sync-on-me-baby reset!) (make-resettable-alarm 1/10)))
      (check-false (sync/timeout 9/100 sync-on-me-baby))
      (reset!)
      (check-false (sync/timeout 9/100 sync-on-me-baby))
      (reset!)
      (check-false (sync/timeout 9/100 sync-on-me-baby))
      (reset!)
      (check-false (sync/timeout 9/100 sync-on-me-baby))
      (check-not-false
       (sync/timeout 2/100 sync-on-me-baby)
       "it didn't")
      ))
   ))

(provide (all-defined))
)
