#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace --no-init-file --mute-banner --version --require "$0" -p "text-ui.ss" "schematics" "schemeunit.plt" -e "(exit (test/text-ui thing-tests 'verbose))"
|#
(module thing mzscheme
(require (lib "trace.ss")
         (planet "test.ss"    ("schematics" "schemeunit.plt" 2))
         (planet "util.ss"    ("schematics" "schemeunit.plt" 2)))

(define (make-my-doohickey interval)
  (let* ((s (make-semaphore))
         (sleeper (lambda ()
                    (sleep interval)
                    (printf "Posting!~%")
                    (semaphore-post s))))
    (let ((t (thread sleeper)))
      (values s (lambda ()
                  (kill-thread t)
                  (printf "Resetting!~%")
                  (set! t (thread sleeper)))))))

(define thing-tests

  (test-suite
   "thing"
   (test-case
    "triggers like any alarm"
    (check-not-false
     (let-values (((sync-on-me-baby reset!) (make-my-doohickey 1)))
       (sync/timeout 2 sync-on-me-baby)
       )
     "damn, it didn't get triggered."))
   (test-case
    "doesn't trigger if we tickle it"
    (let-values (((sync-on-me-baby reset!) (make-my-doohickey 1)))
      (check-false (sync/timeout 9/10 sync-on-me-baby))
      (reset!)
      (check-false (sync/timeout 9/10 sync-on-me-baby))
      (reset!)
      (check-false (sync/timeout 9/10 sync-on-me-baby))
      (reset!)
      (check-false (sync/timeout 9/10 sync-on-me-baby))
      (check-not-false
       (sync/timeout 2/10 sync-on-me-baby)
       "it didn't")
      ))
   ))

(provide (all-defined))
)
