#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace --no-init-file --mute-banner --version --require "$0" -p "text-ui.ss" "schematics" "schemeunit.plt" -e "(exit (test/text-ui alarm-with-snooze-tests 'verbose))"
|#
(module alarm-with-snooze mzscheme
(require (lib "kw.ss")
         (lib "trace.ss")
         (planet "test.ss"    ("schematics" "schemeunit.plt" 2))
         (planet "util.ss"    ("schematics" "schemeunit.plt" 2)))

(define-values (struct:alarm-with-snooze make-alarm-with-snooze alarm-with-snooze? alarm-with-snooze-ref alarm-with-snooze-set!)
    (make-struct-type 'alarm-with-snooze #f 3 0
                      #f (list (cons prop:evt 0))
                      (make-inspector) #f '(0 1 2)))

(define (alarm-with-snooze-snooze-button r)
  (alarm-with-snooze-ref r 1))

(define (alarm-with-snooze-id r)
  (alarm-with-snooze-ref r 2))

(define/kw (public-make-alarm-with-snooze
            interval
            #:key
            [id 'unknown]
            [periodic? #f])
  (let* ((s (make-semaphore))
         (sleeper (lambda ()
                    (let loop ()
                      (sleep interval)
                      (semaphore-post s)
                      (when periodic? (loop))))))
    (let ((t (thread sleeper)))
      (make-alarm-with-snooze
       s
       (lambda/kw (#:key [fatal? #f])
         (kill-thread t)
         (when (not fatal?)
           (set! t (thread sleeper))))
       id))))

(define alarm-with-snooze-tests

  (test-suite
   "alarm-with-snooze"
   (test-case
    "triggers like any alarm"
    (check-not-false
     (let ((ra (public-make-alarm-with-snooze 1/10 #:id 'triggers-like-any)))
       (sync/timeout 2/10 ra))
     "damn, it didn't get triggered."))

   (test-case
    "triggers repeatedly when asked"
    (let ((ra (public-make-alarm-with-snooze
               1/10
               #:periodic? #t
               #:id 'triggers-repeatedly)))
      (check-not-false (sync/timeout 2/10 ra))
      (check-not-false (sync/timeout 2/10 ra))
      (check-not-false (sync/timeout 2/10 ra))
      (check-not-false (sync/timeout 2/10 ra))
      ))

   (test-case
    "dies when asked"
    (let ((ra (public-make-alarm-with-snooze
               1/100
               #:periodic? #t
               #:id 'killable)))
      (check-not-false (sync/timeout 2/100 ra))
      (check-not-false (sync/timeout 2/100 ra))
      ((alarm-with-snooze-snooze-button ra) #:fatal? #t)
      (check-false (sync/timeout 2 ra))
      ))

   (test-case
    "doesn't trigger if we tickle it"
    (let ((ra (public-make-alarm-with-snooze
               1/10
               #:id 'tickle-me-Elmo)))
      (check-false (sync/timeout 9/100 ra))
      ((alarm-with-snooze-snooze-button ra))
      (check-false (sync/timeout 9/100 ra))
      ((alarm-with-snooze-snooze-button ra))
      (check-false (sync/timeout 9/100 ra))
      ((alarm-with-snooze-snooze-button ra))
      (check-false (sync/timeout 9/100 ra))
      (check-not-false
       (sync/timeout 2/100 ra)
       "it didn't")))
   ))

(provide (all-defined-except
          make-alarm-with-snooze
          alarm-with-snooze-ref
          alarm-with-snooze-set!)
         (rename public-make-alarm-with-snooze make-alarm-with-snooze ))
)