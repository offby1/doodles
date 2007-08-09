#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace --no-init-file --mute-banner --version --require "$0" -p "text-ui.ss" "schematics" "schemeunit.plt" -e "(exit (test/text-ui periodic-alarm-tests 'verbose))"
|#
(module periodic-alarm mzscheme
(require (lib "kw.ss")
         (lib "trace.ss")
         (planet "test.ss"    ("schematics" "schemeunit.plt" 2))
         (planet "util.ss"    ("schematics" "schemeunit.plt" 2))
         "alarm-with-snooze.ss")

(define-values (struct:periodic-alarm make-periodic-alarm periodic-alarm? periodic-alarm-ref periodic-alarm-set!)
    (make-struct-type 'periodic-alarm
                      #f 1 0
                      #f (list (cons prop:evt 0))
                      (make-inspector) #f '(0)))

(define/kw (public-make-periodic-alarm interval #:key [id 'unknown])
  (make-alarm-with-snooze interval #:id id))

(define periodic-alarm-tests

  (test-suite
   "periodic-alarm"
   (test-case
    "triggers a buncha times"
    (let ((ra (public-make-periodic-alarm 1/10)))
      (check-not-false (sync/timeout 2/10 ra))
      (check-not-false (sync/timeout 2/10 ra))
      (check-not-false (sync/timeout 2/10 ra))
      (check-not-false (sync/timeout 2/10 ra))))))

(provide periodic-alarm-tests
         (rename public-make-periodic-alarm make-periodic-alarm ))

)
