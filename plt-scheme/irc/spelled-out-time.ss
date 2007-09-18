#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace --no-init-file --mute-banner --version --require "$0" -p "text-ui.ss" "schematics" "schemeunit.plt" -e "(exit (test/text-ui spelled-out-time-tests 'verbose))"
|#
(module spelled-out-time mzscheme
(require (lib "trace.ss")
         (planet "test.ss"    ("schematics" "schemeunit.plt" 2))
         (planet "util.ss"    ("schematics" "schemeunit.plt" 2)))

(define (spelled-out-time . args)
  "Gaah.")


(define spelled-out-time-tests

  (test-suite
   "spelled-out-time"
   (test-equal? "one second" (spelled-out-time 1) "one second")
   (test-equal? "two seconds" (spelled-out-time 2) "two seconds")
   (test-equal? "twenty-five seconds" (spelled-out-time 25) "twenty-five seconds")
   (test-equal? "two minutes" (spelled-out-time 123) "two minutes")
   (test-equal? "an hour" (spelled-out-time 3611) "an hour")
   (test-equal? "two hours" (spelled-out-time 7229) "two hours")
   (test-equal? "a day" (spelled-out-time (+ 17 (* 24 3600))) "a day")))

(provide (all-defined))
)
