#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qtmv "$0" -e "(run-and-exit)"
|#
(module run-all-tests mzscheme
(require (planet "test.ss"    ("schematics" "schemeunit.plt" 2))
         (planet "text-ui.ss"    ("schematics" "schemeunit.plt" 2))
         "alarm-with-snooze.ss"
         "bot-tests.ss"
         "channel-idle-event.ss"
         "del.ss"
         "parse.ss"
         "planet-emacsen.ss"
         "tinyurl-task.ss"
         "tinyurl.ss"
         )
(register-version-string "$Id$")
(define eva-thang (test-suite
                   "eva thang"
                   alarm-with-snooze-tests
                   bot-tests
                   channel-idle-event-tests
                   del.icio.us-tests
                   parse-tests
                   planet-tests
                   tinyurl-task-tests
                   tinyurl-tests))

(define (run-and-exit)
  (exit
   (if (positive? (test/text-ui eva-thang))
       1
     0)))

(provide (all-defined))
)
