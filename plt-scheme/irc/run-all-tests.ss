#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#
(module run-all-tests mzscheme
(require (planet "test.ss"    ("schematics" "schemeunit.plt" 2))
         (planet "text-ui.ss"    ("schematics" "schemeunit.plt" 2))
         "alarm-with-snooze.ss"
         "bot-tests.ss"
         "channel-idle-event.ss"
         "parse.ss"
         "planet-emacsen.ss"
         "tinyurl-task.ss"
         "tinyurl.ss"
         )
(exit (if (positive? (test/text-ui
                      (test-suite
                       "eva thang"
                       alarm-with-snooze-tests
                       bot-tests
                       channel-idle-event-tests
                       parse-tests
                       planet-tests
                       tinyurl-task-tests
                       tinyurl-tests)))
          1 0))
)
