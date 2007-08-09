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
         "parse.ss"
         "tinyurl-task.ss"
         "tinyurl.ss"
         )
(exit (if (positive? (test/text-ui
                      (test-suite
                       "eva thang"
                       alarm-with-snooze-tests
                       parse-tests
                       bot-tests
                       tinyurl-tests
                       tinyurl-task-tests)))
          1 0))
)
