#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module run-all-tests mzscheme
(require
 (planet "text-ui.ss" ("schematics" "schemeunit.plt" 2))
 (planet "test.ss"    ("schematics" "schemeunit.plt" 2))
 (only "globals.ss" *random?*)
 "parse-tests.ss"
 "pe-tests.ss"
 "task-tests.ss"
 "tests.ss")

(parameterize
 ((*random?* #f))

 (exit (if (positive? (test/text-ui
                       (test-suite
                        "Grand unified test suite"
                        task-tests
                        parse-tests
                        tests
                        pe-tests)
                       ))
           1
         0)))
)