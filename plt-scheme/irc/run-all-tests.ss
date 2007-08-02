#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module run-all-tests mzscheme
(require
 (planet "text-ui.ss" ("schematics" "schemeunit.plt" 2))
 (only "globals.ss" *random?*)
 "parse-tests.ss"
 "pe-tests.ss"
 "tests.ss")

(parameterize
 ((*random?* #f))

 (exit (if (positive? (+
                       (test/text-ui
                        parse-tests)
                       (test/text-ui
                        tests)
                       (test/text-ui
                        pe-tests)))
           1
         0)))
)