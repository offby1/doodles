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
 "simple-tests.ss")

(parameterize
 ((*random?* #f))

 (exit (+
        (test/text-ui
         parse-tests)
        (test/text-ui
         simple-tests))))
)