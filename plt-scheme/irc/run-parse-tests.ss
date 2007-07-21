#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module run-parse-tests mzscheme
(require
 (planet "text-ui.ss" ("schematics" "schemeunit.plt" 2))
 (only (planet  "assert.ss"  ("offby1"     "offby1.plt")) exit-if-failed)
 "parse-tests.ss")
(exit-if-failed
 (test/text-ui
  parse-tests))
)