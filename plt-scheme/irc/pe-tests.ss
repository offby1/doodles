#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module pe-tests mzscheme
(require (planet "test.ss"    ("schematics" "schemeunit.plt" 2))
         (planet "util.ss"    ("schematics" "schemeunit.plt" 2))
         (prefix 19: (lib "19.ss" "srfi" )))
(require/expose
 "planet-emacsen.ss"
 (
  entry?
  make-entry
  ))
(define (make-fake-entry)
  (make-entry (19:current-date)
              "Some title"
              "http://some.url.com"))
(define pe-tests
  (test-suite
   "planet.emacsen.org"
   (test-pred
    "it's an entry"
    entry?
    (make-fake-entry))))
(provide pe-tests)
)