#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace --no-init-file --mute-banner --version --require "$0" -p "text-ui.ss" "schematics" "schemeunit.plt" -e "(exit (test/text-ui headline-tests 'verbose))"
|#
(module headline mzscheme
(require (lib "trace.ss")
         (planet "test.ss"    ("schematics" "schemeunit.plt" 2))
         (planet "util.ss"    ("schematics" "schemeunit.plt" 2)))

;; this is what our queue returns.
(define-struct entry (timestamp title link) (make-inspector))


(define headline-tests

  (test-suite
   "headline"
   (test-case
    "yow"
    (check-regexp-match
     #rx"bar"
     "foo"))))

(provide (all-defined))
)
