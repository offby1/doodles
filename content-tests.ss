#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec  mzscheme -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang scheme

(require (planet "test.ss"    ("schematics" "schemeunit.plt" ))
         (planet "text-ui.ss" ("schematics" "schemeunit.plt" ))
         (planet "util.ss"    ("schematics" "schemeunit.plt" ))
         "content.ss")

(define tests
  (test-suite
   "yow"
   (test-case
    "hoo"
    (let ((s (make-store)))
      (check-false (get s 123))
      (let-values (((s sum) (put s "Snarkulous")))
        (check-equal? (get s sum) "Snarkulous"))))))

(provide  main)
(define (main . args)
  (exit (test/text-ui tests 'verbose)))