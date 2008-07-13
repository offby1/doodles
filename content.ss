#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec  mzscheme -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang scheme
(require (planet "test.ss"    ("schematics" "schemeunit.plt" ))
         (planet "text-ui.ss" ("schematics" "schemeunit.plt" ))
         (planet "util.ss"    ("schematics" "schemeunit.plt" )))

(define checksum? exact-integer?)

(define (get c)
  #f)

(define (put thing)
  #f)

(provide/contract
 [get (-> checksum? any)]
 [put (-> any/c checksum?)])

(define tests
  (test-suite
   "yow"
   (test-case
    "hoo"
    (check-false (get 'fred)))))

(provide  main)
(define (main . args)
  (exit (test/text-ui tests 'verbose)))