#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id: v4-script-template.ss 5863 2008-12-21 17:13:36Z erich $
exec  mzscheme --require "$0" --main -- ${1+"$@"}
|#

#lang scheme

(require (planet schematics/schemeunit:3)
         (planet schematics/schemeunit:3/text-ui)
         "incubot.ss")

(require/expose "incubot.ss" (make-db db?))

(define hmm-tests

  (test-suite
   "loop"
   (check-pred db? (file->db "something"))
   (check-false (lookup "snord" (make-db 'stuff)))
   (check-equal? "The snord horde" (lookup "snord" (make-db "The snord horde")))))


(define (main . args)
  (exit (run-tests hmm-tests 'verbose)))
(provide  main)