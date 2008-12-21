#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id: v4-script-template.ss 5863 2008-12-21 17:13:36Z erich $
exec  mzscheme --require "$0" --main -- ${1+"$@"}
|#

#lang scheme
(require (planet schematics/schemeunit:3)
         (planet schematics/schemeunit:3/text-ui))

(define-struct db (stuff) #:transparent)

(provide/contract [file->db [ string? . -> . db?]])
(define (file->db filename)
  (make-db 'stuff))

(define hmm-tests

  (test-suite
   "loop"
   (check-pred db? (file->db "something"))))

(define (main . args)
  (exit (run-tests hmm-tests 'verbose)))
(provide  main)
