#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id: v4-script-template.ss 5748 2008-11-17 01:57:34Z erich $
exec  mzscheme --require "$0" --main -- ${1+"$@"}
|#

#lang scheme
(require (planet schematics/schemeunit:3)
         (planet schematics/schemeunit:3/text-ui)
         (prefix-in json: (planet dherman/json:1:1/json)))

(define *the-string* "[\"sup homies\", {\"foo\": \"bar\"}]")
(define *the-data* (list "sup homies" (make-immutable-hasheq '((foo . "bar")))))

(define hmm-tests

  (test-suite
   "it"
   (test-begin
    (check-equal?
     *the-string*
     (call-with-output-string
      (lambda (op)
        (json:write *the-data* op))))
    (check-equal?
     *the-data*
     (call-with-input-string *the-string* json:read)))))

(define (main . args)
  (exit (run-tests hmm-tests 'verbose)))
(provide (all-defined-out))
