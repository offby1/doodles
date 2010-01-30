#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id: v4-script-template.ss 6182 2009-11-10 04:59:27Z erich $
exec  mzscheme -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang scheme
(require (planet schematics/schemeunit:3)
         (planet schematics/schemeunit:3/text-ui))

(define-syntax-rule (with-swallowed-exceptions exn-predicate body ...)
  (with-handlers
      ([exn-predicate (lambda (e) (printf "Swallowing an exception: ~a~%" (exn-message e)))])
    body ...))

(define-syntax-rule (calm-loop generator-stuff exn-predicate body ...)
  (for generator-stuff
    (with-swallowed-exceptions
     exn-predicate
     body ...)))

(define-struct (exn:bigfaterror exn:fail) () #:transparent)

(define (process datum)
  (if (even? datum)
      (raise (make-exn:bigfaterror (format "I don't like even numbers like ~a" datum) (current-continuation-marks)))
      (format "~a is nice because it's even" datum)))

(define (main . args)
  (calm-loop ([d (list 1 2 3 4 5)]) exn:bigfaterror?
   (display (process d))
   (newline)))

(provide main)
