#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id: v4-script-template.ss 6086 2009-06-14 20:14:28Z erich $
exec  mzscheme -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang scheme
(require (planet schematics/schemeunit:3)
         (planet schematics/schemeunit:3/text-ui))

(define/contract (inject-1 letter word)
  (char?  string? . -> . (listof string?))
  (reverse
   (for/fold ([words '()])
       ([i (in-range (add1 (string-length word)))])
       (cons
        (string-append
         (substring word 0 i)
         (string letter)
         (substring word i))
        words))))

(define/contract (inject letter words)
  (char? (listof string?) . -> . (listof (listof string?)))
  (map (curry inject-1 letter) words))

(define-test-suite inject-tests

  (check-equal? (inject-1 #\a "") (list "a"))
  (check-equal? (inject-1 #\a "xy") (list "axy" "xay" "xya"))

  (check-equal? (inject #\a (list ""))  `(("a")))
  (check-equal? (inject #\a (list "fred")) `(("afred" "fared" "fraed" "fread" "freda")))

  (check-equal? (inject #\a (list "fred" "ted"))
                `(( "afred" "fared" "fraed" "fread" "freda")
                  ("ated" "taed" "tead" "teda")))

  )

(define (main . args)
  (exit (run-tests inject-tests 'verbose)))
(provide inject main)
