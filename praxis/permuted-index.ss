#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id: v4-script-template.ss 6086 2009-06-14 20:14:28Z erich $
exec  mzscheme -l errortrace --require "$0" --main -- ${1+"$@"}
|#

;; http://programmingpraxis.com/2009/12/22/permuted-index/

#lang scheme
(require srfi/1
         (planet schematics/schemeunit:3)
         (planet schematics/schemeunit:3/text-ui))

(define (all-splits s)
  (let* ([s (regexp-split #px" +" s)]
         [l (length s) ])
    (for/list
        ([i (in-range (add1 l))])
      (list (take s i)
            (drop s i)))))

(define-test-suite all-splits-tests
  (pretty-print (all-splits "Yo momma eats elderberries")))

(define-test-suite eva-thang
  all-splits-tests)

(define (main . args)
  (exit (run-tests eva-thang 'verbose)))
(provide main)
