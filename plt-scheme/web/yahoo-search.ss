#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec  mzscheme --require "$0" --main -- ${1+"$@"}
|#

#lang scheme
(require (planet schematics/schemeunit:3)
         (planet schematics/schemeunit:3/text-ui)
         net/url)

(define (yahoo-search query)
  (call/input-url
   (make-url "http" #f "search.yahooapis.com" #f #t (list (make-path/param "WebSearchService" '())
                                                          (make-path/param "V1" '())
                                                          (make-path/param "webSearch" '()))
             `((appid . "QRrlhLPV34Ed3nypZaoUxrZsCa4xOvOArZwAYIyIx1c56rAtHu.xxiStPHapqb8kT79euQQ-")
               (query . ,query)
               )
             #f)
   get-pure-port
   (lambda (ip)
     (let ((op (open-output-string)))
       (copy-port ip op)
       (get-output-string op)))))

(define hmm-tests

  (test-suite
   "loop"
   (test-case
    "yow"
    (check-not-false (yahoo-search "kitty cats")))))
(define (main . args)
  (exit (run-tests hmm-tests 'verbose)))
(provide (all-defined-out))
