#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec racket -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang racket
(require rackunit rackunit/text-ui)
(require racket/date
         (prefix-in srfi-19- srfi/19))

(provide days-since-kelly)
(define (days-since-kelly [now (srfi-19-current-date)])
  (let ([weekday (srfi-19-date-week-day now)])
    (modulo (- weekday 5) 7))
  )

(define-test-suite days-since-kelly-tests
  (for ([offset '(0
                  -28800 ;; Pacific Standard Time
                  )])
    (check-equal? (days-since-kelly
                   (srfi-19-make-date
                    0                   ; nanosecond
                    0                   ; second
                    0                   ; minute
                    0                   ; hour
                    0                   ; day -- 0 = Sunday
                    11                  ; month
                    2010                ; year
                    offset
                    ))
                  2 ;; Friday was 2 days before Sunday
                  )))

(define-test-suite all-tests
  days-since-kelly-tests)

(provide main)
(define (main)
  (exit (run-tests all-tests 'verbose)))
