#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -qu "$0" ${1+"$@"}
|#

(module history mzscheme
(print-struct #t)
(require (lib "assert.ss" "offby1")
         (only (lib "1.ss" "srfi" ) append-map)
         "trick.ss"
         (lib "trace.ss"))
(provide (all-defined))

(define-struct history (tricks) #f)
(define (history-length h)
  (vector-length (history-tricks h)))
(define (history-empty? h)
  (zero? (history-length h)))
(define (history-latest-trick h)
  (vector-ref (history-tricks h)
              (sub1 (history-length h))))
(define (history-complete? h)
  (and (= 13 (history-length h))
       (trick-complete? (history-latest-trick h))))
(define (history-card-set h)
  (append-map trick-cards (vector->list (history-tricks h))))
)
