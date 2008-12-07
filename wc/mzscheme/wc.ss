#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id: v4-script-template.ss 5748 2008-11-17 01:57:34Z erich $
exec  mzscheme --require "$0" --main -- ${1+"$@"}
|#

#lang scheme

(require
 (planet offby1/offby1/bfs)
 (planet offby1/offby1/set)
 (planet offby1/offby1/q)
 "dict.ss")

(define (display-result chain say-bummer?)
  (if chain
      (printf "~a: ~a~n"
              (length chain)
              (string-join chain " -> "))

      (when say-bummer?
        (display "Bummer.  No chain.")
        (newline))))

(define (go word-pair)
  (with-neato-output
   (lambda ()
     (apply bfs  (append word-pair (list string=?  all-neighbors))))))

(provide main)
(define (main . args)
  (if (= 2 (length args))
      (display-result (go args) #t)
      (let loop ()
        (display-result (go (random-word-pair 6)) #f)
        (loop))))

;;; For further reading:

;; http://www.policyalmanac.org/games/aStarTutorial.htm
