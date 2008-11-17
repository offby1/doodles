#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id: v4-script-template.ss 5748 2008-11-17 01:57:34Z erich $
exec  mzscheme --require "$0" --main -- ${1+"$@"}
|#

#lang scheme

(require
 (planet "bfs.ss"  ("offby1" "offby1.plt"))
 (planet "set.ss"  ("offby1" "offby1.plt"))
 (planet "q.ss"    ("offby1" "offby1.plt"))
 "dict.ss")

(define (display-result chain say-bummer?)
  (cond
   (chain => (lambda (chain)
               (printf "~a: ~a~n"
                       (length chain)
                       (string-join chain " -> "))))
   (else
    (when say-bummer?
      (display "Bummer.  No chain.")
      (newline)))))

(define (go word-pair)
  (with-neato-output
   (lambda ()
     (apply bfs  (append word-pair (list string=?  all-neighbors))))))

(let ((ccla (vector->list (current-command-line-arguments))))
  (if (= 2 (length ccla))
      (display-result (go ccla) #t)
      (let loop ()
        (display-result (go (random-word-pair 6)) #f)
        (loop))))

;;; For further reading:

;; http://www.policyalmanac.org/games/aStarTutorial.htm
