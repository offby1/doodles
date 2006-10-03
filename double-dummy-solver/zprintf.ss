#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -qu "$0" ${1+"$@"}
|#

(module zprintf mzscheme

(provide (all-defined))
(require (only (lib "1.ss" "srfi") last-pair))
(define *recursion-level* (make-parameter 0))

(define (zprintf . args)
  (when (zero? (*recursion-level*))
    (apply printf args)
    (flush-output)))

;; handy for debugging -- wrap it around a call like this

;; (zp "is ~a held by ~a or ~a? ~a~%" c lho rho
;;     (or (member c (ha:cards lho))
;;         (member c (ha:cards rho))))

(define (zp . args)
  (apply zprintf args)
  (car (last-pair args)))
(define (p . args)
  (apply printf args)
  (car (last-pair args)))
)
