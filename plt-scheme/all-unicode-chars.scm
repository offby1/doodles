#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id: v4-script-template.ss 5887 2008-12-30 18:12:50Z erich $
exec  mzscheme --require "$0" --main -- ${1+"$@"}
|#

;;; unicode chars range from 0 through #x10FFFF inclusive, except for
;;; #xd800 through #xdfff inclusive.

#lang scheme

(provide main)
(define (main . args)
  (printf "-*-coding:utf-8-*-~%")
  (for ([i (in-range 0 #x110000)])
    (unless (<= #xd800 i #xdfff)
      (let ((c  (integer->char i)))
        (printf "~a ~a ~%" c (char-general-category c))))))
