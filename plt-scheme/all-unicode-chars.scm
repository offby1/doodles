;;; unicode chars range from 0 through #x10FFFF inclusive, except for
;;; #xd800 through #xdfff inclusive.

#lang scheme
(printf "-*-coding:utf-8-*-~%")
(let loop ((chars-considered 0))
  (when (< chars-considered #x110000)
    (unless (<= #xd800 chars-considered #xdfff)
      (let ((c  (integer->char chars-considered)))
      (printf "~a ~a ~%"c  (char-general-category c))))
    (loop (+ 1 chars-considered))))
