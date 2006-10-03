#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -qu "$0" ${1+"$@"}
|#

;; find _either_ the first 1, or else the maximum value.
(define (max-or-1 seq)
  (if (null? seq)
      (error 'max-or-1 "wanted non-empty list; got '()")
    (let ((m (car seq)))
      (if (null? (cdr seq))
          m
        (if (= 1 m)
            m
          (max m (max-or-1 (cdr seq))))))))
(trace max-or-1)

;; compare
(max-or-1 '(0 -1  0 0 -1 0 0 0 0 0 0 0 0 0 0 0 -1))

;; with
(max-or-1 '(0 -1 1 0 0 -1 0 0 0 0 0 0 0 0 0 0 0 -1))

;; the second finishes faster.
