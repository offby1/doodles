#!/usr/local/bin/guile -s
!#

;; When my wristwatch doesn't work, this is how I set a 30-minute
;; timer.

(sleep (* 60 20))

(let loop ()
  (display (integer->char 7))
  (display "Take the beans off the stove!")
  (newline)
  (sleep 1)
  (loop))
