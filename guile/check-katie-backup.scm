#!/usr/bin/guile -s
!#

;; Checks that katie's Windows PC has recently been backed up.  It
;; does this by looking at the timestamp on
;; /katie-local/WINDOWS/WIN386.SWP -- if that file was modified early
;; this morning, then things are OK; if it's older, then things are
;; not OK.


(define filename "/katie-local/WINDOWS/WIN386.SWP")

(define ok? 
  (let* ((age-in-seconds (- (current-time) (stat:mtime (stat filename))))
         (age-in-hours (/ age-in-seconds 3600)))
    (< age-in-hours 24)))

(begin
  (write filename)
  (if (not ok?)
      (display " is more than a day old")
    (display " is nice and fresh"))
  (newline))

