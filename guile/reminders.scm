#!/usr/bin/guile -s
!#

(define now-seconds   (current-time))
(define now-localtime (localtime now-seconds))
(define now-year      (tm:year  now-localtime))
(define now-month     (tm:mon   now-localtime))
(define now-day       (tm:mday  now-localtime))
(define now-hour      (tm:hour  now-localtime))
(define now-minute    (tm:min   now-localtime))
(define now-second    (tm:sec   now-localtime))

;; Note: MONTH ranges from 1 to 12 inclusive, which is different from
;; the month argument to the Guile time functions; they range from 0
;; to 11 inclusive.  
(define (bdt year month day hours minutes seconds)
  (let ((return  (localtime (current-time))))

    (set-tm:year  return year)
    (set-tm:mon   return (- month 1))
    (set-tm:mday  return day)
    (set-tm:hour  return hours)
    (set-tm:min   return minutes)
    (set-tm:sec   return seconds)
       
    ;; We can't simply return RETURN is it is, because it has the same
    ;; day of the week as today, which isn't necessarily correct.
    (cdr (mktime return))))

(let ((some-date (bdt 98 10 5 10 4 0)))

  (if (>= now-seconds (car (mktime some-date)))
      (display "It is on or after ")
    (display "It is before      "))
  (display (strftime "%c" some-date))
  (newline))

(define (do-if-earlier date thunk)
  (if (< now-seconds (car (mktime date)))
      (thunk)))

(define (do-if-later date thunk)
  (if (> now-seconds (car (mktime date)))
      (thunk)))

(define (do-if-between start stop thunk)
  (if (and (>= now-seconds (car (mktime start)))
	   (<= now-seconds (car (mktime stop))))
      (thunk)))

(do-if-between (bdt 98 10 31 0 0 0)
	       (bdt 98 11  1 0 0 0)
	       (lambda (display "Happy Halloween")
		 (newline)))

(define (match year month day hour minute second)
  (set! month (- month 1))
  (and (or (not year  ) (= year  (tm:year  now-localtime)))
       (or (not month ) (= month (tm:mon   now-localtime)))
       (or (not day   ) (= day   (tm:mday  now-localtime)))
       (or (not hour  ) (= hour  (tm:hour  now-localtime)))
       (or (not minute) (= minute(tm:min   now-localtime)))
       (or (not second) (= second(tm:sec   now-localtime)))))

(if (match #f 10 #f #f #f #f)
    (begin
      (display "It's October.")
      (newline)))

(if (about-time #t #t #t 