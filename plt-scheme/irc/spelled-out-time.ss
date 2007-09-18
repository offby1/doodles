#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace --no-init-file --mute-banner --version --require "$0" -p "text-ui.ss" "schematics" "schemeunit.plt" -e "(exit (test/text-ui spelled-out-time-tests 'verbose))"
|#
(module spelled-out-time mzscheme
(require (lib "trace.ss")
         (planet "test.ss"    ("schematics" "schemeunit.plt" 2))
         (planet "util.ss"    ("schematics" "schemeunit.plt" 2))
         (planet "numspell.ss" ("neil" "numspell.plt")))

(define (s? n)
  (if (equal? 1 n)
      ""
    "s"))

(define (maybe n unit-name)
  (if (positive? n)
      (format ", ~a ~a~a"
              (number->english n)
              unit-name
              (s? n))
    ""
    ))

(define (number->english/plural n unit-name)
  (string-append (number->english n)
                 " "
                 unit-name
                 (s? n)))

(define (spelled-out-time seconds)
  (let* ((minutes (floor (/ seconds 60)))
         (hours   (floor (/ minutes 60)))
         (days    (floor (/ hours 24))))
    (cond
     ((positive? days)
      (format "~a~a"
              (number->english/plural days "day")
              (maybe (remainder hours 24) "hour")))
     ((positive? hours)
      (format "~a~a"
              (number->english/plural (remainder hours 24) "hour")
              (maybe (remainder minutes 60) "minute")))
     ((positive? minutes)
      (format "~a~a"
              (number->english/plural (remainder minutes 60) "minute")
              (maybe (remainder seconds 60) "second")))
     (else
      (format "~a"
              (number->english/plural seconds "second"))))))


(define spelled-out-time-tests

  (test-suite
   "spelled-out-time"
   (test-equal? "one second"          (spelled-out-time 1) "one second")
   (test-equal? "two seconds"         (spelled-out-time 2) "two seconds")
   (test-equal? "twenty-five seconds" (spelled-out-time 25) "twenty-five seconds")
   (test-equal? "two minutes, three seconds" (spelled-out-time 123) "two minutes, three seconds")
   (test-equal? "one hour"            (spelled-out-time 3611) "one hour")
   (test-equal? "two hours"           (spelled-out-time 7229) "two hours")
   (test-equal? "one day"             (spelled-out-time (+ 17 (* 24 3600))) "one day")))

(provide spelled-out-time spelled-out-time-tests)
)
