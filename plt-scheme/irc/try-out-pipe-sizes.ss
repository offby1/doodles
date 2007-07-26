#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec mzscheme -qu "$0" ${1+"$@"}
|#

(module try-out-pipe-sizes mzscheme
(require
 (lib "pretty.ss")
  "jordanb.ss")

(parameterize ((*cache-file-name* #f))
              (pretty-print
               (map
                (lambda (pipe-size)
                  (parameterize ((*pipe-max-bytes* pipe-size))
                                (let-values (((rvs cpu-ms wallclock-ms gc-cpu-ms)
                                              (time-apply one-jordanb-quote-no-memoizing '())))
                                  (format
                                   "pipe size ~a bytes takes ~a cpu milliseconds, of which ~a is in gc"
                                   pipe-size
                                   cpu-ms
                                   gc-cpu-ms))))
                (list #f 1000 100 10 1)
                ))))


;; ("pipe size #f bytes takes 158381 cpu milliseconds, of which 3608 is in gc"
;;  "pipe size 1000 bytes takes 174623 cpu milliseconds, of which 15326 is in gc"
;;  "pipe size 100 bytes takes 198361 cpu milliseconds, of which 16629 is in gc"
;;  "pipe size 10 bytes takes 361694 cpu milliseconds, of which 21228 is in gc"
;;  "pipe size 1 bytes takes 1657324 cpu milliseconds, of which 35413 is in gc")
