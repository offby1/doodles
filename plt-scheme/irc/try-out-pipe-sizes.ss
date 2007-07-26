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

