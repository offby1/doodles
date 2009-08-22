#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id: v4-script-template.ss 6086 2009-06-14 20:14:28Z erich $
exec  mzscheme -l errortrace --require "$0" --main -- ${1+"$@"}
|#

;; http://programmingpraxis.com/2009/08/21/string-search-brute-force/

#lang scheme
(require schemeunit
         schemeunit/text-ui
         mzlib/trace)

(define (search pattern string [offset 0])
  (cond
   ((equal? "" pattern)
    offset)
   ((equal? "" string)
    #f)
   ((< (string-length string) (+ offset (string-length pattern)))
    #f)
   ((equal? pattern (substring string offset (+ offset (string-length pattern))))
    offset)
   (else
    (search pattern string (add1 offset)))))

(trace search)
(define-test-suite search-tests

  (check-equal? (search "fred" "") #f)
  (check-equal? (search "" "fred") 0)
  (check-equal? (search "" "") 0)
  (check-equal? (search "fred" "fred") 0)
  (check-equal? (search "fred" "fred has two cats") 0)
  (check-equal? (search "fred" "if I'm fred, you must be mabel") 7)
  (check-equal? (search "fred" "Nobody named Fred here, nuh-uh") #f)

  (check-equal? (search "Programming Praxis" "Programming Praxis") 0)
  (check-equal? (search "Praxis" "Programming Praxis") 12)
  (check-equal? (search "Prax" "Programming Praxis") 12)
  (check-equal? (search "praxis" "Programming Praxis") #f)
  (check-equal? (search "P" "Programming Praxis") 0)
  (check-equal? (search "P" "Programming Praxis" 5) 12)
  )

(define (main . args)
  (exit (run-tests search-tests 'verbose)))
(provide search main)
