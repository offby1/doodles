#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id: v4-script-template.ss 6086 2009-06-14 20:14:28Z erich $
exec  mzscheme -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang scheme
(require schemeunit
         schemeunit/text-ui
         mzlib/trace)

(define (search pattern string [offset 0])
  (let ((string (substring string offset)))
    (cond
     ((equal? "" pattern)
      offset)
     ((equal? "" string)
      #f)
     (else
      (let [(p (string-ref pattern 0))
            (s (string-ref string 0))]
        ;; (printf "p: ~a; s: ~a~%" p s)
        (cond
         ((char=? p s)
          (search (substring pattern 1)
                  (substring string 1)
                  0))
         (else
          (search pattern (substring string 1) 0))))))))
(trace search)
(define-test-suite search-tests

  (check-equal? (search "fred" "") #f)
  (check-equal? (search "" "fred") 0)
  (check-equal? (search "" "") 0)
  (check-equal? (search "fred" "fred") 0)
  (check-equal? (search "fred" "fred has two cats") 0)
  (check-equal? (search "fred" "if I'm fred, you must be mabel") 7)

  )

(define (main . args)
  (exit (run-tests search-tests 'verbose)))
(provide search main)
