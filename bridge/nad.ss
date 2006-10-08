#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec mzscheme -qu "$0" ${1+"$@"}
|#

(module nad mzscheme
  (require (only (lib "1.ss" "srfi") lset-adjoin)
           (planet "test.ss" ("schematics" "schemeunit.plt" 2))
           (planet "text-ui.ss" ("schematics" "schemeunit.plt" 2)))
  (provide non-adjacent-duplicates)

  (define (non-adjacent-duplicates l)
    (let loop ((input l)
               (seen '())
               (last #f)
               (result '()))
      (if (null? input)
          result
        (let ((this (car input)))
          (loop (cdr input)
                (lset-adjoin eq? seen this)
                this
                (if (and (memq this seen)
                         (not (eq? last this)))
                    (lset-adjoin eq? result this)
                  result))))))

  (when
      (not (test/text-ui
            (test-suite
             "aargh"
             (test-case
              "simple cases"
              (check-equal? '() (non-adjacent-duplicates '()))
              (check-equal? '() (non-adjacent-duplicates '(foo)))
              (check-equal? '() (non-adjacent-duplicates '(foo bar)))
              (check-equal? '() (non-adjacent-duplicates '(foo foo bar)))
              )
             (test-case "foo bar foo"     (check-equal? '(foo) (non-adjacent-duplicates '(foo bar foo))))
             (test-case "bar foo foo bar" (check-equal? '(bar) (non-adjacent-duplicates '(bar foo foo bar))))
             (test-case "yow!  yadda! ouch!" (check-equal? '(foo bar) (non-adjacent-duplicates '(bar foo foo bar foo))))
             )))
    
    (exit 1)))