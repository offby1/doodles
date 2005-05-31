#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec mzscheme -qu "$0" ${1+"$@"}
|#

(module nad mzscheme
  (require (only (lib "1.ss" "srfi") lset-adjoin)
           (planet "test.ss" ("schematics" "schemeunit.plt" 1))
           (planet "text-ui.ss" ("schematics" "schemeunit.plt" 1)))
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
            (make-test-suite
             "aargh"
             (make-test-case
              "simple cases"
              (assert-equal? '() (non-adjacent-duplicates '()))
              (assert-equal? '() (non-adjacent-duplicates '(foo)))
              (assert-equal? '() (non-adjacent-duplicates '(foo bar)))
              (assert-equal? '() (non-adjacent-duplicates '(foo foo bar)))
              )
             (make-test-case "foo bar foo"     (assert-equal? '(foo) (non-adjacent-duplicates '(foo bar foo))))
             (make-test-case "bar foo foo bar" (assert-equal? '(bar) (non-adjacent-duplicates '(bar foo foo bar))))
             (make-test-case "yow!  yadda! ouch!" (assert-equal? '(foo bar) (non-adjacent-duplicates '(bar foo foo bar foo))))
             )))
    
    (exit 1)))