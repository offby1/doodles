#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module odometer mzscheme
(require
 (lib "assert.ss" "offby1")
 (lib "trace.ss")
 (planet "test.ss"     ("schematics" "schemeunit.plt" 2))
 (planet "text-ui.ss"  ("schematics" "schemeunit.plt" 2))
 (planet "util.ss"     ("schematics" "schemeunit.plt" 2)))
(provide increment)
(define (increment numbers min max)

  (define (new-value x min max)
    (assert (<= min x max))
    (+ min (modulo (add1 (- x min)) (- max min -1))))

                                        ;(trace new-value)

  (cond
   ((null? numbers)
    '())
   ((and
     (null? (cdr numbers))
     (= min (new-value (car numbers) min max)))
    #f)
   ((= min (new-value (car numbers) min max))
    (let ((from-recursive-call (increment (cdr numbers) min max)))
      (and from-recursive-call
           (cons(new-value (car numbers) min max) from-recursive-call))))
   (#t
    (cons (new-value (car numbers) min max)
          (cdr numbers)))

   ))
;(trace increment)
(exit-if-failed
 (test/text-ui
  (test-suite
   "The one and only suite"

   (test-true
    "null"
    (null? (increment '() 3 4)))
   (test-equal?
    "simplest"
    (increment '(2 3) 0 99)
    '(3 3))
   (test-equal?
    "simplest"
    (increment '(2) 0 3)
    '(3))
   (test-equal?
    "simple rollover"
    (increment '(99 3) 0 99)
    '(0 4))
   (test-equal?
    "cascading rollover 2"
    (increment '(9 9 9 0) 0 9)
    '(0 0 0 1))
   (test-equal?
    "non-zero minimum, no rollover"
    (increment '(2 3) 1 9)
    '(3 3))
   (test-equal?
    "non-zero minimum w/rollover"
    (increment '(9 3) 1 9)
    '(1 4))
   (test-false
    "maxed out"
    (increment '(5) 1 5))
    )
  ))
)
