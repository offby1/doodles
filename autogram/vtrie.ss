#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module vtrie mzscheme
(require
 (lib "assert.ss" "offby1")
 (lib "trace.ss")
 (planet "test.ss"     ("schematics" "schemeunit.plt" 2))
 (planet "text-ui.ss"  ("schematics" "schemeunit.plt" 2))
 (planet "util.ss"     ("schematics" "schemeunit.plt" 2)))
(define (make-vtrie)
  (box '()))

(define (is-present? vt thing)
  (member thing (unbox vt)))
(trace is-present?)
(define (add! vt thing)
  (set-box! vt (cons thing (unbox vt))))

(exit-if-failed
 (test/text-ui
  (test-suite
   "The one and only suite"

   (test-false
    "empty"
    (let ((thing (make-vtrie)))
      (is-present? thing '(a))))
   (test-case
    "adding"
    (let ((thing (make-vtrie)))
    (add! thing '(a))
    (check-not-false
     (is-present? thing '(a)))

    (add! thing '(b))
    (check-not-false
     (and
      (is-present? thing '(a))
      (is-present? thing '(b))))

    (add! thing '(a b))
    (check-not-false
     (and
      (is-present? thing '(a))
      (is-present? thing '(b))
      (is-present? thing '(a b))))
    )))))

)