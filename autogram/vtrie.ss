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
 (planet "util.ss"     ("schematics" "schemeunit.plt" 2))
 "byte-vector-counter.ss")
(define (make-vtrie)
  (box '()))

(define (is-present? vt thing)
  (member thing (unbox vt)))
(trace is-present?)
(define (add! vt thing)
  (set-box! vt (cons thing (unbox vt))))

(define *chars-of-interest* (string->list "boxiest"))
(exit-if-failed
 (let ((c1 (make-count *chars-of-interest*))
       (c2 (make-count *chars-of-interest*)))
   (test/text-ui
    (test-suite
     "The one and only suite"

     #:before
     (lambda ()
       (inc-count! #\b c1 2)
       (inc-count! #\x c2 3))

     (test-false
      "empty"
      (let ((thing (make-vtrie)))
        (is-present? thing c1)))
     (test-case
      "adding"
      (let ((thing (make-vtrie)))
        (add! thing c1)
        (check-not-false
         (is-present? thing c1))

        (add! thing c2)
        (check-not-false
         (and
          (is-present? thing c1)
          (is-present? thing c2)))))))))

)
