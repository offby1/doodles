#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec mzscheme -qr "$0" ${1+"$@"}
|#

(require "misc.ss"
         (lib "list.ss" "srfi" "1")
         (planet "test.ss" ("schematics" "schemeunit.plt" 1))
         (planet "text-ui.ss" ("schematics" "schemeunit.plt" 1))
         (planet "util.ss" ("schematics" "schemeunit.plt" 1)))
(define (make-one-test N seq)
  (let ((rv (group-by N seq)))
    (make-test-suite
     "grouping"
     (make-test-case "input equals output"
                     (assert-equal? (apply append rv) seq ))

     (let ((lengths (map length rv)))
       (make-test-suite
        "more"
        (make-test-case
         "no group is too large"
         (assert-equal? (apply max lengths) N ))

        (make-test-case
         "no group is too small"
         (assert-true (every positive? lengths) ))

        (make-test-case
         "properly ordered"
         (assert-true (or (< (length lengths) 2)
                      (apply >= lengths)) ))

        (make-test-case
         "at most one short group"
         (assert-true (< (length (remove (lambda (l) (= l N)) lengths)) 2) )))))))
(when
    (test/text-ui
     (make-test-suite "I dunno"
      (make-one-test 4 '(a b c d e f g h i))
      (make-one-test 3 '(a b c d e f g h i))
      (make-one-test 4 '(a b c d)))
     )
  (exit 0)
  (exit 1))