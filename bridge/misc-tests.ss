#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec mzscheme -qr "$0" ${1+"$@"}
|#

(require "misc.ss"
         (lib "list.ss" "srfi" "1")
         (planet "test.ss" ("schematics" "schemeunit.plt" 2))
         (planet "text-ui.ss" ("schematics" "schemeunit.plt" 2))
         (planet "util.ss" ("schematics" "schemeunit.plt" 2)))
(define (make-one-test N seq)
  (let ((rv (group-by N seq)))
    (test-suite
     "grouping"
     (test-case "input equals output"
                     (check-equal? (apply append rv) seq ))

     (let ((lengths (map length rv)))
       (test-suite
        "more"
        (test-case
         "no group is too large"
         (check-equal? (apply max lengths) N ))

        (test-case
         "no group is too small"
         (check-true (every positive? lengths) ))

        (test-case
         "properly ordered"
         (check-true (or (< (length lengths) 2)
                      (apply >= lengths)) ))

        (test-case
         "at most one short group"
         (check-true (< (length (remove (lambda (l) (= l N)) lengths)) 2) )))))))
(when
    (zero?
     (test/text-ui
      (test-suite "I dunno"
                  (make-one-test 4 '(a b c d e f g h i))
                  (make-one-test 3 '(a b c d e f g h i))
                  (make-one-test 4 '(a b c d)))
      ))
  (exit 0)
  (exit 1))