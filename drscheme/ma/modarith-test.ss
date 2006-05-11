#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec mzscheme -qu "$0" ${1+"$@"}
|#
(module modarith-test "modarith.ss"
(require  (planet "test.ss" ("schematics" "schemeunit.plt" 1))
          (planet "text-ui.ss" ("schematics" "schemeunit.plt" 1))
          )
(when
    (not
     (test/text-ui
      (make-test-suite
       "Tests for modular arithmetic."

       (make-test-case
        "+"
        (assert = 10 (+ 8 2))
        (with-arithmetic-modulo  7
          (assert = 3 (+ 8 2))))

       (make-test-case
        "-"
        (assert = -4 (- 4))
        (with-arithmetic-modulo  5
          (assert = 1 (- 4))
          (assert = 1 (- 4 3))))

       (make-test-case
        "/"
        (assert = 1/4 (/ 4))
        (assert = 4/30 (/ 4 5 6))
        (assert = 2/12 (/ 2 3 4))
        (with-arithmetic-modulo  5
          (assert = 4 (/ 4))
          (assert = 3 (/ 2))
          (assert = 2 (/ 3))
          (assert = 1 (/ 1))
          (assert = 4 (/ 4 6))
          (assert = 1 (/ 2 3 4)))
        (with-arithmetic-modulo  7
          (assert = 6 (/ 2 3 4))))

       (make-test-case
        "macro"
        (with-arithmetic-modulo
         3
         'ok
         (assert = 0 (+ 2 1)))))))
  (exit 1)))