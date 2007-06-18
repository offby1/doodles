#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec mzscheme -qu "$0" ${1+"$@"}
|#
(module modarith-test "modarith.ss"
(require  (planet "test.ss" ("schematics" "schemeunit.plt" 2))
          (planet "text-ui.ss" ("schematics" "schemeunit.plt" 2))
          )

(when
    (not
     (test/text-ui
      (test-suite
       "Tests for modular arithmetic."

       (test-case
        "+"
        (check = 10 (+ 8 2))
        (with-arithmetic-modulo  7
          (check = 3 (+ 8 2)))
        )

       (test-case
        "-"
        (check = -4 (- 4))
        (with-arithmetic-modulo  5
          (check = 1 (- 4))
          (check = 1 (- 4 3))))

       (test-case
        "/"
        (check = 1/4 (/ 4))
        (check = 4/30 (/ 4 5 6))
        (check = 2/12 (/ 2 3 4))
        (with-arithmetic-modulo  5
          (check = 4 (/ 4))
          (check = 3 (/ 2))
          (check = 2 (/ 3))
          (check = 1 (/ 1))
          (check = 4 (/ 4 6))
          (check = 1 (/ 2 3 4)))
        (with-arithmetic-modulo  7
          (check = 6 (/ 2 3 4))))
       )))
  (exit 1)))