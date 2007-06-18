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

        ;; make sure we have broken regular addition.
        (check-equal? (+ 8 2) 10)

        (with-arithmetic-modulo  7
          (check-equal? (+ 8 2) 3))
        )

       (test-case
        "-"
        (check-equal? (- 4) -4)
        (with-arithmetic-modulo  5
          (check-equal? (- 4) 1)
          (check-equal? (- 4 3) 1)))

       (test-case
        "/"
        (check-equal? (/ 4) 1/4)
        (check-equal? (/ 4 5 6) 4/30)
        (check-equal? (/ 2 3 4) 2/12)
        (with-arithmetic-modulo  5
          (check-equal? (/ 4) 4)
          (check-equal? (/ 2) 3)
          (check-equal? (/ 3) 2)
          (check-equal? (/ 1) 1)
          (check-equal? (/ 4 6) 4)
          (check-equal? (/ 2 3 4) 1))
        (with-arithmetic-modulo  7
          (check-equal? (/ 2 3 4) 6)))

       (test-case
        "exponents"
        (check-equal? (expt 2 3) 8)
        (with-arithmetic-modulo 5
          (check-equal? (expt 2 0) 1)
          (check-equal? (expt 2 1) 2)
          (check-equal? (expt 2 4) 1)
          (check-equal? (expt 2 3) 3))
        (with-arithmetic-modulo 23
          (check-equal? (expt 47 17) 1))
        )
       )))
  (exit 1)))