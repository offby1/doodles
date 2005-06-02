(module param-test "param.ss"
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
          (parameterize ((*modulus* 6))
            (assert = 6 (*modulus*)))
          (assert-false (*modulus*))
          (assert = 10 (+ 8 2))
          (parameterize ((*modulus* 7))
            (assert = 3 (+ 8 2))))
         )))
    (exit 1)))