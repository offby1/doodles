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
          "parameter"
          (parameterize ((*modulus* 6))
            (assert = 6 (*modulus*)))
          (assert-false (*modulus*)))

         (make-test-case
          "+"
          (assert = 10 (+ 8 2))
          (parameterize ((*modulus* 7))
            (assert = 3 (+ 8 2))))

         (make-test-case
          "-"
          (assert = -4 (- 4))
          (parameterize ((*modulus* 5))
            (assert = 1 (- 4))
            (assert = 1 (- 4 3))))
         
         (make-test-case
          "/"
          (assert = 1/4 (/ 4))
          (assert = 4/30 (/ 4 5 6))
          (assert = 2/12 (/ 2 3 4))
          (parameterize ((*modulus* 5))
            (assert = 4 (/ 4))
            (assert = 3 (/ 2))
            (assert = 2 (/ 3))
            (assert = 1 (/ 1))
            (assert = 4 (/ 4 6))
            (assert = 1 (/ 2 3 4)))
          (parameterize ((*modulus* 7))
            (assert = 6 (/ 2 3 4))))
         )))
    (exit 1)))