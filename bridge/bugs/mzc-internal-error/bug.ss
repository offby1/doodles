(require (planet "test.ss" ("schematics" "schemeunit.plt" 1)))

(define (make-one-test N seq)
  (let ((rv (group-by N seq)))
    (make-test-suite
     "grouping"
     (make-test-case "input equals output"
                     (assert-equal? (apply append rv) seq ))


     )))
