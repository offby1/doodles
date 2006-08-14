(module compact mzscheme
(require
 (planet "test.ss"     ("schematics" "schemeunit.plt" 1 1))
 (planet "text-ui.ss"  ("schematics" "schemeunit.plt" 1 1)))

(provide compact-sequence)

(define (compact-sequence s)
  (let loop ((s s)
             (current-item-count 1)
             (result '()))
    (define (maybe-listify)
      (if (< 1 current-item-count)
          (list (car s)
                current-item-count)
        (car s)))
    (cond
     ((null? s)
      (reverse result))
     ((null? (cdr s))
      (reverse (cons (maybe-listify) result)))
     ((equal? (car s)
              (cadr s))
      (loop (cdr s)
            (add1 current-item-count)
            result))
     (else
      (loop (cdr s)
            1
            (cons (maybe-listify) result)))))
  )

(test/text-ui
 (make-test-suite
  "The one and only suite"
  (make-test-case "empty"   (assert-equal? (compact-sequence '()) '()))
  (make-test-case "no dups" (assert-equal? (compact-sequence '(a b c)) '(a b c)))
  (make-test-case "one dup" (assert-equal? (compact-sequence '(a a b c)) '((a 2) b c)))
  (make-test-case "two dups" (assert-equal? (compact-sequence '(a a a b c)) '((a 3) b c)))
  (make-test-case "no dup because out of order"
                  (assert-equal? (compact-sequence '(a b a c))
                                 '(a b a c)))))
)