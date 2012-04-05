(module compact mzscheme
(require
 (planet "test.ss"     ("schematics" "schemeunit.plt" 2))
 (planet "text-ui.ss"  ("schematics" "schemeunit.plt" 2)))

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
            (cons (maybe-listify) result))))))

(test/text-ui
 (test-suite
  "The one and only suite"
  (test-case "empty"   (check-equal? (compact-sequence '()) '()))
  (test-case "no dups" (check-equal? (compact-sequence '(a b c)) '(a b c)))
  (test-case "one dup" (check-equal? (compact-sequence '(a a b c)) '((a 2) b c)))
  (test-case "two dups" (check-equal? (compact-sequence '(a a a b c)) '((a 3) b c)))
  (test-case "no dup because out of order"
             (check-equal? (compact-sequence '(a b a c))
                           '(a b a c)))
  (test-case "Not just the first"
             (check-equal? (compact-sequence '(a b b b c d))
                           '(a (b 3) c d)))

  (test-case "More than one"
             (check-equal? (compact-sequence '(a a a b c c c d))
                           '((a 3) b (c 3) d))))))
