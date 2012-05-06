#lang racket

(require rackunit
         unstable/debug)

(define (multiset seq)
  (for/fold ([rv (make-immutable-hash)])
      ([elt seq])
      (hash-update rv elt add1 0)))

(define (score counts-by-number)
  (let ([counts-by-number (make-hash (dict-map counts-by-number cons))]
        [total 0])

    ;; three ones: 1,000

    ;; three of anything else: 100 * n
    (for ([(num count) (in-hash counts-by-number)])
      (when (<= 3 count)
        (hash-update! counts-by-number num (curryr - 3))
        (set! total (+ total (* num (if (= num 1) 1000 100))))))

    ;; leftover ones: 100 apiece
    (set! total (+ total (* 100 (hash-ref counts-by-number 1 0))))
    ;; leftover fives: 50 apiece
    (set! total (+ total (* 50  (hash-ref counts-by-number 5 0))))

    total))

(define-simple-check (check-score throws expected-score)
  (check-equal? (score (apply multiset throws)) expected-score))

(check-equal? (score (multiset '[1 1 1 5 1])) 1150)
(check-equal? (score (multiset '[2 3 4 6 2]))    0)
(check-equal? (score (multiset '[3 4 5 3 3]))  350)
(check-equal? (score (multiset '[1 5 1 2 4]))  250)
