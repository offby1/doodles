;; Returns a list of those elements in L that appear more than once
;; *next to each other*.  That is, if L is (1 2 2 3 1), this returns
;; just (2).  (The 1s don't count because they're not next to each
;; other).

(define (duplicates seq equal?)

  (define (number-of-duplicated-heads seq)
    (let loop ((seq seq)
               (duplicates 0))
      (cond
       ((null? seq)
        duplicates)
       ((null? (cdr seq))
        duplicates)
       ((equal? (car seq)
                (cadr seq))
        (loop (cdr seq)
              (+ 1 duplicates)))
       (#t duplicates))))

  (let loop ((seq seq)
             (results '()))
    (if (or (null? seq)
            (null? (cdr seq)))
        results
      (let ((n (number-of-duplicated-heads seq)))
        (loop (list-tail seq (+ 1 n))
              (if (positive? n)
                  (cons (car seq) results)
                results))))))
