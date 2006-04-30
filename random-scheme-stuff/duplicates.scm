;; Returns a list of those elements in L that appear more than once
;; *next to each other*.  That is, if L is (1 2 2 3 1), this returns
;; just (2).  (The 1s don't count because they're not next to each
;; other).

(define (duplicates seq equal?)
  (let loop ((input seq)
             (dups '()))
    (if (or (null? input)
            (null? (cdr input)))
        (reverse dups)
      (if (equal? (car input)
                  (cadr input))
          (loop (cddr input)
                (cons (car input)
                      dups))
        (loop (cdr input)
              dups)))))
