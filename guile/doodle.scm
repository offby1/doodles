(let ((output (list '(#f)
                    '(#f)
                    '(#f)
                    '(#f))))
  (define (emptiest lol)
    (define (emptier s1 s2)
      (cond
       ((null? s1)
        s2)
       ((null? s2)
        s1)
       (#t
        (let ()
          (define (sum seq)
            (let loop ((seq seq)
                       (result 0))
              (if (not (car seq))
                  result
                (loop (cdr seq)
                      (+ result (car seq))))))
          (< (sum s1)
             (sum s2))))))
    (let loop ((lol lol)
               (return '()))
      (if (null? lol)
          return
        (loop (cdr lol)
              (if (emptier (car lol)
                           return)
                  (car lol)
                return)))))
  (define (add-to-list! datum lst)
    (set-car! lst (cons datum lst)))
  (let loop ((input (list 1 2 3 4 5)))
    (if (null? input)
        output
      (begin
        (add-to-list! (car input) (emptiest output))
        (loop (cdr input))))))
