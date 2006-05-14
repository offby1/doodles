;; for PLT scheme 301 (and probably earlier too)
(define memoize
  (let ((cache (make-hash-table 'equal)))
    (lambda (func)
      (lambda args
        (let  ((probe (hash-table-get cache args (lambda () #f))))
          (or probe
              (let ((new-value (apply func args)))
                (hash-table-put! cache args new-value)
                new-value)))))))

(define fib
  (memoize
   (lambda (x)
     (cond
      ((zero? x)
       0)
      ((= 1 x)
       1)
      (else
       (+
        (fib (- x 1))
        (fib (- x 2))))))))
