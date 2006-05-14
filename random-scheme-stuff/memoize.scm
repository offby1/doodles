;; for PLT scheme 301 (and probably earlier too)
(define memoize
  (let ((cache (make-hash-table 'equal)))
    (lambda (func)
      (lambda args
        (hash-table-get
         cache
         args
         (lambda ()
           (let ((new-value (apply func args)))
             (hash-table-put! cache args new-value)
             (printf "Stored ~a~%" new-value)
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
