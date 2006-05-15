(module memoize mzscheme
(provide make-memoizer)
(define (make-memoizer)
  (let ((cache (make-hash-table 'equal)))
    (lambda (func)
      (lambda args
        (hash-table-get
         cache
         args
         (lambda ()
           (let ((new-value (apply func args)))
             (hash-table-put! cache args new-value)
             ;;(printf "Stored ~a~%" new-value)
             new-value))))))))
