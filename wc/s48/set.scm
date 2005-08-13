(define make-set
  (lambda words
    (let ((rv (make-string-table)))
      (for-each (lambda (word) (add! word rv))
                words)
      rv)))

(define (is-present? word set)
  (table-ref set word))

(define (add! word set)
  (table-set! set word #t)
  set)