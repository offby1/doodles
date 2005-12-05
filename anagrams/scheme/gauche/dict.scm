(define-module dict
  (use bag))

(select-module dict)

(define *ht* (make-hash-table 'string=?))
(define (word-acceptable? w)
  #t)
(call-with-input-file "/usr/share/dict/words"
  (lambda (p)
    (let loop ((line (read-line p #t)))
      (when (not (eof-object? line))
        (when (word-acceptable? line)
          (hash-table-put! *ht*  line (bag line)))
        (loop (read-line p #t))))))

(display "Dictionary has ")
(display (hash-table-num-entries *ht*))
(display " entries")
(newline)
(provide "dict")
