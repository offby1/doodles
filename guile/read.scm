(define (list-of-lines fn)
  (call-with-input-file fn
    (lambda (port)
      (let loop ((thing (read-line port))
                 (result '()))
        (if (eof-object? thing)
            (reverse result)
          (loop (read-line port)
                (cons thing result)))))))
