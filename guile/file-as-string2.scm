(use-modules (ice-9 rdelim))

(define (file->string fn)
  (call-with-input-file fn
    (lambda (p)
      (let loop ((one-line (read-line p 'concat))
                 (result '()))
        (if (eof-object? one-line)
            (apply string-append (reverse result))
          (loop (read-line p 'concat)
                (cons one-line result)))))))
