(use-modules (ice-9 threads))

(define (countdown)
  (let loop ((x 10))
    (if (positive? x)
        (begin
          (display x)
          (newline)
          (sleep 1)
          (loop (- x 1))))))

(make-thread countdown)
