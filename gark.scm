(let ()
  (define (make-counter)
    (call-with-current-continuation
     (lambda (return)
       (let loop ((counter 0))
         (display counter) (newline)
         (let ((increment (call-with-current-continuation (lambda (rest) (return rest)))))

           (loop (+ increment counter)))))))

  (define counter (make-counter))
  (counter 1)
  (counter 1)
  (counter 1)
  )