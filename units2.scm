(define (linear-transforms scale offset)
    (cons
     
     ;; A linear function
     (lambda (x) (+ offset (* x scale)))
     
     ;; Its inverse
     (lambda (y) (/ (- y offset) scale))))

