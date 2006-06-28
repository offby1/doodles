(define (fast-iterative-exponent b n)
  (let loop ((b b)
             (n n)
             (accumulator 1))
    (cond
     ((zero? n)
      accumulator)
     ((even? n)
      (loop (* b b)
            (/ n 2)
            accumulator))
     (else
      (loop b
            (- n 1)
            (* accumulator b))))))
