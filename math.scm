(lambda ()
  (define (improve-guess function derivitave initial-guess)
    (- initial-guess (/ (function initial-guess)  (derivitave initial-guess))))

  (define (repeat fun-of-one-arg iterations initial-input)
    (if (not (and
              (integer? iterations)
              (positive? iterations)
              (exact? iterations)))
        (error "iterations must be an exact positive integer, but is"
               iterations))
  
    (let ((verbose-fun-of-one-arg
           (lambda (x)
             (let ((result (fun-of-one-arg x)))
               (display result)
               (newline)
               result))))
    
      (let loop ((result initial-input)
                 (iterations iterations))
        (if (zero? iterations)
            result
          (let ((new-result (verbose-fun-of-one-arg result)))
            (if (equal? new-result result)
                result
              (loop new-result
                    (- iterations 1))))))))
  (let ((fun
         (lambda (x)
           (- (* x x)
              x
              10)))
        (derivative
         (lambda (x)
           (- (* 2 x)
              1))))
    (let ((answer
           (repeat (lambda (x) (improve-guess fun derivative x))
                   40 10.)))
      (list answer (fun answer)))))