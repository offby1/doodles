(module param mzscheme
  (require "invert.ss")
  (provide *modulus*
           (rename m+ +)
           (rename m- -)
           (rename m* *)
           (rename m/ /)
           (all-from-except mzscheme + - * /))
  (define *modulus* (make-parameter #f (lambda (value)
                                         (unless (or (and (integer? value)
                                                          (exact? value)
                                                          (< 1 value))
                                                     (not value))
                                           (raise-type-error '*modulus* "exact natural number > 1" value))
                                         value)))
  (define-syntax maybe
    (syntax-rules ()
      ((_ op input)
       (if (*modulus*)
           (op input (*modulus*))
         input))))
  
  (define (maybe-modulo input)
    (maybe modulo input))

  (define (maybe-invert input)
    (if (*modulus*)
        (invert input (*modulus*))
      (/ input)))
  
  (define m+
    (lambda args
      (maybe-modulo (apply + args))))

  (define m*
    (lambda args
      (maybe-modulo (apply * args))))
  
  (define m/
    (case-lambda
      [(x) (maybe-invert x)]
      [(a . any) (m* a (maybe-invert (apply m* any)))]))

  (define m-
    (case-lambda
      [(x) (maybe modulo (- x))]
      [(a . any) (m+ a (- (apply m+ any)))]))
  )
