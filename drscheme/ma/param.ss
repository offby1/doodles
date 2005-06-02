(module param mzscheme
  (provide *modulus*
           (rename m+ +) (all-from-except mzscheme +))
  (define *modulus* (make-parameter #f (lambda (value)
                                         (unless (or (and (integer? value)
                                                          (exact? value)
                                                          (< 1 value))
                                                     (not value))
                                           (raise-type-error '*modulus* "exact natural number > 1" value))
                                         value)))
  (define (maybe-modulo input)
    (if (*modulus*)
      (modulo input (*modulus*))
      input))
  (define m+
    (lambda args
      (maybe-modulo (apply + args)))))


