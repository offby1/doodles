(module modarith mzscheme
(require "invert.ss"
         (only (lib "1.ss" "srfi") fold))
(provide with-arithmetic-modulo
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
(define-syntax with-arithmetic-modulo
  (syntax-rules ()
    ((_ m body ...)
     (parameterize ((*modulus* m))
                   body ...))))

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
    (maybe-modulo
     (fold (lambda (x y)
             (maybe-modulo (+ x y)))
           0
           args))))
(define m*
  (lambda args
    (maybe-modulo
     (fold
      (lambda (x y)
        (maybe-modulo (* x y)))
      1
      args))))

(define m/
  (case-lambda
   [(x) (maybe-invert x)]
   [(a . any) (m* a (maybe-invert (apply m* any)))]))

(define m-
  (case-lambda
   [(x) (maybe modulo (- x))]
   [(a . any) (m+ a (- (apply m+ any)))]))
)
