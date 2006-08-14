(define (proper-fraction n)
  (define (nicely-append s1 s2)
    (string-append
     s1
     (if (or (zero? (string-length s1))
             (zero? (string-length s2)))
         ""
       " ")
     s2))

  (define (num->string-supressing-zero n)
    (if (zero? n)
        ""
      (number->string n)))
  (if (negative? n)
      (string-append "-" (proper-fraction (- n)))
    
    (let ((num  (numerator n))
          (den  (denominator n)))
      (nicely-append
       (num->string-supressing-zero (quotient num den))
       (num->string-supressing-zero (/ (remainder num den)
                                       den))))))
