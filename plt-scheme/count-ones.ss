(define/contract (count-ones n [accumulator 0])
  (natural-number/c . -> . natural-number/c)
  (cond
   ((< n 2)
    (+ n accumulator))
   ((odd? n)
    (count-ones (/ (sub1 n) 2) (add1 accumulator)))
   (else
    (count-ones (/ n 2) accumulator))))
