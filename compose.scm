(define (plus-one x)
  (+ 1 x))

(define (times-two x)
  (* 2 x))

(define (compose . fns)
  (cond
   ((null? fns)
    (lambda (x)
      x))
   (else
    (lambda (x)
      ((car fns)
       ((apply compose (cdr fns)) x))))))

((compose times-two plus-one times-two) 10)


;; => 42
