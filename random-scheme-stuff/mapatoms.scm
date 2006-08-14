(define (atoms x)

  (cond
   ((null? x)
    '())

   ((vector? x)
    (atoms (vector->list x)))

   ((pair? x)
    (append (atoms (car x) )
	    (atoms (cdr x) )))

   (else
    (list  x))))
