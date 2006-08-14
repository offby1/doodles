(define (flatten thing)
  (cond
   ((null? thing)
    thing)
   ((pair? thing)
    (append (flatten (car thing))
            (flatten (cdr thing))))
   (#t (list thing))))