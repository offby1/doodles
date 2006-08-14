(define (copy thing)
  (cond
   ((pair?   thing) (cons (copy (car thing)) (copy (cdr thing))))
   ((string? thing) (string-copy thing))
   ((vector? thing) (apply vector (map copy (vector->list thing))))
   (#t thing)))
