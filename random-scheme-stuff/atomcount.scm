(define (atom-count x)

  ;; apply function f to each element in vector v, in order, and
  ;; return a list of the results.

  (define (vector-map v fn)
    (list->vector (map fn (vector->list v))))

  (cond
   ((null? x)
    0)

   ((vector? x)
    (apply + (vector-map x atom-count)))

   ((pair? x)
    (+ (atom-count (car x))
       (atom-count (cdr x))))

   (else
    ;;(display-many x #\newline)
    1)))
