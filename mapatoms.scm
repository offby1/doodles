(define (map-atoms x fn)

  ;; apply function fn to each element in vector v, in order, and
  ;; return a list of the results.

  (define (vector-map v fn arg)
    (let loop ((n 0)
	       (so-far '()))
      (if (< n (vector-length v))
	  (loop (+ 1 n)
		(append (fn (vector-ref v n) arg)
			so-far))
	so-far)))

  (cond
   ((null? x)
    '())

   ((vector? x)
    (vector-map x map-atoms fn))

   ((pair? x)
    (append (map-atoms (car x) fn)
	    (map-atoms (cdr x) fn)))

   (else
    (list (fn x)))))
