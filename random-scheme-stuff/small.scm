; A recursive definition...
(define (smallest ls)
  (cond
   ((null? (cdr ls))
    (car ls))
   (else
    (min (car ls)
	 (smallest (cdr ls))))))

; An iterative definition.
(define (smallest ls)
  (let loop ((so-far (car ls))
	     (ls ls))
    (cond
     ((null? (cdr ls))
      so-far)
     (else
      (loop (min (car ls)
		 so-far)
	    (cdr ls))))))