(require 'sort)
(require 'factor)

(let loop ((x 1))
  (if (< x 100)
      (begin
	(write x)

	(let ((factors (factor x)))
	  (if (null? (cdr factors))
	      (display " is prime")
	      (begin
		(display "'s factors are ")
		(write (sort factors
			     <)))))
	(newline)
	(loop (+ 1 x)))))
