; To make a pascal's triangle of order 0, return ((1)) .

; To make a pascal's triangle of order n > 0, return pascal's triangle
; of order n-1, plus a new row which contains the sums of the
; interstices of the previous row.

(define (pascal order)

  (define (new-row old-row)
    (let loop ((result (list (car old-row)))
	       (old-row old-row))
      (cond ((null? old-row)
	     result)
	    ((= (length old-row)
		1)
	     (append result (list (car old-row))))
	    (#t (loop (append result (list (+ (car old-row) (cadr old-row))))
		      (cdr old-row))))))

  (define (last list)
    (if (null? (cdr list))
	(car list)
      (last (cdr list))))

  (if (>= 0 order)
      '((1))
    (let ((smaller-triangle (pascal (- order 1))))
      (append smaller-triangle
	      (list (new-row (last smaller-triangle)))))))