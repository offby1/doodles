(require 'interval)

(define (sum first how-many term)
  (apply + (map term (enumerate-interval first how-many))))

(define (my-expt x power)
  (if (or (not (integer? power))
	  (negative? power))
      (error power "is not a non-negative integer"))
  (let loop ((power power)
	     (result 1))
    (if (= power 0)
	result
      (loop (- power 1)
	    (* result x)))))

;; For x > 0
(define (my-log x)

  (define (odd i) (- (* 2 i) 1))

  (* 2
     (sum 1 1000 
          (lambda (i)
            (* (/ 1 (odd i))
               (my-expt (/ (- x 1)
                           (+ x 1))
                        (odd i))
               )))))

;; pi
;; (sqrt (* 24 (sum 1 10000 (lambda (i) (/ 1 4 i i)))))
;; (sqrt (* 6  (sum 1 10000 (lambda (i) (/ 1 i i)))))