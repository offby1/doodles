(define (factorial i)
  (define (inner i f)
    (if (zero? i)
        f
      (inner (- i 1)
             (* i f))))

  ;; This actually makes it slower.
  ;;(memoize! inner)

  (inner i 1))

(define (square x)
  (* x x))

(time
 (let loop ((i 0)
            (sum 0))
   (if (< i 5000)
       (loop (+ i 1)
             (+ sum (/ 
                     (* (square (factorial i))
                        (expt 2 (+ i 1)))
                     (factorial (+ i i 1))
                     )))
     (printf "~a~%" (exact->inexact sum)))))