;; formulas are from the CRC Standard Mathematical Tables, 23d edition

(define (find-best-fit-line points)
  (define (square x) (* x x))

  (let* ((xs (map car points))
         (ys (map cdr points))
         (sum-of-xs (apply + xs))
         (sum-of-ys (apply + ys))
         (n (length points))
         (mean-of-xs (/ sum-of-xs n))
         (mean-of-ys (/ sum-of-ys n))

         (slope (/ (- (* n 
                      ;; sum (x_i * y_i)
                      (apply + (map (lambda (point)
                                      (* (car point)
                                         (cdr point)))
                                    points))
                      )
                   (* sum-of-xs sum-of-ys))
                (- (* n (apply + (map square xs)))
                   (square sum-of-xs))))

         (y-intercept (- mean-of-ys (* slope mean-of-xs)))

         (correlation-coefficient
          (/
            
           (apply + (map
                     (lambda (point) (* (- (car point) mean-of-xs) (- (cdr point) mean-of-ys)))
                     points))
           (sqrt (* (apply + (map (lambda (x)
                                    (square (- x mean-of-xs)))
                                  xs))
                    (apply + (map (lambda (y)
                                    (square (- y mean-of-ys)))
                                  ys)))))))
    
    (list y-intercept slope correlation-coefficient)))
