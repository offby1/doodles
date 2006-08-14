;; The old-fasioned way
(let ((inner-calls 0)
      (found #f))
  (let loop ((x 100))
    (if (and
         (not (zero? x))
         (not found))
        (begin
          (let loop ((y 100))
            (if (and
                 (not (zero? y))
                 (not found))
                (begin
              
                  (if (and (= x 50)
                           (= y 50))
                      (set! found #t)
                    (begin
                      (set! inner-calls (+ 1 inner-calls))
                      (loop (- y 1)))))))
          
          (loop (- x 1)))))
     
  inner-calls)

;; The Scheme way
(call-with-current-continuation
 (lambda (return)
   (let ((inner-calls 0))
     (let loop ((x 100))
       (if (not (zero? x))
           (begin
             (let loop ((y 100))
               (if (not (zero? y))
                   (begin
              
                     (if (and (= x 50)
                              (= y 50))
                         (return inner-calls))
                     
                     (set! inner-calls (+ 1 inner-calls))
                     (loop (- y 1)))))
             (loop (- x 1))))))))
