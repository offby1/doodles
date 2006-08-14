(define (enumerate-interval start how-many)
  (let loop ((result '())
             (start start)
             (how-many how-many))
    (if (zero? how-many)
         result
      (loop (append result (list start))
            (+ 1 start)
            (- how-many 1)))))

(provide 'interval)