;; -*-scheme-*-
;; For testing Stalin.

(define (factorial n)
  (let loop ((n n)
             (result 1))
    (if (not (and (integer?  n)
                  (positive? n)))
        result
      (loop (- n 1)
            (* n result)))))

(display (factorial (string->number (vector-ref argv 1))))
(newline)