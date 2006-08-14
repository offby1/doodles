(define (mand Z iters)

  (define (infinite? X)
    (and (number? X)
	 (= X (+ 1 X))))

  (let loop ((result 0)
             (iters iters))
    (if (or
         (> (magnitude result) 2)
         (= iters 0))
        result
      (loop (+ Z (* result result))
            (- iters 1)))))
