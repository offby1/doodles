(require 'random)

(define (mean numbers)
  (/ (apply + numbers)
     (length numbers)))

(define (standard-deviation numbers)
  (if (and
       (list? numbers)
       (> (length numbers) 1))
      (let ((m (mean numbers)))
        (sqrt (/ (apply + (map (lambda (n)
                                 (expt (- m n) 2))
                               numbers))
                 (- (length numbers)
                    1))))
    #f))

(define (mean-deviation numbers)
  (and (list? numbers)
       (let ((m (mean numbers)))
         (/ (apply + (map (lambda (n)
                            (abs (- m n)))
                          numbers))
            (length numbers)))))

(define (make-list K fill-proc)
  (let ((return (vector->list (make-vector K))))
    (let loop ((return return))
      (if (not (null? return))
          (begin
            (set-car! return (fill-proc))
            (loop (cdr return)))))
    return))

(define (throw-dice)
  (+ 2 (random 6) (random 6)))

(define (histogram numbers)
  (let ((return (make-vector (+ 1 (apply max numbers)) 0)))
    (let loop ((numbers numbers))
      (if (not (null? numbers))
        (let ()

          (define (vector-increment! VECTOR K)
            (let ((old-value (vector-ref VECTOR K)))
              (vector-set! VECTOR K (+ 1 old-value))))

          (vector-increment! return (car numbers))
          (loop (cdr numbers)))))
    return))
