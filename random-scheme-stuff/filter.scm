(define (flatmap proc seq)
  (define (accumulate proc initial list)
    (if (null? list)
        initial
      (proc initial (accumulate proc (car list) (cdr list)))))
  (accumulate append '() (map proc seq)))

(define (filter predicate seq)
  (let loop ((result '())
             (seq seq))
    (if (null? seq)
        (reverse result)
      (loop (if (predicate (car seq))
                (cons (car seq)
                      result)
              result)
            (cdr seq)))))

(provide 'filter)