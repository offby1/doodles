(define (lwtp . args)
  (if (null? args)
      '()
    (cons 
     args  

     ;; find the tail.
     (let loop ((args args))
       (if (and (not (null? args))
                (not (null? (cdr args))))
           (loop (cdr args))
         args)))))

(define-syntax append!
  (syntax-rules ()
    ((_ l datum)
     (let ((new  (list datum)))
       (if (null? l)
           (set! l (cons new new))
         (let ((tail (cdr l)))
           (set-cdr! tail new)
           (set-cdr! l new)))
       l))))
