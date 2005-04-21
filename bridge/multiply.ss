(module multiply mzscheme
  (provide multiply)
  (define (distribute-1 atom lst)

    (if (null? lst)
        lst
      (map (lambda (elt)
             (cons atom elt))
           lst)))

  (define (distribute-3 lst lst-o-lst)

    (if (null? lst)
        '()
      (append
       (distribute-1 (car lst)
                     lst-o-lst)
       (distribute-3 (cdr lst)
                     lst-o-lst))))
  (define (multiply . args)
    (cond
     ((null? args)
      '())
     ((= 1 (length args))
      (map list (car args)))
     (#t
      (distribute-3 (car args) (apply multiply (cdr
                                                args))))))
  
  )