(define (a . seqs)
  (define (append2 a b)
    (cond
     ((null? b) a)
     ((null? a) b)
     (else
      (cons (car a)
            (append2 (cdr a)
                     b)))))
  (if (null? seqs)
      '()
    (append2 (car seqs)
             (apply a (cdr seqs)))))
