(define (rev seq)
  (let loop ((seq seq)
             (result '()))
    (if (null? seq)
        result
      (loop (cdr seq)
            (cons (car seq) result)))))

(define (append2 a b)
  (if (null? b) a
    (let loop ((a (rev a))
               (result  b))
      (cond
       ((null? a)
        result)
       (else
        (loop (cdr a)
              (cons (car a) result)))))))

(define (a . seqs)
  (if (null? seqs)
      '()
    (append2 (car seqs)
             (apply a (cdr seqs)))))
