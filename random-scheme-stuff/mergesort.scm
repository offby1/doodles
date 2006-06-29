(module mergesort mzscheme

(define (split seq)
  (let loop ((seq seq)
             (left '())
             (right '())
             (left? #t))
    (if (null? seq)
        (values left right)
      (let ((this (car seq)))
        (loop
         (cdr seq)
         (if left? (cons this left) left)
         (if left? right (cons this right))
         (not left?))))))

(define (merge a b <)
  (let loop ((a a)
             (b b)
             (result '()))
    (cond
     ((null? a)
      (append (reverse result) b))
     ((null? b)
      (append (reverse result) a))
     (else
      (let ((a1 (car a))
            (b1 (car b)))
        (if (< a1 b1)
            (loop (cdr a)
                  b
                  (cons a1 result))
          (loop a
                (cdr b)
                (cons b1 result))))))))

(define (mergesort seq <)
  (if (or (null? seq)
          (null? (cdr seq)))
      seq
    (let-values (((left right) (split seq)))
      (merge (mergesort left <)
             (mergesort right <)
             <))))


(let ((l  '(2 1 4 3)))
  (printf "~a => ~a~%" l (mergesort l <)))
)