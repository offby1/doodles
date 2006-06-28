(module mergesort mzscheme

(require (lib "1.ss" "srfi"))

(define (split seq )
  (let* ((half  (round (/ (length seq) 2)))
         (left  (take seq half) )
         (right (drop seq half)))
    (values left right)))

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


)