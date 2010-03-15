(use srfi-1)

;;; (intersperse 'a '(1 2 3)) => ((1 2 3 a) (1 2 a 3) (1 a 2 3) (a 1 2 3))
(define (intersperse item l)
  (let loop ((left '())
             (right l)
             (result '()))
    (cond
     ((null? right)
      (cons (append l (list item)) result))
     (else
      (loop (append left (list (car right)))
            (cdr right)
            (cons (append left (list item) right) result))))))

(define (permute l)
  (cond
   ((null? l)
    '())
   ((null? (cdr l))
    (list l))
   (else
    (append-map (lambda (seq)
                  (intersperse (car l) seq))
                (permute (cdr l))))))

(display "Hello, world")
(newline)

(let ((n 9))
  (printf "There are at least ~a permutations of the first ~a nonnegative integers~%"
          (length (permute (iota n)))
          n))