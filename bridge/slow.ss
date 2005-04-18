;; a toy function that generates a large return value (a list with
;; many elements), and takes a long time to do it.

(require (lib "list.ss" "srfi" "1")
         (lib "trace.ss"))

;; (distribute 3 (list         )) => '()
;; (distribute 3 (list 'a      )) => ((3 a    ) (a 3    )                    )
;; (distribute 3 (list 'a 'b   )) => ((3 a b  ) (a 3 b  ) (a b 3  )          )
;; (distribute 3 (list 'a 'b 'c)) => ((3 a b c) (a 3 b c) (a b 3 c) (a b c 3))
(define (distribute item l)
  (cond
   ((null? l)
    '())
   ((null? (cdr l))
    (list (list item (car l))
          (list (car l) item)))
   (#t
    (cons (cons item l)
          (map (lambda (seq)
                 (cons (car l) seq)) (distribute item (cdr l)))))
   ))

(define (permute l)
  (cond
   ((null? l)
    '())
   ((null? (cdr l))
    (list l))
   (#t
    (append-map (lambda (seq)
                  (distribute (car l) seq))
                (permute (cdr l))))))
