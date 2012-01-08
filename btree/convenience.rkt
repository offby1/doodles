(define (list->tree l)
  (for/fold ([t (public-make-tree)])
      ([p  l])
      (tree-set t (car p) (cdr p))))

(define (ql->t keys)
  (list->tree (map (lambda (k) (cons k k)) keys)))
