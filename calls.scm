(require 'sort)

(let ()
  (define (calls exp)
    (if (pair? exp)
        (cons 
         (if (symbol? (car exp))
             (car exp)
           (calls (car exp)))
         (apply append (map calls (cdr exp))))
      '()))

  (define (uniqify seq equal?)
    (cond
     ((not (pair? seq))
      seq)
     ((null? (cdr seq))
      seq)
     ((equal? (car seq)
              (cadr seq))
      (uniqify (cdr seq) equal?))
     (#t
      (cons (car seq)
            (uniqify (cdr seq)  equal?)))))

  (uniqify
   (sort
    (calls (call-with-input-file "calls.scm" read))
    (lambda (s1 s2)
      (string<? (symbol->string s1)
                (symbol->string s2))))
   eq?))