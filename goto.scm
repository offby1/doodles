(define label #f)
(define goto  #f)

(let ((labels '()))
  (set! label 
        (lambda (sym)
          (call-with-current-continuation
           (lambda (k)
             (set! labels (assq-set! labels sym k))))))
  (set! goto (lambda (sym)
               (let ((datum (assq sym labels)))
                 (if (not datum)
                     (error "No label named " sym))
                 ((cdr datum))))))

(let ((x 5))
  (label 'top)
  (if (zero? x)
      (begin (display "done") (newline))
    (begin 
      (set! x (- x 1))
      (display x)
      (newline)
      (goto 'top))))
