(require 'record)

(define thread? #f)
(define is-finished? #f)
(define value #f)
(define wait-for #f)
(define eval-async #f)

(let ()

  (define rtd (make-record-type "thread" '(value)))

  (set! thread?  (record-predicate rtd))
  
  (set! is-finished?
        (lambda (T)
          (if (not (thread? T))
              (error "That ain't no thread:" T))
          #t))

  (set! value  (record-accessor rtd 'value))

  (set! eval-async
        (lambda (exp env)
          ((record-constructor rtd) (eval exp env))))
     
  (set! wait-for value))
