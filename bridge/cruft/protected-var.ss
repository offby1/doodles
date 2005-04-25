(module protected-var mzscheme
  (provide (rename my-make-pv make-protected-var)
           (rename my-pv-set! protected-var-set!)
           (rename pv-get  protected-var-get))

  (define-values (struct:pv make-pv pv? pv-ref pv-set!)
    (make-struct-type
     'protected-variable                ;name-symbol
     #f                                 ;super-struct-type
     2                                  ;field: value, semaphore
     0                                  ;auto-field-k
     #f                                 ;auto-v
     null                               ;prop-value-list
     #f                                 ;inspector-or-false
     #f                                 ;proc-spec
     '(1)                               ;immutable-k-list
     #f                                 ;guard proc
     ))

  (define get-val
    (make-struct-field-accessor pv-ref 0))

  (define get-sem
    (make-struct-field-accessor pv-ref 1))
  
  (define set-val!
    (make-struct-field-mutator  pv-set! 0))

  (define (my-make-pv)
    (make-pv (void) (make-semaphore 1)))

  (define (pv-get var)
    (call-with-semaphore
     (get-sem var)
     (lambda () (get-val var))))
  
  (define (my-pv-set! var value)
    (call-with-semaphore
     (get-sem var)
     (lambda () (set-val! var value))))
  
  (define *g* (my-make-pv))
  (my-pv-set! *g* 44)
  (unless (= 44 (pv-get *g*))
    (error "Uh oh."))
  (printf "Whew~n")
  )