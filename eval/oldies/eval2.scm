;; An environment for Scheme evaluation.

;; See SICP, 2nd ed.

;; An environment is a pointer to a frame.
;; (new-environment initial-frame) => environment

;; A frame is zero or more bindings, with a constraint: no variable
;; may be referred to by more than one binding.  Frames also have a
;; pointer to an enclosing environment.

;; A binding is a variable name and a value.

;; (lookup-name env) => either a pair (name . value), if it was found,
;; or #f.

(defmacro assert (expression)
         `(if (not ,expression)
              (error "Assertion failed:" ',expression)))
(define (all-true? seq)
  (let loop ((seq seq))
    (cond
     ((null? seq)
      #t)
     ((not (car seq))
      #f)
     (#t
      (loop (cdr seq))))))

(define (cell-new datum) (cons #t datum))
(define (is-cell? thing) (and (pair? thing) (eq? #t (car thing))))
(define (cell-ref c) (assert (is-cell? c)) (cdr c))
(define (cell-set! c datum) (assert (is-cell? c)) (set-cdr! c datum))

(define (stack-new) (cell-new '()))
(define (is-stack? thing) (and (is-cell? thing) (list? (cell-ref thing))))

(define (stack-push! stack datum) 
  (assert (is-stack? stack))
  (cell-set! stack (cons datum (cell-ref stack)))
  (assert (is-stack? stack)))

(define (stack-empty? stack) 
  (assert (is-stack? stack))
  (null? (cell-ref stack)))

(define (stack-pop! stack)
  (assert (is-stack? stack))

  (if (stack-empty? stack)
      (error "Cannot pop from empty stack"))

  (cell-set! stack (cdr (cell-ref stack)))
  (assert (is-stack? stack)))

(define (stack-top stack) 
  (assert (is-stack? stack))
  
  (if (stack-empty? stack)
      (error "Cannot examine top element of an empty stack."))

  (car (cell-ref stack)))

;; This returns a stack, not a list
(define (stack-all-but-first stack)
  (assert (is-stack? stack))
  (assert (not (stack-empty? stack)))
  (let ((return (cons #t (cdr (cell-ref stack)))))
    (assert (is-stack? return))
    return))

;; This returns a list, not a stack
(define (stack-map proc stack)
  (map proc (cell-ref stack)))

(define (binding-make name value)
  (if (not (symbol? name))
      (error "name must be a symbol, but is " name))
  (cons name value))

(define (is-binding? thing) (and (pair? thing) (symbol? (car thing))))

(define (binding-name b)
  (assert (is-binding? b))
  (car b))

(define (binding-value b)
  (assert (is-binding? b))
  (cdr b))

(define (binding-set-value! b datum)
  (assert (is-binding? b))
  (set-cdr! b datum)
  (assert (is-binding? b)))

(define (frame-make names values)
  (let ((return (stack-new)))
    (let loop ((bindings (map binding-make names values)))
      (if (null? bindings)
          return
        (begin
          (frame-add-binding! return (car bindings))
          (loop (cdr bindings)))))))

(define (is-frame? thing)
  (and (is-stack? thing)
       (all-true? (stack-map is-binding? thing))))

(define (frame-lookup frame name)
  (assert (is-frame? frame))

  (cond 
   ((stack-empty? frame)                         
    #f)
   ((eq? (binding-name (stack-top frame)) name) 
    (stack-top frame))
   (#t                                       
    (frame-lookup (stack-all-but-first frame) name))))

(define (frame-add-binding! frame binding)
  (assert (is-frame? frame))
  (assert (is-binding? binding))

  (let ((present (frame-lookup frame (binding-name binding))))
    (if present
        (error "name already present:" (binding-name binding)))
     
    (stack-push! frame binding))

  (assert (is-frame? frame)))

(define (environment-make) (list (frame-make '() '())))
(define (is-environment? thing)
  (and (list? thing)
       (all-true? (map is-frame? thing))))

(define (environment-lookup env name)
  (let loop ((frames env))
    (if (null? frames)
        #f
      (or (frame-lookup (car frames) name)
          (loop (cdr frames))))))

(define (environment-extend env frame)
  (assert (is-environment? env))
  (assert (is-frame? frame))
  (cons frame env))

(define (environment-current-frame env)
  (assert (is-environment? env))
  (car env))


;; To apply a procedure to arguments:
;; check that the number of arguments is correct for the procedure
;; for each argument, make a binding whose name is the corresponding
;; formal parameter, and whose value is the argument.
;; Create a new frame with those bindings.
;; Extend the procedure's environment with those bindings.
;; Evaluate the procedure body in the extended environment.

(define (my-apply proc arglist)
  (cond 
   ((not (is-procedure? proc))
    (error "Not a procedure: " proc))
   ((not (list? arglist))
    (error "Not a list: " arglist))
   ((not (= (length (proc-retrieve-formals proc))
            (length arglist)))
    (error "Wrong number of arguments" proc arglist))
   (#t
    (let ((extended-environment (environment-extend 
                                 (proc-retrieve-environment proc)
                                 (frame-make (proc-retrieve-formals proc)
                                             arglist))))
      (let loop ((body-forms (proc-retrieve-body proc))
                 (return (if #f #f)     ; unspecified value
                         ))
        (if (null? body-forms)
            return
          (loop (cdr body-forms)
                (my-eval
                 (car body-forms)
                 extended-environment))))))))

(define (is-formals-list? thing)
  (define (all-distinct? seq)
    (cond
     ((null? seq)
      #t)
     ((null? (cdr seq))
      #t)
     (#t
      (and (all-distinct? (cdr seq))
           (not (memq (car seq)
                      (cdr seq)))))))

  (and (list? thing)
       (all-true? (map symbol? thing))
       (all-distinct? thing)))

(define (make-procedure formals body environment)
  (assert (is-formals-list? formals))
  (assert (list? body))
  (assert (is-environment? environment))
  (let ((return (list formals body environment)))
    (assert (is-procedure? return))
    return))

(define (is-procedure? thing)
  (and (list? thing)
       (= 3 (length thing))
       (is-formals-list? (list-ref thing 0))
       (list?            (list-ref thing 1))
       (is-environment?  (list-ref thing 2))))

(define (proc-retrieve-formals proc)
  (assert (is-procedure? proc))
  (list-ref proc 0))

(define (proc-retrieve-body proc)
  (assert (is-procedure? proc))
  (list-ref proc 1))

(define (proc-retrieve-environment proc)
  (assert (is-procedure? proc))
  (list-ref proc 2))


;; So a procedure is:

;; An environment (namely, the one in which it was defined)

;; A list of formals (symbols, no two the same)

;; (To do: also allow a single symbol, and an improper list whose last
;; element is a symbol)

;; A body is a list of one or more expressions.

;; `lambda' gets passed the formals and the body; it somehow needs to
;; snarf the current environment and package that up, too.

(define (my-eval exp env)
  (assert (is-environment? env))
  (cond

   (;; Self-evaluating forms
    (or
     (null? exp)
     (boolean? exp)
     (char? exp)
     (vector? exp)
     (number? exp)
     (string? exp))
    ;;(display "self evaluating form `") (write exp) (display "'") (newline)
    exp)

   ((symbol? exp)
    (let ((binding (environment-lookup env exp)))
      (or (and binding
               (binding-value binding))
          (error "Unbound variable: " exp))))

   ((pair? exp)
    (let ((head (car exp))
          (args (cdr exp)))

      (define (check-arg-length special-form-name args proper-length)
        (cond
         ((< (length args)
             proper-length)
          (error "Too few arguments to `"
                    (symbol->string special-form-name)
                    "': "
                    args))
         ((> (length args)
             proper-length)
          (error "Too many arguments to `"
                    (symbol->string special-form-name)
                    "': "
                    args))))

         ;; Handle special forms first.

      (cond

       ((eq? head 'set!)
        (check-arg-length 'set! args 2)
        (if (symbol? (car args))
            (begin
              ;;(display "Assigning expression `") (write (cadr args)) (display "' to variable `") (write (car args)) (display "'") (newline)
              (let ((binding (environment-lookup env (car args))))
                (if (not binding)
                    (error "Can't set unbound variable: " (car args))
                  (binding-set-value! binding (my-eval (cadr args) env)))))
          (error "Argument to `set!' is not a symbol: " (car args))))

       ((eq? head 'define)
        (check-arg-length 'define args 2)
        (if (symbol? (car args))
            (begin
              ;;(display "Defining variable `") (write (car args)) (display "' to expression `") (write (cadr args)) (display "'") (newline)
              (let ((binding (environment-lookup env (car args)))
                    (new-value (my-eval (cadr args) env)))
                (if (not binding)
                    (frame-add-binding! (environment-current-frame env)
                                        (binding-make (car args) new-value))
                  (binding-set-value! binding new-value))))
          
          (error "Argument to `define' is not a symbol: " (car args))))

       ((eq? head 'begin)
        (let loop ((args args))
          (if (null? (cdr args))
              (my-eval (car args) env)
            (begin
              (my-eval (car args) env)
              (loop (cdr args))))))

       ((eq? head 'lambda)
        (make-procedure (car args)
                        (cdr args)
                        env))
       
       ((eq? head 'quote)
        (check-arg-length 'quote args 1)
        (car args))

       ((eq? head 'if)
        ;; check for correct number of arguments...
        (check-arg-length 'if args 3)
        (if (eq? (my-eval (car args) env)
                 #f)
            (my-eval (caddr args) env)
          (my-eval (cadr args) env)))

       ;; procedure call
       (else (my-apply
              (my-eval head env)
              (map (lambda (arg)
                     (my-eval arg env)) args))))))
   (else
    (error "unknown type: "
           exp))))

