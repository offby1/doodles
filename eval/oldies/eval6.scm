(use-modules (ice-9 slib))
(require 'object->string)

;; To do: some of the asserts and errors probably need to be handled
;; differently; I don't distinguish errors in this code from errors in
;; the argument to `eval', and of course I should.  (The former should
;; cause an assertion, but the latter should be handled more
;; gracefully.)

(defmacro assert (expression)
         `(if (not ,expression)
              (error "Assertion failed:" ',expression)))

;; Given a list of booleans, return #f if and only if at least one of
;; them is #f.  I originally thought I could do
;;
;;       (apply and seq)
;;
;; but of course it turns out that `and' is syntax, not a procedure,
;; and thus cannot be passed as an argument to `apply'.

(define (all-true? seq)
  (let loop ((seq seq))
    (cond
     ((null? seq)
      #t)
     ((not (car seq))
      #f)
     (#t
      (loop (cdr seq))))))

(define (my-object->string thing indentation follow-procedure-environments)
  (cond
   ((is-procedure? thing)
    (procedure->string thing indentation follow-procedure-environments))
   (#t
    (string-append (make-string indentation #\space) (object->string thing)))))

(define (is-value-list? thing) (list? thing))

(define (are-good-frame-arguments? names values)
  (and (is-formals-list? names)
       (is-value-list? values)
       (= (length names)
          (length values))))

(define (frame-make names values)
  (assert (are-good-frame-arguments? names values))

  (cons 'frame (map cons names values)))

(define (is-frame? thing)
  (and (list? thing)
       (eq? 'frame (car thing))
       (are-good-frame-arguments? (map car (cdr thing))
                                  (map cdr (cdr thing)))))

(define (frame-lookup frame name)
  (assert (is-frame? frame))
  (assert (symbol? name))

  (assq name (cdr frame)))

(define (frame-add-binding! frame binding)
  (assert (is-frame? frame))
  (assert (is-binding? binding))
  (assert (not (frame-lookup frame (binding-name binding))))
  
  (set-cdr! frame (cons binding (cdr frame)))

  (assert (let ((what-we-just-added (frame-lookup frame (binding-name binding))))
            (and
             (eq? (binding-name binding)
                  (binding-name what-we-just-added))
             (eq? (binding-value binding)
                  (binding-value what-we-just-added))))))

(define (frame-name-map proc frame)
  (assert (is-frame? frame))
  (map proc (map car (cdr frame))))

(define (frame-for-each-binding proc frame)
  (assert (is-frame? frame))
  (for-each proc (cdr frame)))

(define (frame->string frame indentation follow-procedure-environments)
  (apply string-append
         (map (lambda (name-value)
                (string-append 
                 (make-string indentation #\space) "Frame -------------------\n"
                 (make-string indentation #\space) "Name:   " (object->string (car name-value)) "\n"
                 (make-string indentation #\space) "Value: `" (my-object->string (cdr name-value) indentation follow-procedure-environments) "'\n"
                 ))
              (cdr frame))))

(define (binding-make name value)
  (assert (symbol? name))

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

  (set-cdr! b datum))

;; Frames and values have an interesting, symmetrical, relationship: a
;; frame can contain values, of course (that's it's whole purpose in
;; life), but a value can also contain frames: if the value `contains'
;; a procedure, that procedure has a captured environment, which is a
;; list of frames.

;; A value `contains' a procuedure if and only if:
;;
;; either the value *is* a procedure, or
;;
;; the value is a pair, either the car or cdr of which `contains' a
;; procedure.

(define (frame-get-values frame)
  (assert (is-frame? frame))
  (map binding-value frame))

(define (value-get-frames value)
  ;; This assertion essentially duplicates the structure of the big
  ;; `cond' in `my-eval', and thus that redundancy should somehow be
  ;; factored out.  Until then, ensure that this assertion stays in
  ;; sync with the `cond'.
  (assert (or (is-procedure? value)
              (pair? value)
              (my-null? value)
              (or
               (boolean? value)
               (char? value)
               (vector? value)
               (number? value)
               (string? value))))
  (cond
   ((is-procedure? value)
    (proc-retrieve-environment value)
    '())
   ((pair? value)
    (append (value-get-frames (my-car value)
                              (my-cdr value))))))



(define (environment-make) (list (frame-make (list) (list))))

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

  ;; For each binding in this frame, check the environment to see if
  ;; there is a binding with the same name.
  (frame-name-map 
   (lambda (name)
     (if (environment-lookup env name)
         (begin
           (display "Warning: shadowing variable `")
           (display name)
           (display "'")
           (newline))))
   frame)

  (cons frame env))

(define (environment-current-frame env)
  (assert (is-environment? env))
  (car env))

(define (environment-for-each-frame proc env)
  (assert (is-environment? env))
  (for-each proc env))

(define (env->string env indentation follow-procedure-environments)
  (let loop ((frames env)
             (return ""))
    (if (null? frames)
        return
      (loop (cdr frames)
            (string-append return (frame->string (car frames) indentation follow-procedure-environments))))))


;; To apply a procedure to arguments:
;; check that the number of arguments is correct for the procedure
;; for each argument, make a binding whose name is the corresponding
;; formal parameter, and whose value is the argument.
;; Create a new frame with those bindings.
;; Extend the procedure's environment with those bindings.
;; Evaluate the procedure body in the extended environment.

;; (To do: also allow a single symbol, and an improper list whose last
;; element is a symbol)

(define (eval-args-returning-last args env)
  (let loop ((args args)
             (return (if #f #f)))
    (if (null? args)
        return
      (loop (cdr args)
            (internal-eval (car args) env)))))

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
    ;;(display "apply ") (write (proc-retrieve-body proc)) (display " to ") (write (proc-retrieve-formals proc)) (newline)
    (eval-args-returning-last
     (proc-retrieve-body proc)
     (environment-extend 
      (proc-retrieve-environment proc)
      (frame-make (proc-retrieve-formals proc)
                  arglist))))))

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
  (assert (not (null? body)))
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

(define (procedure->string proc indentation follow-procedure-environments)
  (assert (is-procedure? proc))
  (string-append "Formals: `"
                 (object->string (proc-retrieve-formals proc))
                 "'; body: `"
                 (object->string (proc-retrieve-body proc))
                 "'"
                 (if follow-procedure-environments
                     (string-append "; environment:\n" (env->string (proc-retrieve-environment proc) (+ 1 indentation) #f))
                   "")))



(define *top-level-environment* (environment-make))

(define (my-eval exp)
  (internal-eval exp *top-level-environment*))

(define (internal-eval exp env)
  (assert (is-environment? env))
  (cond

   (;; Self-evaluating forms
    (or
     (boolean? exp)
     (char? exp)
     (vector? exp)
     (number? exp)
     (string? exp))
    ;;(display "self-evaluating form `") (write exp) (display "'") (newline)
    exp)

   ((null? exp) my-nil)

   ((symbol? exp)
    ;;(display "variable reference: ") (write exp) (newline)
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
                  (binding-set-value! binding (internal-eval (cadr args) env)))))
          (error "Argument to `set!' is not a symbol: " (car args))))

       ((eq? head 'define)
        (check-arg-length 'define args 2)
        (if (symbol? (car args))
            (begin
              ;;(display "Defining variable `") (write (car args)) (display "' to expression `") (write (cadr args)) (display "'") (newline)
              (let ((binding (environment-lookup env (car args)))
                    (new-value (internal-eval (cadr args) env)))
                (if (not binding)
                    (frame-add-binding! (environment-current-frame env)
                                        (binding-make (car args) new-value))
                  (binding-set-value! binding new-value))))
          
          (error "Argument to `define' is not a symbol: " (car args))))

       ((eq? head 'begin)
        ;;(display "`begin' ") (write args) (newline)
        (let loop ((args args))
          (if (null? (cdr args))
              (internal-eval (car args) env)
            (begin
              (internal-eval (car args) env)
              (loop (cdr args))))))

       ((eq? head 'lambda)
        ;;(display "`lambda' ") (write args) (newline)
        (make-procedure (car args)
                        (cdr args)
                        env))
       
       ((eq? head 'quote)
        ;;(display "`quote' ") (write args) (newline)
        (check-arg-length 'quote args 1)
        (car args))

       ((eq? head 'if)
        ;; check for correct number of arguments...
        (check-arg-length 'if args 3)
        ;;(display "`if' ") (write args) (newline)
        (if (eq? (internal-eval (car args) env)
                 #f)
            (internal-eval (caddr args) env)
          (internal-eval (cadr args) env)))

       ((eq? head 'and)
        ;;(display "`and' ") (write args) (newline)
        (cond
         ((eq? (length args)
               0)
          #t)
         ((eq? (length args)
               1)
          (internal-eval (car args) env))
         (#t
          (if (internal-eval (car args) env)
              (internal-eval  (cons 'and (cdr args)) env)
            #f))))
       
       ((assq head snarfed-procedures)
        => (lambda (alist-entry)
             ;;(display "snarfed proc ") (write head) (display ": ") (write args) (newline)
             (apply (cdr alist-entry)
                    (map (lambda (arg)
                           (internal-eval arg env)) args))))

       ;; procedure call
       (else 
        ;;(display "call ") (write head) (display " with ") (write args) (newline)
        (my-apply
         (internal-eval head env)
         (map (lambda (arg)
                (internal-eval arg  env)) args))))))
   (else
    (error "unknown type: "
           exp))))

(defmacro snarf-em (procnames)
  (list 'define 'snarfed-procedures
        (cons 'list
              (map (lambda (sym)
                     (list 'cons (list 'quote sym) sym)) procnames))))

(snarf-em (*
           +
           -
           /
           <
           <=
           =
           >
           >=
           acos
           angle
           ;;apply
           asin
           atan
           
           ;; Doesn't make sense to snarf this -- it wants a Guile
           ;; procedure, but such things don't exist in my evaluator's
           ;; world.  (My procedures are simply a list of a formals
           ;; list, a body, and an environment)
           ;;call-with-current-continuation

           ;;car
           ;;cdr
           ceiling
           char->integer
           char-ready?
           char<=?
           char<?
           char=?
           char>=?
           char>?
           char?
           close-input-port
           close-output-port
           complex?
           ;;cons
           cos
           current-input-port
           current-output-port
           dynamic-wind
           eof-object?
           eq?
           eqv?
           ;;eval
           exact->inexact
           exact?
           exp
           expt
           floor
           imag-part
           inexact->exact
           inexact?
           input-port?
           integer->char
           integer?
           log
           magnitude
           make-polar
           make-rectangular
           make-string
           make-vector
           modulo
           number->string
           number?
           open-input-file
           open-output-file
           output-port?
           ;;pair?
           peek-char
           procedure?
           quotient
           rational?
           read-char
           real-part
           real?
           remainder
           round
           ;;set-car!
           ;;set-cdr!
           sin
           sqrt
           string->number
           string->symbol
           string-length
           string-ref
           string-set!
           string?
           symbol->string
           symbol?
           tan
           truncate
           vector-length
           vector-ref
           vector-set!
           vector?
           write-char
           ))

(load "memory-allocator.scm")

;; Now add some of my own handmade functions, but under the normal names.
(set! snarfed-procedures (append (list (cons 'cons     my-cons)
                                       (cons 'pair?    my-pair?)
                                       (cons 'car      my-car)
                                       (cons 'cdr      my-cdr)
                                       (cons 'set-car! my-set-car!)
                                       (cons 'set-cdr! my-set-cdr!)
                                       (cons 'null?    my-null?)
                                       (cons 'apply my-apply)
                                       (cons 'eval  internal-eval))
                                 snarfed-procedures))

;; This should print out 
;; 93326215443944152681699238856266700490715968264381621468592963895217599993229915608941463976156518286253697920827223758251185210916864000000000000000000000000
(lambda ()

  (my-eval '((lambda ()
               (define fact
                 (lambda (n)
                   (if (and (integer? n)
                            (> n 1))
                       (* n (fact (- n 1)))
                     1)))
                    
               (fact 100)))))

;; This should print out 0, 1, 2.
(lambda ()

  (my-eval 
   '(define golly 
      ((lambda (counter)
         (lambda ()
           (set! counter (+ 1 counter))
           (- counter 1))) 
       0)))

  (my-eval
   '(golly))

  (my-eval
   '(golly))

  (my-eval
   '(golly)))
