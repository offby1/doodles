;;(define-module (eric eval))

(use-modules (ice-9 slib))

;; To do: some of the asserts and errors probably need to be handled
;; differently; I don't distinguish errors in this code from errors in
;; the argument to `eval', and of course I should.  (The former should
;; cause an assertion, but the latter should be handled more
;; gracefully.)

(define debug? #t)

(defmacro assert (expression)
  (if debug?
      `(if (not ,expression)
           (error "Assertion failed:" ',expression))
    0))

;; Given a list of booleans, return #f if and only if at least one of
;; them is #f.  I originally thought I could do
;;
;;       (apply and seq)
;;
;; but of course it turns out that `and' is syntax, not a procedure,
;; and thus cannot be passed as an argument to `apply'.

(define (all-true? seq)
  (cond
   ((null? seq)
    #t)
   ((not (car seq))
    #f)
   (#t
    (all-true? (cdr seq)))))


(define is-value-list? list?)

(define (arglist-satisfies-formals? formals arglist)
  (and 
   (is-value-list? arglist)
   (cond
    ((list? formals)
     (= (length arglist)
        (length formals)))
    ((pair? formals)
     (>= (length arglist)
         (- (length (improper->list formals)) 1)))
    ((symbol? formals)
     #t)
    (else #f))))


(define (frame-make formals values)
  (assert (arglist-satisfies-formals? formals values))

  ;; Since we're about to bind some values, we need to check each
  ;; value -- if it's a pair, we need to change its status to 'bound.
  ;; The previous value will be either 'required-for-current-eval or
  ;; 'bound.  In either case, the next garbage collection will mark as
  ;; bound any values that are pointed to by this one.
  (for-each (lambda (value)
              (if (my-pair? value)
                  (mark-pair-in-use! value 'bound)))
            values)

  (cons 'frame 
        (cond
         ((list? formals)
          (map cons formals values))
         ((symbol? formals)
          (list (cons formals values)))
         (else

          (define (but-last improper)
            (assert (pair? improper))
            (cond
             ((not (pair? (cdr improper)))
              (list (car improper)))
             (else
              (cons (car improper)
                    (but-last (cdr improper))))))

          (define num-required-args (- (length (improper->list formals)) 1))

          ;; BUGBUG -- list-tail is not guaranteed to work on improper
          ;; lists -- although it seems to in Guile 1.3.1

          (define optional-arg (list-tail formals num-required-args))

          (append
           (map cons 
                (but-last formals)
                (but-last values))
           (list (cons optional-arg
                       (list-tail values num-required-args))))))))

(define (is-frame? thing)
  (and (list? thing)
       (not (null? thing))
       (eq? 'frame (car thing))
       (all-distinct-symbols? (map car (cdr thing)))))

;; Returns #f if no binding with that name is found.
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

(define (frame-map-bindings proc frame)
  (assert (is-frame? frame))
  (map proc (cdr frame)))



(define (mark-value-chain-unused value)
  (if (and
       (my-pair? value)
       (eq? (pair-status value) 'required-for-current-eval))
      (begin
        (mark-pair-in-use! value 'unused)
        (mark-value-chain-unused (my-car value))
        (mark-value-chain-unused (my-cdr value))

        (display (pairs-as-string #f))
        (newline))))

(define (frame-deep-map proc frame)

  (define frames-being-processed '())

  (define (being-processed? frame)
    (assert (is-frame? frame))
    (memq frame frames-being-processed))

  (define (being-processed frame yes?)
    (if yes?
        (begin
          (assert (not (being-processed? frame)))
          (set! frames-being-processed (cons frame frames-being-processed)))
      (begin
        (assert (eq? frame (car frames-being-processed)))
        (set! frames-being-processed (cdr frames-being-processed)))))
  
  (define (value-deep-map proc value)
    (append
     (list (proc value))
     (cond
      ((is-procedure? value)
       (procedure-deep-map proc value))
      ((my-pair? value)
       (cons
        (value-deep-map proc (my-car value))
        (value-deep-map proc (my-cdr value))))
      (else '()))))
  
  (define (procedure-deep-map actor procedure)
    (assert (is-procedure? procedure))
    (append
     (list (actor procedure))
     (environment-map-frames (lambda (frame)
                               (internal-frame-deep-map actor frame))
                             (proc-retrieve-environment procedure))))

  (define (internal-frame-deep-map proc frame)
    (assert (is-frame? frame))

    (if (being-processed? frame)
        '()
      (let ()

        (define (map-values proc frame)
          (map proc (frame-map-bindings binding-value frame)))

        (being-processed frame #t)

        (append (list
                 (proc frame))
                (let ((return (map-values (lambda (value)
                                            (value-deep-map proc value)) 
                                          frame)))
                  (being-processed frame #f)
                  return)))))

  (internal-frame-deep-map proc frame))


(define-public (summarize-frames)
  (define summary '())

  (define (internal-summarize-frame thing)
    (define identity (lambda (x) x))
    (if (is-frame? thing)
        (set! summary (cons (frame-name-map identity
                                            thing) summary))))

  (frame-deep-map internal-summarize-frame (environment-current-frame *top-level-environment*))
  summary)


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

  (set-cdr! b datum)
  (if #f #f))


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

(define (environment-map-frames proc env)
  (assert (is-environment? env))
  (map proc env))


;; To apply a procedure to arguments:
;;
;; make bindings whose names are the formal parameters, and whose
;; values are the arguments.
;;
;; Create a new frame with those bindings.
;;
;; Extend the procedure's environment with that new frame.
;;
;; Evaluate the procedure body in the extended environment.

(define (eval-exprs-returning-last forms env)
  (let loop ((forms forms)
             (return (if #f #f)))
    (if (null? forms)
        return
      (begin

        ;; Since we're ignoring this value, make sure it gets
        ;; garbage-collected.
        (mark-value-chain-unused return)

        (loop (cdr forms)
              (internal-eval (car forms) env))))))

(define (my-apply proc arglist)
  (assert (and
           (is-procedure? proc)
           (list? arglist)))
  
  (eval-exprs-returning-last
   (proc-retrieve-body proc)
   (environment-extend 
    (proc-retrieve-environment proc)
    (frame-make (proc-retrieve-formals proc)
                arglist))))

(define (improper->list thing)
  (cond 
   ((not (pair? thing))
    '())
   ((pair? (cdr thing))
    (cons (car thing)
          (improper->list (cdr thing))))
   (else
    (list (car thing)
          (cdr thing)))))

(define (all-distinct-symbols? seq)
  (cond
   ((null? seq)
    #t)
   ((null? (cdr seq))
    (symbol? (car seq)))
   (#t
    (and (all-distinct-symbols? (cdr seq))
         (not (memq (car seq)
                    (cdr seq)))))))

;; Returns #t if and only if THING is a legal formals list for a
;; lambda form.  That is:
;;
;; Returns 'symbol if THING is a symbol, 'list if it's a proper list
;; of distinct symbols, and 'pair if it's an improper list of distinct
;; symbols.  Otherwise returns #f.
;;
(define (is-formals-list? thing)
  (cond
   ((symbol? thing)                     ;(lambda x blah ...)
    'symbol)
   ((list? thing)                       ;(lambda (x y z) blah ...)
    (and
     (all-distinct-symbols? thing)
     'list))
   ((pair? thing)                       ;(lambda (x y . z) blah ... )
    (and (all-distinct-symbols? (improper->list thing))
         'pair))
   (else #f)))

(define (make-procedure formals body environment)
  (assert (is-formals-list? formals))
  (assert (and (list? body)
               (not (null? body))))
  (assert (is-environment? environment))
  (let ((return (make-vector 3)))
    (vector-set! return 0 formals)
    (vector-set! return 1 body)
    (vector-set! return 2 environment)
    (assert (is-procedure? return))
    return))

(define (proc-retrieve-formals proc)
  (assert (is-procedure? proc))
  (vector-ref proc 0))

(define (proc-retrieve-body proc)
  (assert (is-procedure? proc))
  (vector-ref proc 1))

(define (proc-retrieve-environment proc)
  (assert (is-procedure? proc))
  (vector-ref proc 2))

(define (is-procedure? thing)
  (and (vector? thing)
       (= 3 (vector-length thing))
       (is-formals-list? (vector-ref thing 0))
       (list?            (vector-ref thing 1))
       (not (null?       (vector-ref thing 1)))
       (is-environment?  (vector-ref thing 2))))



(define *top-level-environment* (environment-make))

(define-public (my-eval exp)
  (let ((return (internal-eval exp *top-level-environment*)))
    (mark-value-chain-unused return)

    ;; If we were to garbage-collect now, we could be guaranteed that
    ;; there would be no pairs whose status is
    ;; `required-for-current-eval'.  And now is the only time that
    ;; that statement is true.
    
    return))

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

        (let ()
          ;; First see if we're defining a procedure.

          (define is-procedure?
            (or
             (list? (car args))
             (pair? (car args))))
          
          (define binding
            (let ()
              ;; Now figure out the name of the variable we're defining.

              (define name
                (cond
                 ((symbol? (car args))
                  (car args))
                 (is-procedure?
                  (caar args))
                 (else
                  (error "First arg to `define' isn't a symbol, list, or improper list"
                         (car args)))))
              (frame-lookup (environment-current-frame env) name)))

          ;; evaluate the body forms

          ;; Note that if the name was found inside a list, or
          ;; improper list, then we're defining a procedure.

          (define new-value 
            (assert (or is-procedure? (= 1 (length (cdr args)))))
            (internal-eval
             (if is-procedure?
                 (make-procedure (cdar args) body-forms env)
               (car body-forms))
             env))

          ;; stick the name-value pair into the current frame,
          ;; regardless of whether that frame already holds a binding
          ;; for that name.

          (if (not binding)
              (frame-add-binding! (environment-current-frame env)
                                  (binding-make name new-value))
            (binding-set-value! binding new-value))))


       ((eq? head 'begin)
        ;;(display "`begin' ") (write args) (newline)
        (eval-exprs-returning-last args env))

       ((eq? head 'lambda)
        ;;(display "`lambda' ") (write args) (newline)
        (make-procedure (car args)
                        (cdr args)
                        env))

       ((eq? head 'let)
        (if (< (length args) 2)
            (error "`let' has too few arguments"))
         
        (let* ((bindings (car args))
               (names    (map car bindings))
               (values   (map cadr bindings))
               (body-forms (cdr args)))
          (internal-eval (append (list (append (list 'lambda names) body-forms))
                                 values)
                         env
                         )))

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
                (internal-eval arg env)) args))))))
   (else
    (error "unknown type: "
           exp))))


(require 'object->string)

;; The first bit of the pair is either 
;;
;; the symbol 'unused, which means this pair is available to be
;; returned by `cons';
;;
;; the symbol 'bound, which means the pair is the value of some
;; variable, or
;;
;; a positive integer, which means that the pair is part of the
;; current evaluation.  The integer represents the evaluation depth.
;; One of the last things `eval' does is clean out any pairs it
;; allocated and no longer needs.

(define (new-unused-pair car cdr) 
  (let ((return (make-vector 3)))
    (vector-set! return 0 'unused)
    (vector-set! return 1 car)
    (vector-set! return 2 cdr) return))

(define (my-pair? thing)
  (and (vector? thing)
       (valid-status? thing)))

(define (pair-status pair)
  (assert (my-pair? pair))
  (vector-ref pair 0))

(define (in-use? pair)
  (assert (my-pair? pair))
  (not (eq? 'unused (pair-status pair))))

(define (valid-status? pair)
   (memq (vector-ref pair 0) '(unused bound required-for-current-eval)))

(define (mark-pair-in-use! pair status)
  (assert (my-pair? pair))
  (vector-set! pair 0 status)
  (assert (valid-status? pair)))

(define (my-car pair)
  (assert (my-pair? pair))
  (vector-ref pair 1))

(define (my-cdr pair)
  (assert (my-pair? pair))
  (vector-ref pair 2))

(define (my-set-car! pair datum)
  (assert (my-pair? pair))
  (vector-set! pair 1 datum))

(define (my-set-cdr! pair datum)
  (assert (my-pair? pair))
  (vector-set! pair 2 datum))

(define (my-cons car cdr)

  (if debug?
      (begin
        (display "Before garbage collection:\n")
        (display (pairs-as-string #f))
        (newline)))
  
  (maybe-collect-garbage)

  (die-if-no-pairs)

  ;; find an unused pair
  (let ((pair (available-pair)))
    
    ;; mark it as in use
    (mark-pair-in-use! pair 'required-for-current-eval)

       ;; set its car and cdr fields
    (my-set-car! pair car)
    (my-set-cdr! pair cdr)

       ;; return it

    pair))

(define my-nil '())

(define (my-null? x)
  (eq? x my-nil))


(define pairs (make-vector 10))

(let loop ((pairs-initialized 0))
  (define unspecified (if #f #f))
  (if (< pairs-initialized (vector-length pairs))
      (begin
        (vector-set! pairs 
                     pairs-initialized 
                     (new-unused-pair unspecified unspecified))
        (loop (+ 1 pairs-initialized)))))

(define-public (pairs-as-string show-unused? . statuses-that-are-known-authoritative)
  (define (pair->string p)
    (define (value->string v)
      (define (get-index p)
        (let loop ((pairs-examined 0))
          (if (= pairs-examined (vector-length pairs))
              (error "Cannot find pair " (object->string p)))
          (if (eq? p (vector-ref pairs pairs-examined))
              pairs-examined
            (loop (+ 1 pairs-examined)))))
      
      (if (my-pair? v)
          (string-append "#pair "
                         (number->string (get-index v)))
        (object->string v)))

    (let ((status (pair-status p)))
      (string-append (if (memq status statuses-that-are-known-authoritative)
                         "!"
                       " ")
                     (symbol->string status)
                     "("   (value->string (my-car p))
                     " . " (value->string (my-cdr p))
                     ")")))
  
  (string-append
   "(`!' means status is known authoritatively)\n"
   (let loop ((pairs-examined 0)
              (return ""))
     (if (= pairs-examined (vector-length pairs))
         return
       (loop (+ 1 pairs-examined)
             (let* ((p (vector-ref pairs pairs-examined)))
               (string-append 
                return 
                (if (or show-unused?
                        (in-use? p))
                    (string-append
                     (if (positive? (string-length return))
                         "\n"
                       "")
                     (number->string pairs-examined)
                     ": "
                     (pair->string p))
                  ""))))))))

(define (available-pair)
  (let loop ((pairs-examined 0))
    
    ;; This function is only called from my-cons, which has already
    ;; ensured that there is at least one available pair.
    (assert (< pairs-examined (vector-length pairs)))
    (let ((this-pair (vector-ref pairs pairs-examined)))
      (if (not (in-use? this-pair))
          this-pair
        (loop (+ 1 pairs-examined))))))

(define (pairs-used)
  (apply + (map (lambda (pair)
                  (if (in-use? pair)
                      1
                    0))
                (vector->list pairs))))

;; Kind of simplistic, I realize ... but possibly useful for
;; debugging.
(define (maybe-collect-garbage) 
  (collect-garbage)
  ;;0
  )

(define-public (collect-garbage)

  ;; First mark all bound values as unused, because *some* of those
  ;; values may indeed be unused ...
  (for-each (lambda (pair)
              (if (eq? 'bound (pair-status pair))
                  (mark-pair-in-use! pair 'unused)))
            (vector->list pairs))

  ;; Now find which values are *really* bound.
  (frame-deep-map (lambda (thing)
                    (if (my-pair? thing)
                        (begin
                          (display "collect-garbage: about to mark this pair as bound: `")
                          (display (pair-status thing))
                          (display " ")
                          (display (my-car thing))
                          (display " . ")
                          (display (my-cdr thing))
                          (newline)
                          (mark-pair-in-use! thing 'bound))))
                  (environment-current-frame *top-level-environment*))

  ;; Now is the only time that a pair status of `bound' is guaranteed
  ;; to be correct.
  (newline)
  (display (pairs-as-string #t 'bound))
  (newline) (newline))


(define (die-if-no-pairs)
  (if (= (pairs-used)
         (vector-length pairs))
      (error "No more pairs.")))

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
           asin
           atan
           
           ;; Doesn't make sense to snarf this -- it wants a Guile
           ;; procedure, but such things don't exist in my evaluator's
           ;; world.  (My procedures are simply a list of a formals
           ;; list, a body, and an environment)
           ;;call-with-current-continuation

           ;;car ; I got my own definition of this
           ;;cdr ; ditto
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
           ;;cons ; rolled my own
           cos
           current-input-port
           current-output-port
           display
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
           newline
           number->string
           number?
           open-input-file
           open-output-file
           output-port?
           ;;pair? ; rolled my own
           peek-char
           ;;procedure? ; rolled my own
           quotient
           rational?
           read-char
           real-part
           real?
           remainder
           round
           ;;set-car! ; rolled my own
           ;;set-cdr! ; ditto
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

;; Now add some of my own handmade functions, but under the normal names.
(set! snarfed-procedures (append (list 
                                  
                                  (cons 'cons     my-cons)
                                  (cons 'pair?    my-pair?)
                                  (cons 'car      my-car)
                                  (cons 'cdr      my-cdr)
                                  (cons 'set-car! my-set-car!)
                                  (cons 'set-cdr! my-set-cdr!)
                                  (cons 'null?    my-null?)
                                  (cons 'apply my-apply)
                                  (cons 'eval  internal-eval)
                                  (cons 'procedure? is-procedure?))
                                 snarfed-procedures))