(let ()

  ;; The read-eval-print loop will redefine this procedure so that,
  ;; when you call it, you get put back into the read-eval-print
  ;; loop.  This will be how we handle errors.
  (define (back-to-repl)
    (error "back-to-repl hasn't been defined yet."))

  (define (my-error . args)
    (display "Eric sez ")
    (for-each display args)

    ;; back-to-repl takes one argument, which the read-eval-print loop
    ;; expects to be something other than a list.  If it's a list, the
    ;; read-eval-print loop will think that my-eval returned without
    ;; encountering an error.

    (back-to-repl 'error)
                         )

  (define symbol-table
    (let ((my-environment `(

                            ;; Snarf the native scheme's definitions for many of the common
                            ;; procedures.

                            ;; It'd be nice if I could write a macro to
                            ;; help me do this, but I haven't yet figured
                            ;; out how.

                            (*                . ,*)
                            (+                . ,+)
                            (=                . ,=)
                            (append           . ,append)
                            (boolean?         . ,boolean?)
                            (display          . ,display)
                            (car              . ,car)
                            (cdr              . ,cdr)
                            (cons             . ,cons)
                            (eq?              . ,eq?)
                            (length           . ,length)
                            (list             . ,list)
                            (list?            . ,list?)
                            (not              . ,not)
                            (null?            . ,null?)
                            (number?          . ,number?)
                            (pair?            . ,pair?)
                            (procedure?       . ,procedure?)
                            (symbol?          . ,symbol?)
                            )))

      ;; This is the code for `symbol-table'.  It is essentially a
      ;; dispatcher, because we want to be able to perform lots of
      ;; operations on our symbol table, but `symbol-table' itself is
      ;; only one function.

      (lambda (operation arg1 . arg2)

        ;; These are the internal functions that implement the
        ;; operations on the symbol table.

        (define (lookup symbol)
          ;;(display "Looking up symbol `") (write symbol) (write "'") (newline)
          (let ((value (assq symbol my-environment)))
            (if (not value)
                (my-error "Undefined symbol: " symbol)
              (cdr value))))

        (define (modify! symbol new-value)
          ;;(display "Modifying symbol `") (write symbol) (display "' with value `") (write new-value) (display "'") (newline)
          (let ((old-value (assq symbol my-environment)))
            (if (not old-value)
                (my-error "Undefined symbol: " symbol)
              (set-cdr! old-value new-value ))))

        (define (install! symbol initial-value replace-existing)
          ;;(display "Installing new symbol `") (write symbol) (display "' with initial value `") (write initial-value) (display "'") (newline)
          (let ((old-value (assq symbol my-environment)))
            (if old-value
                (if (not replace-existing)
                    (my-error "Thou shalt not redefine symbol " symbol)
                  (modify! symbol initial-value))
              (set! my-environment (cons (cons symbol initial-value) my-environment)))))

        (define (dump)
          (display my-environment))

        (if (> (length arg2)
               1)
            (error "internal error: too many arguments to symbol-table dispatch" arg2)
          (cond

           ((eq? operation 'lookup)
            (if (not (null? arg2))
                (error "internal error: extra argument to symbol-table operation `lookup' :" arg2))
            (lookup arg1))

           ((eq? operation 'modify)
            (if (null? arg2)
                (error "internal error: missing second argument to symbol-table operation `modify'")
              (modify! arg1 (car arg2))))

           ((eq? operation 'install)
            (if (null? arg2)
                (error "internal error: missing second argument to symbol-table operation `install'")
              (install! arg1 (car arg2) #f)))

           ((eq? operation 'safe-install)
            (if (null? arg2)
                (error "internal error: missing second argument to symbol-table operation `safe-install'")
              (install! arg1 (car arg2) #t)))

           ((eq? operation 'dump)
            (dump))

           (else
            (error "internal error: unknown symbol-table operation: " operation)))))))

  (define (my-eval exp)
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
       (symbol-table 'lookup exp))

     ((pair? exp)
      (let ((head (car exp))
            (args (cdr exp)))

        (define (check-arg-length special-form-name args proper-length)
          (cond
           ((< (length args)
               proper-length)
            (my-error "Too few arguments to `"
                      (symbol->string special-form-name)
                      "': "
                      args))
           ((> (length args)
               proper-length)
            (my-error "Too many arguments to `"
                      (symbol->string special-form-name)
                      "': "
                      args))))

        ;; Handle special forms first.

        (cond

         ;; dump shouldn't be a special form; it should be a
         ;; predefined procedure.  But I don't know how to do those
         ;; yet.
         ((eq? head 'dump)
          (symbol-table 'dump 'dummy))
         ((eq? head 'set!)
          (check-arg-length 'set! args 2)
          (if (symbol? (car args))
              (begin
                ;;(display "Assigning expression `") (write (cadr args)) (display "' to variable `") (write (car args)) (display "'") (newline)
                (symbol-table 'modify (car args) (my-eval (cadr args))))
            (my-error "Argument to `set!' is not a symbol: " (car args))))

         ((eq? head 'define)
          (check-arg-length 'define args 2)
          (if (symbol? (car args))
              (begin
                ;;(display "Defining variable `") (write (car args)) (display "' to expression `") (write (cadr args)) (display "'") (newline)
                (symbol-table 'install (car args) (my-eval (cadr args))))
            (my-error "Argument to `define' is not a symbol: " (car args))))

         ((eq? head 'begin)
          (let loop ((args args))
            (if (null? (cdr args))
                (my-eval (car args))
              (begin
                (my-eval (car args))
                (loop (cdr args))))))

         ;; I don't know how to do `lambda' yet.  This version is
         ;; obviously broken, in that it completely ignores the
         ;; formals.

         ((eq? head 'lambda)
          (check-arg-length 'lambda args 2)
          (display "Lambda: args is `")
          (write args)
          (display "'")
          (newline)
          (lambda dummy (my-eval (cons 'begin
                                       (cdr args))))
          )

         ((eq? head 'quote)
          (check-arg-length 'quote args 1)
          (car args))

         ((eq? head 'if)
          ;; check for correct number of arguments...
          (check-arg-length 'if args 3)
          (if (eq? (my-eval (car args))
                     #f)
                (my-eval (caddr args))
              (my-eval (cadr args))))
         (else (apply
                (my-eval head)
                (map my-eval args))))))
     (else
      (error "Error in SCM: `"
             exp
             "' is not any known type"))))

  (display "Enter the end-of-file character to quit.")
  (newline)
  (let loop ()

    (display "Eric> ")

    (let ((what-we-read (read)))

      (if (eof-object? what-we-read)
          (begin
            (display "OK, later...")
            (newline))

        (let ((result
               (call-with-current-continuation
                (lambda (x)
                  (set! back-to-repl x)
                  (list (my-eval what-we-read))))))

          (if (not (eq? result 'error))
              (begin
                (display "=> ")
                (write (car result))))

          (newline)
          (loop)))))
  )