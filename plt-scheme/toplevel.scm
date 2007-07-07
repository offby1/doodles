;; from Eli Barzilay -- a quick simulation of the scheme48 toplevel

(module toplevel mzscheme

(define toplevel-prompt "-")
(define prompt toplevel-prompt)

(define-struct cmd (help handler))
(define commands '())
(define-syntax defcommand
  (syntax-rules ()
    [(_ cmd help body ...)
     (set! commands (cons (cons `cmd (make-cmd `help (lambda () body ...)))
                          commands))]))
(define (run-cmd command)
  (cond [(assq command commands)
         => (lambda (command) ((cmd-handler (cdr command))))]
        [else (printf "Unknown command: ~s\n" command)]))

(defcommand help "display available commands"
  (printf "Available commands:\n")
  (for-each (lambda (command)
              (let ([cmd (cdr command)])
                (printf "  ~s: ~a\n" (car command) (cmd-help cmd))))
            (reverse commands)))

(defcommand cd "change the current directory"
  (let* ([arg (read-bytes-line)]
         [arg (and (bytes? arg)         ; might be EOF
                   (regexp-match #rx#"^[ \t]*(.+?)[ \t]*$" arg))]
         [arg (and arg (bytes->path (cadr arg)))])
    (if arg (current-directory arg) (printf "~a\n" (current-directory)))))

(define top-level-ns #f)
(defcommand open "require a module and go into its namespace"
  (let ([arg (read)])
    (dynamic-require arg #f)
    (unless top-level-ns (set! top-level-ns (current-namespace)))
    (current-namespace (module->namespace arg))
    (set! prompt (format "~s" arg))))
(defcommand close "go back to the toplevel"
  (when top-level-ns
    (current-namespace top-level-ns)
    (set! top-level-ns #f)
    (set! prompt toplevel-prompt)))

;; setup the handler
(define old-prompt-read (current-prompt-read))
(define (new-prompt-read)
  (let loop ()
    (display prompt) (flush-output)
    (let ([stx (old-prompt-read)])
      (syntax-case stx ()
        [(uq cmd) (eq? 'unquote (syntax-e #'uq))
         (begin (run-cmd (syntax-object->datum #'cmd)) (loop))]
        [_else stx]))))
(when (eq? old-prompt-read (current-prompt-read))
  (current-prompt-read new-prompt-read))
;; diable inlining => allow redefinitions
(compile-enforce-module-constants #f))

(require toplevel)
