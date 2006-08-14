;; For Gambit Scheme 2.7, and perhaps other implementations.

(define-macro (dump thing) 
  `(begin
     (display (symbol->string ',thing))
     (display ": ")
     (display ,thing)
     (newline)))

;; SLIB defines a queue that does what I want ... except there's no
;; way to display a queue.

;; `define-macro' doesn't work on all Scheme implementations.  The
;; only implementation on which I know for sure that it works is
;; Gambit 2.7. and Guile 1.3.4

(define-macro (push! stack datum)
  `(set! ,stack (cons ,datum ,stack)))

(define-macro (pop! stack)
  `(let ((result (car ,stack)))
     (set! ,stack (cdr ,stack))
        
     result))

(define-macro (noisy-if test consequent alternate why-not)
  `(if ,test
       ,consequent
     (begin ,alternate (display ,why-not)
            (newline))))

(define-macro (prepend! datum seq)
  `(if (not (pair? ,seq))
       (set! ,seq (list ,datum))
     (let ((copy-of-first-elt (cons (car ,seq)
                                    (cdr ,seq))))
       (set-car! ,seq
                 ,datum)
       (set-cdr!  ,seq
                  copy-of-first-elt))))
