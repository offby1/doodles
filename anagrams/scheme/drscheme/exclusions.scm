(module exclusions
    mzscheme
  (require "assert.scm"
           (lib "defmacro.ss")
           (prefix srfi-1- (lib "1.ss" "srfi")))
  (provide empty-exclusions excluded? add-exclusion! save-exclusions)
  


  (begin
    ;; The simple-and-youd-think-itd-be-slow-but-its-really-fast way
    (define (empty-exclusions) '())
    (define excluded? member)

    (define-macro (add-exclusion! exclusions datum)
      `(set! ,exclusions (cons ,datum ,exclusions)))

    (define-macro (save-exclusions exclusions . body)
      (let ((saved (gensym))
            (rv (gensym)))
        `(let* ((,saved (srfi-1-list-copy ,exclusions))
                (,rv (begin ,@body)))
           (set! ,exclusions ,saved)
           ,rv
           ))))
  
;     (begin
;       ;; The hopefully-faster way
;       (define (empty-exclusions) (list (make-hash-table)))
;       (define (excluded? datum list)
;         (let loop ((list list))
;           (and (not (null? list))
;                (or (hash-ref (car list) datum)
;                    (loop (cdr list))))))
;       (define-macro (add-exclusion! exclusions datum)
;         `(hash-set! (car ,exclusions) ,datum #t))

;       (define-macro (save-exclusions exclusions . body)
;         ;; In theory, I should use `with-fluids' or `dynamic-wind'.  But
;         ;; I suspect that's slow.
;         (let ((rv (gensym)))
;           `(begin
;              (set! ,exclusions (cons (make-hash-table) ,exclusions))
;              (let ((,rv (begin ,@body)))
;                (set! ,exclusions (cdr ,exclusions))
;                ,rv)))))
    
  
  ;; unit tests

  (define e (empty-exclusions))
  (assert (not (excluded? 'bob e)))
  (add-exclusion! e 'bob)
  (assert (excluded? 'bob e))

  (save-exclusions e
    (assert (excluded? 'bob e))
    (assert (not (excluded? 'sam e)))
    (add-exclusion! e 'sam)
    (assert (excluded? 'sam e)))

  (assert (not (excluded? 'sam e)))
  (assert (excluded? 'bob e))

  (fprintf (current-error-port) (format "exclusions tests passed.~%" )))

;; Local Variables:
;; eval: (put 'save-exclusions 'scheme-indent-function 1)
;; End:
