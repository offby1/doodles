(define-module (exclusions))


(if #t
    (begin
      ;; The simple-and-possibly-slow way
      (define-public (empty-exclusions) '())
      (define-public excluded? member)

      (define-macro (add-exclusion! exclusions datum)
        `(set! ,exclusions (cons ,datum ,exclusions)))
      (export add-exclusion!)

      (define-macro (save-exclusions exclusions . body)
        (let ((saved (gensym))
              (rv (gensym)))
          `(let* ((,saved (copy-tree ,exclusions))
                  (,rv (begin ,@body)))
             (set! ,exclusions ,saved)
             ,rv
             )))
      (export save-exclusions))
  (begin
    ;; The hopefully-faster way
    (define-public (empty-exclusions) (list (make-hash-table)))
    (define-public (excluded? datum list)
      (let loop ((list list))
        (and (not (null? list))
             (or (hash-ref (car list) datum)
                 (loop (cdr list))))))
    (define-macro (add-exclusion! exclusions datum)
      `(hash-set! (car ,exclusions) ,datum #t))
    (export add-exclusion!)
    (define-macro (save-exclusions exclusions . body)
      ;; In theory, I should use `with-fluids' or `dynamic-wind'.  But
      ;; I suspect that's slow.
      (let ((rv (gensym)))
        `(begin
           (set! ,exclusions (cons (make-hash-table) ,exclusions))
           (let ((,rv (begin ,@body)))
             (set! ,exclusions (cdr ,exclusions))
             ,rv))))
    (export save-exclusions)))

;; unit tests
(use-modules (assert))
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

(format #t "~a tests passed.~%" (module-name (current-module)))

;; Local Variables:
;; eval: (put 'save-exclusions 'scheme-indent-function 1)
;; End:
