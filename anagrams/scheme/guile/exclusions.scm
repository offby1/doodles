(define-module (exclusions))


(if #t
    (begin
      ;; The simple-and-youd-think-itd-be-slow-but-its-really-fast way
      (define-public (empty-exclusions) '())
      (define-public excluded? member)

      (define-macro (add-exclusion! exclusions datum)
        `(set! ,exclusions (cons ,datum ,exclusions)))
      (export add-exclusion!)

      )
  (begin
    ;; The I'd-have-thought-itd-be-faster way
    (define-public (empty-exclusions) (list (make-hash-table)))
    (define-public (excluded? datum list)
      (let loop ((list list))
        (and (not (null? list))
             (or (hash-ref (car list) datum)
                 (loop (cdr list))))))
    (define-macro (add-exclusion! exclusions datum)
      `(hash-set! (car ,exclusions) ,datum #t))
    (export add-exclusion!)
))

;; unit tests
(use-modules (assert))
(define e (empty-exclusions))
(assert (not (excluded? 'bob e)))
(add-exclusion! e 'bob)
(assert (excluded? 'bob e))

(format #t "~a tests passed.~%" (module-name (current-module)))

;; Local Variables:
;; eval: (put 'save-exclusions 'scheme-indent-function 1)
;; End:
