;; 15 Oct 1998

(use-modules (ice-9 syncase))

(defmacro save (symbol)
  `(internal-save ',symbol ,symbol))

(defmacro restore! (symbol)
  `(set! ,symbol (internal-lookup ',symbol)))

(define-syntax fluid-let
  (syntax-rules ()
                ((fluid-let ((name val) ...) body1 body2 ...)
                 (begin
                   (define internal-save   #f)
                   (define internal-lookup #f)

                   (let ((saved-stuff '()))
                     (set! internal-save
                           (lambda (symbol new-value)
                             (let ((old-value (assq symbol saved-stuff)))
                               (if old-value
                                   (set-cdr! old-value new-value)
                                 (set! saved-stuff (cons (cons symbol new-value)
                                                         saved-stuff))))))
                     (set! internal-lookup
                           (lambda (symbol)
                             (let ((saved-value (assq symbol saved-stuff)))
                               (or saved-value (error "can't restore" symbol))
                               (cdr saved-value)))))

                   (save name) ...
                   (dynamic-wind
                    (lambda ()
                      (set! name val)
                      ...)
                    (lambda ()
                      body1 body2 ...)
                    (lambda ()
                      (restore! name)
                      ...))))))



;; a simple test
(let ((a #f)
      (b #f))
  (for-each display (list "A is " a "; b is " b #\newline))
  (fluid-let ((a 3)
              (b 4))
    (for-each display (list "Sum of fluid values is " (+ a b) #\newline))
    (internal-lookup a))
     
  (for-each display (list "A is " a "; b is " b #\newline)))

