(require 'record)

(define make-environment #f)
(define environment? #f)
(define environment-extend! #f)
(define environment-lookup #f)

(let ()

  ;; frames are essentially ALISTs in which the car of each pair is a
  ;; symbol, and no symbol is the car of more than one pair.

  (define frame-rtd (make-record-type "frame" '(data)))

  (define make-frame (lambda () ((record-constructor frame-rtd) '())))
  (define frame? (record-predicate frame-rtd))
  (define frame-bind!
    (lambda (frame name value)
      (if (not (symbol? name))
          (error "This isn't a symbol:" name))
      (let ((already-exists (frame-lookup frame name)))
        (if (pair? already-exists)
            (error "Cannot add " name " to frame; it already exists with value" (cdr already-exists))
          (let ((old ((record-accessor frame-rtd 'data) frame)))
            ((record-modifier frame-rtd 'data) frame (cons (cons name value)
                                                           old)))))))
     ;; If the given NAME is bound, changes its value to VALUE, and
     ;; returns #t.  Otherwise returns #f.
  (define frame-set!
    (lambda (frame name value)
      (let ((already-exists (assq name ((record-accessor frame-rtd 'data) frame))))
        (if (pair? already-exists)
            (begin
              (set-cdr! already-exists value)
              #t)
          #f))))

     ;; returns #f if the name isn't bound in the frame, or a list
     ;; containing the bound value.
  (define frame-lookup
    (lambda (frame name)
          
      ;; don't return the pair itself, because then clients could
      ;; modify the value directly.  Instead return a new list
      ;; whose sole element is the value.
      (let ((result (assq name ((record-accessor frame-rtd 'data) frame))))
        (if (pair? result)
            (list (cdr result))
          result))))


  (define environment-rtd (make-record-type "environment" '(data)))

  (set! make-environment (lambda () ((record-constructor environment-rtd ) '())))
  (set! environment? (record-predicate environment-rtd))
  (set! environment-extend!
        (lambda (env name-value-pairs)
          (let ((frame (make-frame))
                (old ((record-accessor environment-rtd 'data) env)))
            (let loop ((name-value-pairs name-value-pairs))
              (if (not (null? name-value-pairs))
                  (begin
                    (frame-bind! frame
                                 (car (car name-value-pairs))
                                 (cdr (car name-value-pairs)))
                    (loop (cdr name-value-pairs)))))
            ((record-modifier environment-rtd 'data) env (cons frame old)))))

  (set! environment-lookup (lambda (env name)
                             (let loop ((frames ((record-accessor environment-rtd 'data) env))
                                        (result #f))
                               (if (null? frames)
                                   result
                                 (let ((in-this-frame (frame-lookup (car frames) name)))
                                   (if (pair? in-this-frame)
                                       in-this-frame
                                     (loop (cdr frames)
                                           #f)))))))
  (set! environment-pop! (lambda (env)
                           (let ((frames  ((record-accessor environment-rtd 'data) env)))
                             (if (null? frames)
                                 (error "Cannot pop empty environment")
                               ((record-modifier environment-rtd 'data) env (cdr frames)))))))

