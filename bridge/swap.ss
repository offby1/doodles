(module swap mzscheme
  (provide swap!)
  (define-syntax swap!
    (syntax-rules ()
      ((_ a b)
       (let ((tmp a))
         (set! a b)
         (set! b tmp))))))
