(module exceptions mzscheme
  (provide exn:fail:bridge? raise-bridge-error)
  (define-struct (exn:fail:bridge exn:fail) ())
  (define (raise-bridge-error name-symbol text value)
    (raise (make-exn:fail:bridge
            (string->immutable-string
             (format
              "~a: expects something of type <~a>; given ~s"
              name-symbol
              text
              value))
            (current-continuation-marks)))))
