(module hello-world    ; the module name  
  mzscheme       ; initial syntax and variable bindings  
  ;  for the module body  
  ; the module body  
  (define (hello)
    (display "Hello world!")  
    (newline))
  (provide hello)) 