(module contract mzscheme

  (require (lib "trace.ss"))
  (provide
   make-contract contract-bid contract-risk)

  (print-struct #t)

  (define-struct contract (bid seat risk) #f))
