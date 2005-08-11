;;   -*- mode: scheme48; scheme48-package: config -*-
(define-structure boink (export *the-hash-table*) 
  (open scheme tables reduce primitives) 
  (files boink)) 

(define-structure wc (export wc)
  (open tables scheme boink)
  (files wc))