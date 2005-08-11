;;   -*- mode: scheme48; scheme48-package: config -*-
(define-structure dict (export *the-hash-table*) 
  (open scheme tables reduce primitives) 
  (files dict)) 

(define-structure wc (export wc)
  (open tables scheme dict)
  (files wc))