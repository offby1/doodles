;;   -*- mode: scheme48; scheme48-package: config -*-
(define-structure dict (export *the-hash-table*) 
  (open scheme tables reduce primitives) 
  (files dict)) 

(define-structure wc (export wc)
  (open scheme tables dict bfs filter)
  (files wc))

(define-structure set (export make-set is-present? add!)
  (open scheme tables)
  (files set))

(define-structure bfs (export bfs)
  (open scheme signals filter i/o srfi-9 tables set q)
  (files bfs))

(define-structure q (export make-queue insert-queue! delete-queue! empty-queue? front-queue)
  (open scheme srfi-9 signals)
  (files q))

(define-structure filter (export filter)
  (open scheme)
  (files filter))