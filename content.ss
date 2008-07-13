#lang scheme

(define checksum? exact-integer?)

(define (get store c)
  #f)

(define (put store thing)
  (values (make-store) 123))

(define (make-store)
  #f)

(define (store? thing)
  #t)

(provide/contract
 [make-store (-> store?)]
 [get (-> store? checksum? any)]
 [put (-> store? any/c (values store? checksum?))])
