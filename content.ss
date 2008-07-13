#lang scheme

(define checksum? exact-integer?)

(define (get store c)
  #f)

(define (put store thing)
  #f)

(define (make-store)
  #f)

(define (store? thing)
  #t)

(provide/contract
 [make-store (-> store?)]
 [get (-> store? checksum? any)]
 [put (-> store? any/c checksum?)])
