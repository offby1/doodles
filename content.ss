#lang scheme

(define checksum? exact-integer?)

(define (get store c)
  (let ((sum (equal-hash-code c)))
    (hash-ref store sum #f)))

(define (put store thing)
  (let ((sum (equal-hash-code thing)))
    (values (hash-set store sum
                      thing)
            sum)))

(define (make-store)
  (make-immutable-hash '()))

(define (store? thing)
  #t)

(provide/contract
 [make-store (-> store?)]
 [get (-> store? checksum? any)]
 [put (-> store? any/c (values store? checksum?))])
