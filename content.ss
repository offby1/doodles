#lang scheme

(require file/gzip
         file/gunzip
         (planet "hash-store.ss" ("jaymccarthy" "hash-store.plt")))

(define checksum? bytes?)

(define sum SHA1)

(define (sum->path store s)
  (build-path store s))

(define (get store expected-sum)
  (let* ((content (lookup store expected-sum))
         (actual-sum (sum content)))
    (unless (equal? expected-sum actual-sum)
      (error 'get
             "Corrupted file: ~a.  Sum is ~a but we expected ~a"
             store actual-sum expected-sum))
    content))

(define put! store!)

(define (make-store)
  (create (build-path ".flit")))

(define store? hash-store?)

(provide/contract
 [make-store (-> store?)]
 [sum (-> bytes? checksum?)]
 [get (-> store? checksum? (or/c bytes? false?))]
 [put! (-> store? bytes? void)])
