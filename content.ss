#lang scheme

(define checksum? exact-integer?)

(define (sum->path store s)
  (build-path store (number->string s)))

(define sum equal-hash-code)

(define (get store sum)
  (let ((p (sum->path store sum)))
    (with-handlers
        ([exn:fail:filesystem?
          (lambda (v) #f)])
      (call-with-input-file p
        (lambda (ip)
          (read-bytes (file-size p) ip))))))

(define (put! store thing)
  (let ((p (sum->path store (sum thing))))
    (call-with-output-file p
      (lambda (op)
        (write-bytes thing op))
      #:exists 'replace)))

(define (make-store)
  (let ((dirname ".flit"))
    (unless (store? dirname)
      (make-directory dirname))
    (for ((f (in-list (directory-list dirname))))
      (delete-file (build-path dirname f)))
    dirname))

(define (store? thing)
  (directory-exists? thing))

(provide/contract
 [make-store (-> store?)]
 [sum (-> bytes? checksum?)]
 [get (-> store? checksum? (or/c bytes? false?))]
 [put! (-> store? bytes? void)])
