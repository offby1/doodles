#lang scheme

(define checksum? exact-integer?)

(define (sum->path store s)
  (build-path store (number->string s)))

(define sum equal-hash-code)

(define (get store expected-sum)
  (let ((p (sum->path store expected-sum)))
    (with-handlers
        ([exn:fail:filesystem?
          (lambda (v) #f)])
      (call-with-input-file p
        (lambda (ip)
          (let* ((content (read-bytes (file-size p) ip))
                 (actual-sum (sum content)))
            (unless (equal? expected-sum actual-sum)
              (error 'get
                     "Corrupted file: ~a.  Sum is ~a but we expected ~a"
                     p actual-sum expected-sum))
            content))))))

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
      (with-handlers
          ([exn:fail:filesystem? void])
        (delete-file (build-path dirname f))))
    dirname))

(define (store? thing)
  (directory-exists? thing))

(provide/contract
 [make-store (-> store?)]
 [sum (-> bytes? checksum?)]
 [get (-> store? checksum? (or/c bytes? false?))]
 [put! (-> store? bytes? void)])
