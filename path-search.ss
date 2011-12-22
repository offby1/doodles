#lang scheme

(define/contract (path-search name)
  (string? . -> . (or/c path? #f))

  (define (executable? candidate)
    (and (file-exists? candidate)
         (memq 'execute (file-or-directory-permissions candidate))))

  (findf executable?
         (map (curryr build-path name)
              (regexp-split #rx":" (getenv "PATH")))))
