#lang scheme

(require srfi/13
         srfi/14)

(define (executable? candidate)
  (and (file-exists? candidate)
       (memq 'execute (file-or-directory-permissions candidate))))

(define (path-search name)
  (findf executable?
         (map (curryr build-path name)
              (string-tokenize
               (getenv "PATH")
               (char-set-complement (char-set #\:))))))

(provide/contract
 [path-search (string? . -> . (or/c path? #f))])
