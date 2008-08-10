#lang scheme

(require (lib "13.ss" "srfi")
         (lib "14.ss" "srfi"))

(define (path-search name)
  (findf (lambda (candidate)
           (and (file-exists? candidate)
                (memq 'execute (file-or-directory-permissions candidate))))
         (map (lambda (d)
                (build-path d name))
              (string-tokenize
               (getenv "PATH")
               (char-set-complement (char-set #\:))))))
