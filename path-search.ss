#lang scheme

(require (lib "13.ss" "srfi")
         (lib "14.ss" "srfi"))

(define (find-first-exe name dirs)
  (findf (lambda (candidate)
           (and (file-exists? candidate)
                (memq 'execute (file-or-directory-permissions candidate))))
         (map (lambda (d)
                (build-path d name))
              dirs)))

(define (path-search name)
  (find-first-exe name (string-tokenize (getenv "PATH") (char-set-complement (char-set #\:)))))