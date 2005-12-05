#!/usr/bin/env gosh

(use anagrams)

(define (main args)
  (define op (open-output-string))
  (define argstring
    (let loop ((args (cdr args)))
      (if (null? args)
          (get-output-string op)
        (begin
          (display (car args) op)
          (when (not (null? (cdr args)))
            (display " " op))
          (loop (cdr args))))))
  (display (anagrams argstring)))

