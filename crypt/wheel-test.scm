#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec mzscheme -qu "$0" ${1+"$@"}
|#

(module wheel-test mzscheme
(require (only (lib "1.ss" "srfi")
               drop
               every
               filter
               iota
               take)
         (only (lib "13.ss" "srfi")
               string-filter
               string-join
               string-take
               string-take-right)
         "spindle.scm")

(print-struct #t)

(define *wheels-per-spindle* 30)

(define (split-into-short-strings input max-length)
  (let loop ((input (string-downcase (string-filter char-alphabetic? input)))
             (output '()))
    (if (zero? (string-length input))
        (reverse output)
      (let* ((input-length (string-length input))
             (left (min input-length max-length))
             (right (- input-length left)))
        (loop (string-take-right input right)
              (cons (string-take input left) output))))))

(define (counted-string s)
  (format "~a:~s"(string-length s) s))

(define encrypting? #f)
(define text #f)
(define encryption-key #f)

(cond
 ((= 1 (vector-length (current-command-line-arguments)))
  (set! encrypting? #t)
  (set! text (string-join (vector->list (current-command-line-arguments)))))
 ((= 2 (vector-length (current-command-line-arguments)))
  (set! encryption-key (string->number (vector-ref (current-command-line-arguments) 0)))
  (set! text (string-join (cdr (vector->list (current-command-line-arguments))))))
 (else
  (error "Usage: wheel-test [key] string" )))

(if encryption-key
    (random-seed encryption-key)
  (let ((new-key (modulo (current-milliseconds) 2147483647)))
    (printf "Key: ~s~%" new-key)
    (random-seed new-key)))

(let ((s (make-spindle *wheels-per-spindle*)))
  (for-each (lambda (str)
              (rotate-to-display! s str)
              (printf "~%~a~%" (tableaux s)))
       (split-into-short-strings text *wheels-per-spindle*)))
)
