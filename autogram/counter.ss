#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module counter mzscheme
(provide
 get-count
 inc-count!
 (rename my-make-char-counts make-count)
 char-counts->string
)
(define-struct char-counts (ht) #f)
(define (get-count char counter)
  (hash-table-get (char-counts-ht counter) char 0))
(define (inc-count! char counter)
  (hash-table-put! (char-counts-ht counter) char (add1 (get-count char counter))))
(define (char-counts->string cc)
  (format "~a" (hash-table-map (char-counts-ht cc) cons)))
(define (my-make-char-counts)
  (make-char-counts (make-hash-table)))
)