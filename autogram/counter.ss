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
(define (inc-count! char counter . amount)
  (when (null? amount)
    (set! amount 1))
  (hash-table-put! (char-counts-ht counter) char (+ amount (get-count char counter))))
(define (char-counts->string cc)
  (format "~a" (hash-table-map (char-counts-ht cc) cons)))
(define (my-make-char-counts)
  (make-char-counts (make-hash-table)))
(define (combine-counts c1 c2)
  (let ((rv (my-make-char-counts)))
    (hash-table-for-each
     (char-counts-ht c1)
     (lambda (left-key left-value)
       (inc-count! left-key rv (+ left-value (get-count left-key c2)))))

    rv
    ))
)