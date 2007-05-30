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
 combine-counts
)
(define-struct char-counts (ht) #f)
(define (get-count char counter)
  (hash-table-get (char-counts-ht counter) char 0))
(define (inc-count! char counter . amount)
  (if (null? amount)
      (set! amount 1)
    (set! amount (car amount)))
  (hash-table-put! (char-counts-ht counter) char (+ amount (get-count char counter))))
(define (char-counts->string cc)
  (format "~a" (hash-table-map (char-counts-ht cc) cons)))
(define (my-make-char-counts)
  (make-char-counts (make-hash-table)))
(define (combine-counts c1 c2)
  (let ((rv (make-char-counts (hash-table-copy (char-counts-ht c2)))))
    (hash-table-for-each
     (char-counts-ht c1)
     (lambda (left-key left-value)
       (inc-count! left-key rv left-value)))
    rv
    ))
)