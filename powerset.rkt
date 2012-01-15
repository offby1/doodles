#lang racket

(define (with-extra item seqs)
  (apply set
         (set-map seqs
                  (lambda (s)
                    (set-add s item)))))

(define (powerset seq)
  (cond
   ((null? seq)
    (set))
   ((null? (cdr seq))
    (set (set) (set (car seq))))
   (else
    (set-union
     (powerset (cdr seq))
     (with-extra (car seq)
                 (powerset (cdr seq)))))))
