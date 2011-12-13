#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec racket -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang racket
(require rackunit
         rackunit/text-ui
         racket/trace)

(struct tree (key value left right) #:transparent)

(define (make-tree) (tree #f #f #f #f))
(define (public-tree-empty? t) (not (box? (tree-key t))))
(define (tree-empty? t) (or (not t) (public-tree-empty? t)))
(define (tree-ref-internal t k failure-result)
  (cond
   ((tree-empty? t)
    (failure-result))
   ((equal? k (unbox (tree-key t)))
    t)
   ((< k (unbox (tree-key t)))
    (tree-ref-internal (tree-left t) k failure-result))
   (else
    (tree-ref-internal (tree-right t) k failure-result))))


(define (tree-ref t k
                  [failure-result (lambda ()
                                    (raise
                                     (make-exn:fail
                                      "Not found"
                                      (current-continuation-marks))))])
  (let ([int (tree-ref-internal t k failure-result)])
    (cond
     (int => tree-value)
     (else
      (failure-result)))))

(define (tree-set t k v)
  (cond
   ((tree-empty? t)
    (tree (box k) v #f #f))
   ((equal? k
            (unbox (tree-key t)))
    (tree
     (box k) v
     (tree-left t)
     (tree-right t)))
   ((< k (unbox (tree-key t)))
    (tree (tree-key t) (tree-value t)
          (tree-set (tree-left t) k v)
          (tree-right t)))
   (else
    (tree (tree-key t) (tree-value t)
          (tree-left t)
          (tree-set (tree-right t) k v)))))

(define (tree-fold t init proc)
  (cond
   ((tree-empty? t)
    init)
   (else (proc (unbox (tree-key t))
               (tree-value t)
               (tree-fold (tree-left t) init proc)
               (tree-fold (tree-right t) init proc)
               ))))

(define (tree->list t)
  (tree-fold t '() list))

(define-test-suite all-tests
  (check-true (tree-empty? (make-tree)) "empty")
  (let ([t3  (tree-set (make-tree) 3 'three)])
    (check-false (tree-empty? t3) "not empty")
    (check-false (tree-ref t3 6 (thunk #f)) "failure thunk when not found")
    (check-equal? (tree-ref t3 3)  'three "found 3")
    (let ([t4 (tree-set t3 4 'four)])
      (check-equal? (tree-ref t4 3 ) 'three "3 still in new tree")
      (check-equal? (tree-ref t4 4) 'four "4 in new tree too")))
  (let ([t
         (for/fold ([t (make-tree)])
             ([i 100])
             (tree-set t (random 100) 'frotz))])
    (check-false (tree-empty? t))
    (pretty-print (tree->list t))))

(provide main)
(define (main . args)
  (exit (run-tests all-tests 'verbose)))
