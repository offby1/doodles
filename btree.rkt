#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec racket -l errortrace --require "$0" --main -- ${1+"$@"}
|#

;; slavishly following http://en.wikipedia.org/wiki/Binary_search_tree

#lang racket
(require rackunit
         rackunit/text-ui
         racket/trace)

(struct tree (key value left right)

        ;; #:property prop:dict (vector

        ;;                       tree-ref
        ;;                       #f        ;set!
        ;;                       tree-set

        ;;                       #f        ;remove!
        ;;                       tree-remove
        ;;                       tree-count

        ;;                       tree-iterate-first
        ;;                       tree-iterate-next
        ;;                       tree-iterate-key

        ;;                       tree-iterate-value
        ;;                       )
        )

(define (tree-count t) 'fixme)
(define (tree-iterate-first t) 'fixme)
(define (tree-iterate-next t) 'fixme)
(define (tree-iterate-key t) 'fixme)
(define (tree-iterate-value t) 'fixme)

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

;; fairly lame -- builds up a whole new tree while omitting the
;; specified element
(define (tree-remove t k)
  (cond
   ((tree-empty? t)
    t)
   ((equal? k
            (unbox (tree-key t)))
    (merge-trees (tree-remove (tree-left t) k)
                 (tree-remove (tree-right t) k)))
   ((< k (unbox (tree-key t)))
    (tree (tree-key t) (tree-value t)
          (tree-remove (tree-left t) k)
          (tree-right t)))
   (else
    (tree (tree-key t) (tree-value t)
          (tree-left t)
          (tree-remove (tree-right t) k)))))

(define (merge-trees a b)
  (tree-fold a b
             (lambda (accum k v left right )
               (tree-set accum k v))))

(define (tree-fold t accum proc)
  (cond
   ((tree-empty? t)
    accum)
   (else (proc accum
               (unbox (tree-key t))
               (tree-value t)
               (tree-fold (tree-left t) accum proc)
               (tree-fold (tree-right t) accum proc)
               ))))

(define (tree->list t)
  (tree-fold t '()
             (lambda (accum k v left right )
               (cons (list k v left right) accum))))

(define-test-suite all-tests
  (check-true (tree-empty? (make-tree)) "empty")
  (let ([t3  (tree-set (make-tree) 3 'three)])
    (check-false (tree-empty? t3) "not empty")
    (check-false (tree-ref t3 6 (thunk #f)) "failure thunk when not found")
    (check-equal? (tree-ref t3 3)  'three "found 3")
    (let ([t4 (tree-set t3 4 'four)])
      (check-equal? (tree-ref t4 3 ) 'three "3 still in new tree")
      (check-equal? (tree-ref t4 4) 'four "4 in new tree too")

      (let ([t (tree-remove t4 12345)])
        (check-equal? (tree->list t)
                      (tree->list t4))
        (set! t (tree-remove t 3))
        (check-equal? (tree->list t)
                      '((4 four () ()))))))

  (let ([t
         (for/fold ([t (make-tree)])
             ([i 100])
             (tree-set t (random 100) 'frotz))])
    (check-false (tree-empty? t))
    (displayln t)
    (pretty-print (tree->list t))))

(provide main)
(define (main . args)
  (exit (run-tests all-tests 'verbose)))
