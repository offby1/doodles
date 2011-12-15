#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec racket -l errortrace --require "$0" --main -- ${1+"$@"}
|#

;; slavishly following http://en.wikipedia.org/wiki/Binary_search_tree

#lang racket
(require rackunit
         rackunit/text-ui
         racket/generator
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
        #:transparent)

(define (tree-count t) 'fixme)

(define (tree-iterate-first t)
  (generator ()
    (let loop ([t t])
      (cond
       ((tree-empty? t) #f)
       (else
        (loop (tree-left t))
        (yield (cons (unbox (tree-key t))
                     (tree-value t)))
        (loop (tree-right t)))))))

(define (tree-iterate-next g)
  (let ([s (generator-state g)])
    (and (not (eq? 'done s))
         (g))))

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
  (cond
   ((tree-empty? a)
    b)
   ((tree-empty? b)
    a)
   ((< (unbox (tree-key a))
       (unbox (tree-key b)))
    (tree (tree-key a)
          (tree-value a)
          (tree-left a)
          (merge-trees (tree-right a)
                       b)))
   ((= (unbox (tree-key a))
       (unbox (tree-key b)))
    (tree (tree-key a)
          (tree-value a)
          (merge-trees (tree-left a)
                       (tree-left b))
          (merge-trees (tree-right a)
                       (tree-right b))))
   (else
    (tree (tree-key a)
          (tree-value a)
          (merge-trees (tree-left a)
                       b)
          (tree-right a)))))

(define (tree->list t)
  (for/list ([p (in-producer (tree-iterate-first t) #f)])
    p))

(check-not-false (tree-empty?
                  (merge-trees (make-tree)
                               (make-tree))))
(check-equal? (tree->list
               (merge-trees (make-tree)
                            (tree-set (make-tree) 2 3)))
              '((2 . 3)))

(let ([t (tree-set (make-tree) 2 3)])
  (check-equal? (tree->list
                 (merge-trees  t t ))
                '((2 . 3))))

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
                      '((4 . four))))))

  (let ([t
         (for/fold ([t (make-tree)])
             ([i 100])
             (tree-set t (random 100) 'frotz))])
    (check-false (tree-empty? t))
    (for/fold ([t t])
              ([key (in-list (map car (tree->list t)))])
      (displayln (tree->list t))
      (tree-remove t key))))


(provide main)
(define (main . args)
  (exit (run-tests all-tests 'verbose)))
