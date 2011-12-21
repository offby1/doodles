#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec racket -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang racket
(require rackunit
         rackunit/text-ui
         racket/trace)

(define (tree-count t)
  (sequence-length (in-dict-keys t)))

;; Our "pos" is a stack of tree nodes -- the nodes we need to pass
;; through to get to a particular node.

(define (tree-iterate-first t [pos '()])
  (cond
   ((tree-empty? t)
    #f)
   ((not (tree-left t))
    (cons t pos))
   (else
    (tree-iterate-first (tree-left t) (cons t pos)))))

(define (tree-iterate-next t pos)
  ;; BUGBUG -- raise exn:fail:contract if POS is not valid for t
  (let loop ([pos pos])
    (cond
     ((null? pos) #f)
     ((tree-right (car pos))
      (tree-iterate-first (tree-right (car pos)) (cdr pos)))
     ((null? (cdr pos)) #f)
     ((< (unbox (tree-key (cadr pos)))
         (unbox (tree-key (car pos))))
      (loop (cdr pos)))
     (else (cdr pos)))))

(define (check-round-trip . seq)
  (check-equal? (map car (tree->list (ql->t seq)))
                (sort seq <)))

(define-test-suite iterate-tests
  (check-false  (tree-iterate-first (make-tree)))
  (check-round-trip 2)
  (check-round-trip 2 3 1)
  (check-round-trip 7 0 6 4)
  (check-round-trip 0 1)
  (check-equal? 3 (tree-count (ql->t '(1 2 3))))
  (apply check-round-trip (shuffle (build-list 100 values))))

(define (tree-iterate-key t pos)
  ;; BUGBUG -- raise exn:fail:contract if pos isn't valid for t
  (tree-key (car pos)))

(define (tree-iterate-value t pos)
  ;; BUGBUG -- raise exn:fail:contract if pos isn't valid for t
  (tree-value (car pos)))

(define (make-tree) (tree #f #f #f #f))
(define (public-tree-empty? t) (not (box? (tree-key t))))
(define (tree-empty? t) (or (not t) (public-tree-empty? t)))
(define (tree-ref t k [failure-result (lambda ()
                                        (raise
                                         (make-exn:fail
                                          "Not found"
                                          (current-continuation-marks))))])
  (cond
   ((tree-empty? t)
    (if (procedure? failure-result)
        (failure-result)
        failure-result))
   ((equal? k (unbox (tree-key t)))
    (tree-value t))
   ((< k (unbox (tree-key t)))
    (tree-ref (tree-left t) k failure-result))
   (else
    (tree-ref (tree-right t) k failure-result))))

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

(provide (rename-out [make-tree tree]))
(struct tree (key value left right)

        #:property prop:dict (vector

                              tree-ref
                              #f        ;set!
                              tree-set

                              #f        ;remove!
                              tree-remove
                              tree-count

                              tree-iterate-first
                              tree-iterate-next
                              tree-iterate-key

                              tree-iterate-value
                              )
        #:transparent)

(define (tree->list t) (dict-map t (lambda (k v) (cons (unbox k) v))))

(define (list->tree l)
  (for/fold ([t (make-tree)])
      ([p  l])
      (tree-set t (car p) (cdr p))))

;; quick list->tree.  Just for testing.
(define (ql->t keys)
  (list->tree (map (lambda (k) (cons k k)) keys)))

(define-test-suite misc-tests
  (let ([t (make-tree)])
    (check-equal? (list->tree (tree->list t)) t)
    (set! t (tree-set t 2 'two))
    (check-equal? (list->tree (tree->list t)) t))

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

  (check-equal? (dict-ref (make-tree) 0 (thunk 'not-found))
                'not-found))

(define-test-suite all-tests
  iterate-tests
  misc-tests
  (check-true (tree-empty? (make-tree)) "empty")
  (let ([t3  (tree-set (make-tree) 3 'three)])
    (check-false (tree-empty? t3) "not empty")
    (check-false (tree-ref t3 6 (thunk #f)) "failure thunk when not found")
    (check-equal? (tree-ref t3 6 'not-found) 'not-found "failure non-thunk when not found")
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

  )

(provide main)
(define (main . args)
  (random-seed 2)
  (exit (run-tests all-tests 'verbose)))
