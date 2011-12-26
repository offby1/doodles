#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec racket -l errortrace --require "$0" --main -- ${1+"$@"}
|#

#lang racket
(require rackunit
         rackunit/text-ui
         racket/trace)

(define key? number?)                   ; TODO -- make this, and our
                                        ; equality and ordering tests,
                                        ; parameterizable

(define (tree-count t)
  (sequence-length (in-dict-keys t)))

;; Our "pos" is a stack of tree nodes -- the nodes we need to pass
;; through to get to a particular node.

;; TODO -- rewrite to use find-node, passing some magic value for the
;; sought key, such that that magic key is always smaller than all the
;; existing keys.  (Or for that matter, pass an "equal?" and "<"
;; procedure that always return #f and #t respectively)
(define (tree-iterate-first t [pos '()])
  (cond
   ((tree-empty? t)
    #f)
   ((tree-empty? (tree-left t))
    (cons t pos))
   (else
    (tree-iterate-first (tree-left t) (cons t pos)))))

(define (tree-iterate-next t pos)
  ;; BUGBUG -- raise exn:fail:contract if POS is not valid for t
  (let loop ([pos pos])
    (cond
     ((null? pos) #f)
     ((not (tree-empty? (tree-right (car pos))))
      (tree-iterate-first (tree-right (car pos)) (cdr pos)))
     ((null? (cdr pos)) #f)
     ((< (tree-key (cadr pos))
         (tree-key (car pos)))
      (loop (cdr pos)))
     (else (cdr pos)))))

(define (check-round-trip . seq)
  (check-equal? (map car (tree->alist (ql->t seq)))
                (sort seq <)
                (format "Round-tripping ~a" seq)))

(define-test-suite iterate-tests
  (check-false  (tree-iterate-first (public-make-tree)))
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

(define (make-tree k v [l #f] [r #f])
  (set! l (or l (public-make-tree)))
  (set! r (or r (public-make-tree)))
  (tree (node k v l r)))

(define (tree-empty? t)
  (not (tree-node-or-false t)))

(define (find-subtree t k [stack '()])
  (cond
   ((tree-empty? t) (cons t stack))
   ((equal? k (tree-key t)) (cons t stack))
   ((< k (tree-key t))
    (find-subtree (tree-left t) k (cons t stack)))
   (else
    (find-subtree (tree-right t) k (cons t stack)))))

(define (tree-ref t k [failure-result (lambda ()
                                        (raise
                                         (make-exn:fail
                                          "Not found"
                                          (current-continuation-marks))))])
  (let ([path (find-subtree t k)])
    (cond
     ((not (tree-empty? (car path)))
      (tree-value (car path)))
     ((procedure? failure-result) (failure-result))
     (else failure-result))))

(define (tree-set t k v)
  (cond
   ((tree-empty? t)
    (make-tree k v))
   ((equal? k (tree-key t))
    (make-tree
     k v
     (tree-left t)
     (tree-right t)))
   ((< k (tree-key t))
    (make-tree (tree-key t) (tree-value t)
          (tree-set (tree-left t) k v)
          (tree-right t)))
   (else
    (make-tree (tree-key t) (tree-value t)
          (tree-left t)
          (tree-set (tree-right t) k v)))))

(define (tree-remove t k)

  (define (decapitate target)

    (define (in-order-successor t)
      ((lambda (x) (tree? x)) . -> . (or/c false? (lambda (x) (tree? x))))
      (cond
       ((tree-iterate-first (tree-right t)) => car)
       (else #f)))

    (cond
     ((and (tree-empty? (tree-left  target))
           (tree-empty? (tree-right target)))
      (tree-left target))
     ((tree-empty? (tree-left target))
      (tree-right target))
     ((tree-empty? (tree-right target))
      (tree-left target))
     (else
      ;; TODO -- flip a coin, and choose child randomly so as to not
      ;; get too unbalanced
      (let ([s (in-order-successor target)])
        (make-tree (tree-key s)
                   (tree-value s)
                   (tree-left target)
                   (tree-right s))))))

  (for ([permutation '((1 2 3)
                       (1 3 2)
                       (3 1 2)
                       (3 2 1)
                       (2 3 1)
                       (2 1 3))])
    (check-equal? (map car (tree->alist (decapitate (ql->t permutation))))
                  (sort (cdr permutation) <)
                  (format "~a" permutation)))

  (let ([stack-o-trees (find-subtree t k)])
    (define (which-child parent child)
      (cond
       ((eq? child (tree-left parent))
        'left)
       ((eq? child (tree-right parent))
        'right)
       (else
        (error 'which-child (format "~a is not a parent of ~a" parent child)))))

    (cond
     ((tree-empty? (car stack-o-trees))
      t)
     ((null? (cdr stack-o-trees))
      (decapitate (car stack-o-trees)))
     (else
      (for/fold ([result (decapitate (car stack-o-trees))])
          ([child  (cdr stack-o-trees)]
           [parent stack-o-trees])
          (case (which-child child parent)
            ((left)
             (make-tree
              (tree-key child)
              (tree-value child)
              result
              (tree-right child)))
            ((right)
             (make-tree
              (tree-key child)
              (tree-value child)
              (tree-left child)
              result))
            ))))))

(define (public-make-tree) (tree #f))
(provide (rename-out [public-make-tree tree]))

(define prop-dict-vector
  (vector

   tree-ref
   #f                                   ;set!
   tree-set

   #f                                   ;remove!
   tree-remove
   tree-count

   tree-iterate-first
   tree-iterate-next
   tree-iterate-key

   tree-iterate-value
   ))

;; this is overkill :)
;; (for ([proc prop-dict-vector]) (when (procedure? proc) (trace proc)))

(struct tree (node-or-false)

        #:guard (lambda (thing type-name)
                  (when (and (not (node? thing))
                             (not (false? thing)))
                    (error type-name "struct tree be wantin' a node or false, yo; not ~s" thing))
                  thing)

        #:property prop:dict prop-dict-vector
        #:transparent)

(struct node (key value left right)
        #:guard (lambda (key value left right type-name)
                  (when (not (tree? left))
                    (error type-name "My left child gotta be a tree, not ~s" left))
                  (when (not (tree? right))
                    (error type-name "My right child gotta be a tree, not ~s" right))
                  (when (not (key? key))
                    (error type-name "My key gotta be a number, not ~s" key))
                  (values key value left right))
        #:transparent)

;; Convenience wrappers
(define (tree-left  t) (node-left  (tree-node-or-false t)))
(define (tree-right t) (node-right (tree-node-or-false t)))
(define (tree-key   t) (node-key   (tree-node-or-false t)))
(define (tree-value t) (node-value (tree-node-or-false t)))

(define (tree->alist t)
  (dict-map t cons))

(define (list->tree l)
  (for/fold ([t (public-make-tree)])
      ([p  l])
      (tree-set t (car p) (cdr p))))

;; quick list->tree.  Just for testing.
(define (ql->t keys)
  (list->tree (map (lambda (k) (cons k k)) keys)))

(define-test-suite misc-tests
  (let ([t (public-make-tree)])
    (check-equal? (list->tree (tree->alist t)) t)
    (set! t (tree-set t 2 'two))
    (check-equal? (list->tree (tree->alist t)) t))

  (check-equal? (tree-count (ql->t '(1 2 3))) 3)

  (check-equal? (dict-ref (public-make-tree) 0 (thunk 'not-found))
                'not-found))

(define-test-suite more-misc-tests
  (check-true (tree-empty? (public-make-tree)) "empty")
  (let ([t3  (tree-set (public-make-tree) 3 'three)])
    (check-false (tree-empty? t3) "not empty")
    (check-false (tree-ref t3 6 (thunk #f)) "failure thunk when not found")
    (check-equal? (tree-ref t3 6 'not-found) 'not-found "failure non-thunk when not found")
    (check-equal? (tree-ref t3 3)  'three "found 3")
    (let ([t4 (tree-set t3 4 'four)])
      (check-equal? (tree-ref t4 3 ) 'three "3 still in new tree")
      (check-equal? (tree-ref t4 4) 'four "4 in new tree too")

      (for ([permutation '((1 2 3)
                           (1 3 2)
                           (3 1 2)
                           (3 2 1)
                           (2 3 1)
                           (2 1 3))])
        (for ([elt '(1 2 3)])
          (let ([me  (tree-remove (ql->t permutation) elt)]
                [plt (dict-remove (tree->alist (ql->t permutation)) elt)])
            (check-equal? (tree->alist me)
                          plt
                          (format "~a minus ~a => ~a" permutation elt plt)))))

      (let ([t (tree-remove t4 12345)])
        (check-equal? (tree->alist t)
                      (tree->alist t4))
        (set! t (tree-remove t 3))
        (check-equal? (tree->alist t)
                      '((4 . four))))))
  (let ([t (ql->t '(3))])
    (check-not-false (tree-empty? (dict-remove t 3)))))

(define-test-suite all-tests
  iterate-tests
  misc-tests
  more-misc-tests)


(provide main)
(define (main . args)
  (random-seed 2)
  (exit (run-tests all-tests 'verbose)))
