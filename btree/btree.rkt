#lang racket
(include "convenience.rkt")
(define key? number?)                   ; TODO -- make this, and our
                                        ; equality and ordering tests,
                                        ; parameterizable

(define (tree-count t)
  (sequence-length (in-dict-keys t)))

;; Our "pos" is a stack of tree nodes -- the nodes we need to pass
;; through to get to a particular node, starting at the root.

;; TODO -- combine with find-subtree -- they're awfully similar
(define (tree-iterate-first t [pos (make-pos t)])
  (cond
   ((tree-empty? t)
    #f)
   ((tree-empty? (tree-left t))
    (pos-push t pos))
   (else
    (tree-iterate-first (tree-left t) (pos-push t pos)))))

(define (tree-iterate-next t pos)
  (pos-validate t pos)
  (let loop ([pos pos])
    (cond
     ((not (tree-empty? (tree-right (pos-head pos))))
      (tree-iterate-first (tree-right (pos-head pos)) (pos-rest pos)))
     ((pos-empty? (pos-rest pos)) #f)
     ((< (tree-key (pos-head (pos-rest pos)))
         (tree-key (pos-head pos)))
      (loop (pos-rest pos)))
     (else (pos-rest pos)))))

(define (tree-iterate-key t pos)
  (pos-validate t pos)
  (tree-key (pos-head pos)))

(define (tree-iterate-value t pos)
  (pos-validate t pos)
  (tree-value (pos-head pos)))

(define (make-tree k v
                   #:left [l #f]
                   #:right [r #f]
                   #:MaxNodeCount [MaxNodeCount 1])
  (set! l (or l (public-make-tree)))
  (set! r (or r (public-make-tree)))
  (tree (node k v l r
              (add1 (+   (tree-weight l) (tree-weight r))))
        MaxNodeCount))

(define (tree-empty? t)
  (not (tree-node-or-false t)))

(define (find-subtree t k [stack (make-pos t)])
  (cond
   ((tree-empty? t) (pos-push t stack))
   ((equal? k (tree-key t)) (pos-push t stack))
   ((< k (tree-key t))
    (find-subtree (tree-left t) k (pos-push t stack)))
   (else
    (find-subtree (tree-right t) k (pos-push t stack)))))

(define (tree-ref t k [failure-result (lambda ()
                                        (raise
                                         (make-exn:fail
                                          "Not found"
                                          (current-continuation-marks))))])
  (let ([path (find-subtree t k)])
    (cond
     ((not (tree-empty? (pos-head path)))
      (tree-value (pos-head path)))
     ((procedure? failure-result) (failure-result))
     (else failure-result))))

;; "Install" NEW-LEAF at the location indicated by STACK, and return
;; the root of the new tree.

;; tree? pos? -> tree?
(define (graft new-leaf stack)
  (for/fold ([result new-leaf])
      ([parent (pos-rest stack)]
       [child  stack])
      (case (which-child parent child)
        ((left)
         (make-tree
          (tree-key parent)
          (tree-value parent)
          #:left result
          #:right (tree-right parent)
          #:MaxNodeCount (max (tree-MaxNodeCount parent)
                              (tree-MaxNodeCount result))))
        ((right)
         (make-tree
          (tree-key parent)
          (tree-value parent)
          #:left  (tree-left parent)
          #:right result
          #:MaxNodeCount (max (tree-MaxNodeCount parent)
                              (tree-MaxNodeCount result)))))))

(define alpha 3/4)

(define (h-alpha weight)
  (floor (/ (log weight) (log (/ alpha)))))

(define (is-deep? pos)
  ((pos-depth pos) . > . (h-alpha (add1 (tree-weight (pos-last pos))))))

(define (alpha-weight-balanced t)
  (and
   ((tree-weight (tree-left  t)) . <= . (* alpha (tree-weight t)))
   ((tree-weight (tree-right t)) . <= . (* alpha (tree-weight t)))))

;; tree? key? any/c -> tree?
(define (tree-set t k v)

  ;; This whole function looks laughably inefficient.
  (define (maybe-post-insert-rebalance root)

    (define (find-scapegoat root pos)
      (define scapeulous (compose not alpha-weight-balanced))
      (let loop ([pos pos])
        (cond
         ((pos-empty? pos)
          (error 'scapeulous "This isn't supposed to happen"))
         ((scapeulous (pos-head pos))
          pos)
         (else
          (loop (pos-rest pos))))))

    ;; Calling find-subtree here, when we just computed the stack, is
    ;; wasteful; but it's O(log(N)) wasteful, and we're about to
    ;; rewrite a chunk of the tree in O(N) time, so ... it's probably
    ;; OK.
    (let ([new-entry-pos (find-subtree root k)])
      (if (is-deep? new-entry-pos)
          (let ([path-to-scapegoat (find-scapegoat root new-entry-pos)])
            (let ([sg (pos-head path-to-scapegoat)])
              (graft
                (balance sg)
                path-to-scapegoat)))
          root)))

  (let ([stack-o-trees (find-subtree t k)])
    (maybe-post-insert-rebalance
     (graft
      (make-tree k v #:MaxNodeCount
                 (max (add1 (tree-weight t))
                      (tree-MaxNodeCount t)))
      stack-o-trees))))

(define (in-order-successor t)
  (cond
   ((tree-iterate-first (tree-right t)) => pos-head)
   (else #f)))

;; This should be private to tree-remove, but it's at top level so
;; that the tests can get at it.
(define (decapitate target)
  (cond
   ((tree-empty? (tree-left target))
    (tree
     (tree-node-or-false (tree-right target))
     (tree-MaxNodeCount target)))
   ((tree-empty? (tree-right target))
    (tree
     (tree-node-or-false (tree-left target))
     (tree-MaxNodeCount (tree-left target))))
   (else
    ;; TODO -- flip a coin, and choose child randomly so as to not
    ;; get too unbalanced
    (let ([s (in-order-successor target)])
      (make-tree (tree-key s)
                 (tree-value s)
                 #:left  (tree-left target)
                 #:right (tree-right s)
                 #:MaxNodeCount (max (tree-MaxNodeCount target)
                                     (tree-MaxNodeCount s)))))))

(define (which-child parent child)
  (cond
   ((eq? child (tree-left  parent)) 'left )
   ((eq? child (tree-right parent)) 'right)
   (else
    (error 'which-child (format "~a is not the parent of ~a" parent child)))))

(define (tree-remove t k)

  (define (maybe-rebalance t)
    (if ((tree-weight t) . <= . (/ (tree-MaxNodeCount t) 2))
        (balance t)
        t))

  (let ([stack-o-trees (find-subtree t k)])
    (cond
     ((tree-empty? (pos-head stack-o-trees))
      t)
     (else
      (maybe-rebalance
       (graft (decapitate (pos-head stack-o-trees))
              stack-o-trees))))))

(define (tree-weight t)
  (cond
   ((tree-node-or-false t) => node-weight)
   (else 0)))


;; Code for rebuilding a new balanced tree

;; See
;; http://www.akira.ruc.dk/~keld/teaching/algoritmedesign_f07/Artikler/03/Galperin93.pdf,
;; section 6.1

(define (partition-nonempty-sorted-list a)
  (let* ([half (/ (length a) 2)]
         [small (take a (floor half))]
         [large-plus-median (list-tail a (floor half))]
         )
    (values small (car large-plus-median) (cdr large-plus-median))))

(define (alist->new-tree a #:MaxNodeCount [MaxNodeCount #f])
  (if (null? a)
      (public-make-tree)
      (let-values ([(small median large) (partition-nonempty-sorted-list a)])
        (make-tree (car median)
                   (cdr median)
                   #:left (alist->new-tree small)
                   #:right (alist->new-tree large)
                   #:MaxNodeCount (or MaxNodeCount (length a))))))

(define (balance t)
  (alist->new-tree (dict-map t cons)))



(define (public-make-tree) (tree #f 0))
(provide (rename-out [public-make-tree tree])
         tree?
         tree-weight)

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

(struct tree (node-or-false MaxNodeCount)
        #:property prop:dict prop-dict-vector
        ;; we never mutate input trees, but on occasion we will
        ;; construct a private tree, mutate it, and then return it
        #:mutable
        #:transparent)

(define (treenode->string t)
  (let ([n (tree-node-or-false t)])
    (if n
        (format
         "Node (~a, ~a) has weight ~a, MaxNodeCount ~a"
         (tree-key t)
         (tree-value t)
         (tree-weight t)
         (tree-MaxNodeCount t))
        (format "Empty node"))))

(struct node (key value left right weight)
        #:mutable
        #:transparent)

;; a POS is basically a list, with a pointer to the original tree, so
;; that we can raise exn:fail:contract when needed.
(struct pos (original-tree stack)
        #:transparent
        #:property prop:sequence (lambda (p) (pos-stack p)))
(define (make-pos original-tree) (pos original-tree '()))
(define (pos-push thing p)
  (pos (pos-original-tree p)
       (cons thing (pos-stack p))))
(define (pos-rest p)
  (pos (pos-original-tree p)
       (cdr (pos-stack p))))

(define pos-depth  (compose length pos-stack))
(define pos-head   (compose car    pos-stack))
(define pos-last   (compose last   pos-stack))
(define pos-empty? (compose null?  pos-stack))

(define (pos-validate t p)
  (when (not (eq? t (pos-original-tree p)))
    (raise (exn:fail:contract
            (format "Dude, ~a and ~a aren't related" t p)
            (current-continuation-marks)))))

;; Convenience wrappers
(define (tree-left  t) (node-left  (tree-node-or-false t)))
(define (tree-right t) (node-right (tree-node-or-false t)))
(define (tree-key   t) (node-key   (tree-node-or-false t)))
(define (tree-value t) (node-value (tree-node-or-false t)))
