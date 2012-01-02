#lang racket
(define key? number?)                   ; TODO -- make this, and our
                                        ; equality and ordering tests,
                                        ; parameterizable

(define (tree-count t)
  (sequence-length (in-dict-keys t)))

;; Our "pos" is a stack of tree nodes -- the nodes we need to pass
;; through to get to a particular node.

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
     ((null? pos) #f)
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

(define (make-tree k v [l #f] [r #f])
  (set! l (or l (public-make-tree)))
  (set! r (or r (public-make-tree)))
  (tree (node k v l r (add1
                       (max (cond ((tree-node-or-false l) => node-height) (else 0))
                            (cond ((tree-node-or-false r) => node-height) (else 0)))))))

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

;; Given the result of find-subtree, throw away the first element,
;; replace it with NEW-HEAD, and return a new STACK in which all
;; elements are ancestors of NEW-HEAD.
(define (stitch-up-stack new-head stack)
  (for/fold ([result new-head])
      ([parent (pos-rest stack)]
       [child  stack])
      (displayln (treenode->string child))
      (case (which-child parent child)
        ((left)
         (make-tree
          (tree-key parent)
          (tree-value parent)
          result
          (tree-right parent)))
        ((right)
         (make-tree
          (tree-key parent)
          (tree-value parent)
          (tree-left parent)
          result)))))

(define (tree-set t k v)
  (let ([stack-o-trees (find-subtree t k)])
    (stitch-up-stack (make-tree k v) stack-o-trees)))

;; This should be private to tree-remove, but it's at top level so
;; that the tests can get at it.
(define (decapitate target)

  (define (in-order-successor t)
    ((lambda (x) (tree? x)) . -> . (or/c false? (lambda (x) (tree? x))))
    (cond
     ((tree-iterate-first (tree-right t)) => pos-head)
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

(define (which-child parent child)
  (cond
   ((eq? child (tree-left  parent)) 'left )
   ((eq? child (tree-right parent)) 'right)
   (else
    (error 'which-child (format "~a is not the parent of ~a" parent child)))))

(define (tree-remove t k)
  (let ([stack-o-trees (find-subtree t k)])
    (cond
     ((tree-empty? (pos-head stack-o-trees))
      t)
     (else
      (stitch-up-stack (decapitate (pos-head stack-o-trees))
                       stack-o-trees)))))

(define (tree-height t)
  (cond
   ((tree-node-or-false t) => node-height)
   (else 0)))

(define (public-make-tree) (tree #f))
(provide (rename-out [public-make-tree tree])
         tree?
         tree-height)

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
        #:property prop:dict prop-dict-vector
        #:transparent)

(define (treenode->string t)
  (let ([n (tree-node-or-false t)])
    (if n
        (format
         "Node (~a, ~a) has height ~a, balance factor ~a"
         (tree-key t)
         (tree-value t)
         (tree-height t)
         (abs
          (- (tree-height (tree-left t))
             (tree-height (tree-right t)))))
        (format "Empty node"))))

(struct node (key value left right height) #:transparent)

(struct pos (original-tree stack)
        #:transparent
        #:property prop:sequence (lambda (p) (pos-stack p)))
(define (make-pos original-tree) (pos original-tree '()))
(define (pos-push thing p)
  (pos (pos-original-tree p)
       (cons thing (pos-stack p))))
(define pos-head (compose car pos-stack))
(define (pos-rest p)
  (pos (pos-original-tree p)
       (cdr (pos-stack p))))
(define pos-empty? (compose null? pos-stack))
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
