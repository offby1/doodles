#lang racket
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

(define (tree-iterate-key t pos)
  ;; BUGBUG -- raise exn:fail:contract if pos isn't valid for t
  (tree-key (car pos)))

(define (tree-iterate-value t pos)
  ;; BUGBUG -- raise exn:fail:contract if pos isn't valid for t
  (tree-value (car pos)))

(define (make-tree k v [l #f] [r #f])
  (set! l (or l (public-make-tree)))
  (set! r (or r (public-make-tree)))
  (tree (node k v l r (add1
                       (max (cond ((tree-node-or-false l) => node-depth) (else 0))
                            (cond ((tree-node-or-false r) => node-depth) (else 0)))))))

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

;; This should be private to tree-remove, but it's at top level so
;; that the tests can get at it.
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

(define (tree-remove t k)
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

(define (tree-depth t)
  (cond
   ((tree-node-or-false t) => node-depth)
   (else 0)))

(define (public-make-tree) (tree #f))
(provide (rename-out [public-make-tree tree])
         tree?
         tree-depth)

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

(struct node (key value left right depth) #:transparent)

;; Convenience wrappers
(define (tree-left  t) (node-left  (tree-node-or-false t)))
(define (tree-right t) (node-right (tree-node-or-false t)))
(define (tree-key   t) (node-key   (tree-node-or-false t)))
(define (tree-value t) (node-value (tree-node-or-false t)))
