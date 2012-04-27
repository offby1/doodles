#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec racket -l errortrace --require "$0" --main -- ${1+"$@"}
|#

;; http://en.wikipedia.org/wiki/Djikstra%27s_algorithm

#lang racket
(require unstable/debug)

(struct graph (nodes-by-name) #:transparent)
(struct node  (name edgeset) #:transparent
        #:property prop:custom-write
        (lambda (thing op write?)
          (fprintf op "<node ~a ~a>"
                   (node-name thing)
                   (set-map (node-edgeset thing) (lambda (e)
                                                   (cons (edge-dest-node-name e)
                                                         (edge-weight e)))))))

(define (graph-ref g name [default #f]) (dict-ref (graph-nodes-by-name g) name default))

(struct edge  (dest-node-name weight) #:transparent)

(define (make-graph-from-edge-list things)
  (graph
   (let ([nodes-by-name (make-hash)])
     (for ([thing things])
       (let ([source-node-name (first thing)]
             [dest-node-name   (second thing)]
             [edge-weight      (third thing)])
         (dict-update! nodes-by-name
                       dest-node-name
                       values
                       (node dest-node-name (set)))
         ;; create entries for source and dest nodes in
         ;; nodes-by-name

         ;; modify source node by adding edge to dest node
         (dict-update! nodes-by-name
                       source-node-name
                       (lambda (n)
                         (node (node-name n)
                               (set-add (node-edgeset n)
                                        (edge dest-node-name edge-weight))))
                       (node
                        source-node-name
                        (set))

                       )
         ))
     nodes-by-name)))

(define (nearest-node-name n)
  (define (mmin key-fn first . rest)
    (for/fold ([rv first])
        ([i rest])
        (if (< (key-fn i)
               (key-fn rv))
            i
            rv)))
  (edge-dest-node-name
   (apply mmin edge-weight (set->list (node-edgeset n)))))

(define (traverse-from g init)
  (when (not (graph-ref g (node-name init) #f))
    (error 'traverse-from "No node named ~s in graph" (node-name init)))
  (define distances-by-node (make-hash))

  ;; 1. Assign to every node a tentative distance value: set it to
  ;; zero for our initial node and to infinity for all other nodes.
  (hash-set! distances-by-node init 0)
  (for ([n (in-dict-values (graph-nodes-by-name g))])
    (when (not (eq? n init))
      (hash-set! distances-by-node n +inf.0)))

  ;; 2. Mark all nodes unvisited.  Set the initial node as
  ;; current. Create a set of the unvisited nodes called the unvisited
  ;; set consisting of all the nodes except the initial node.
  (define visited-by-node (make-hash))
  (define unvisited-set (set))
  (for ([n (in-dict-values (graph-nodes-by-name g))])
    (when (not (eq? n init))
      (set-add unvisited-set n))
    (hash-set! visited-by-node n #f))
  (let loop ([current init])

    ;; 3. For the current node, consider all of its unvisited
    ;; neighbors and calculate their tentative distances. For example,
    ;; if the current node A is marked with a tentative distance of 6,
    ;; and the edge connecting it with a neighbor B has length 2, then
    ;; the distance to B (through A) will be 6+2=8. If this distance
    ;; is less than the previously recorded tentative distance of B,
    ;; then overwrite that distance. Even though a neighbor has been
    ;; examined, it is not marked as visited at this time, and it
    ;; remains in the unvisited set.
    (for ([e (node-edgeset current)])
      (let ([A current]
            [B (graph-ref g (edge-dest-node-name e))])
        (let ([tentative-distance (+ (edge-weight e)
                                     (dict-ref distances-by-node A))])
          (when (< tentative-distance (dict-ref distances-by-node B))
            (dict-set! distances-by-node B tentative-distance)))))

    ;; 4. When we are done considering all of the neighbors of the
    ;; current node, mark the current node as visited and remove it
    ;; from the unvisited set. A visited node will never be checked
    ;; again; its distance recorded now is final and minimal.
    (dict-set! visited-by-node current #t)
    (set! unvisited-set (set-remove unvisited-set current))

    ;; 5.  If the destination node has been marked visited (when
    ;; planning a route between two specific nodes) or if the smallest
    ;; tentative distance among the nodes in the unvisited set is
    ;; infinity (when planning a complete traversal), then stop. The
    ;; algorithm has finished.
    (if  (set-empty? (node-edgeset current))
         distances-by-node
         (let ([nearest  (graph-ref g (nearest-node-name current))])
           (if (not (rational? (dict-ref distances-by-node nearest)))
               distances-by-node
               ;; 6.  Set the unvisited node marked with the smallest
               ;; tentative distance as the next "current node" and go back
               ;; to step 3.
               (loop nearest))))))

(provide main)
(define (main . args)
  (define g (make-graph-from-edge-list
             '((a b 5)
               (a c 10)
               (b c 3)
               (a e 1)
               (e c 5)
               )))
  (for ([(name node) (in-dict (graph-nodes-by-name g))])
    (for ([e (node-edgeset node)])
      (printf  "~a: ~a ~a~%" name (edge-dest-node-name e) (edge-weight e))))
  (let ([init (graph-ref g 'a)])
    (printf "Starting at ~a...~%" init)
    (pretty-print (traverse-from g init)))
  )
