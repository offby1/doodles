#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec racket -l errortrace --require "$0" --main -- ${1+"$@"}
|#

;; http://en.wikipedia.org/wiki/Djikstra%27s_algorithm

#lang racket

(require unstable/debug)

(struct node (name distance visited neighbor-to-weight) #:transparent #:mutable)
(struct graph (nodes) #:transparent #:mutable)

(define (nearest node-seq)
  (for/fold ([node #f]
             [dist +inf.0])
      ([candidate node-seq])
      (if (< (node-distance candidate)
             dist)
          (values candidate (node-distance candidate))
          (values node dist))))

(define/contract (get-named-node graph name)
  (graph? symbol? . -> . node?)
  (first
   (filter (lambda (candidate)
             (equal? name (node-name candidate))) (debug (graph-nodes graph)))))

;; graph node -> (listof node)
(define (find-shortest-path graph init)
  ;; assert INIT is a node in GRAPH

  ;; 1. Assign to every node a tentative distance value: set it to
  ;; zero for our initial node and to infinity for all other nodes.
  (for ([node (graph-nodes graph)])
    (set-node-distance!
     node
     (if (eq? node init)
         0
         +inf.0)))

  ;; 2. Mark all nodes unvisited.  Set the initial node as
  ;; current. Create a set of the unvisited nodes called the unvisited
  ;; set consisting of all the nodes except the initial node.
  (for ([node (graph-nodes graph)])
    (set-node-visited!
     node #f))

  (let loop ([current init])
    (let ([unvisited-set
           (set-remove
            (apply set (graph-nodes graph))
            init)
           ])
      ;; 3. For the current node, consider all of its unvisited
      ;; neighbors and calculate their tentative distances. For example,
      ;; if the current node A is marked with a tentative distance of 6,
      ;; and the edge connecting it with a neighbor B has length 2, then
      ;; the distance to B (through A) will be 6+2=8. If this distance
      ;; is less than the previously recorded tentative distance of B,
      ;; then overwrite that distance. Even though a neighbor has been
      ;; examined, it is not marked as visited at this time, and it
      ;; remains in the unvisited set.
      (for ([(neighbor weight) (in-dict (node-neighbor-to-weight current))]
            #:when (not (node-visited neighbor)))
        (let ([distance (+ (node-distance current) weight)])
          (when (< distance (node-distance neighbor))
            (set-node-distance! neighbor distance))))

      ;; 4. When we are done considering all of the neighbors of the
      ;; current node, mark the current node as visited and remove it
      ;; from the unvisited set. A visited node will never be checked
      ;; again; its distance recorded now is final and minimal.
      (set-node-visited! current #t)
      (set! unvisited-set (set-remove unvisited-set current))


      ;; 5.  If the destination node has been marked visited (when
      ;; planning a route between two specific nodes) or if the smallest
      ;; tentative distance among the nodes in the unvisited set is
      ;; infinity (when planning a complete traversal), then stop. The
      ;; algorithm has finished.
      (let ([next (nearest unvisited-set)])
        (if (not (rational? (node-distance next)))
            graph
            (loop next))))))

;; simple test
(define g (graph
           (let ([nodes-by-name (make-hash '())])
             (for ([thing '((a b 10)
                            (a c 5)
                            (b c 3)
                            (b d 12)
                            (b e 10))])

               (let* ([name (first thing)]
                      [node (dict-ref nodes-by-name name
                                      (node (first thing)
                                            +inf.0
                                            #f
                                            (make-hash '())))])
                 (dict-set! (node-neighbor-to-weight node)
                            (second thing)
                            (third thing))
                 (dict-set! nodes-by-name name node)))
             (dict-map nodes-by-name (lambda (k v) v)))))
(displayln (find-shortest-path g (get-named-node g 'a)))
