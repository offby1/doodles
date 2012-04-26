#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec racket -l errortrace --require "$0" --main -- ${1+"$@"}
|#

;; http://en.wikipedia.org/wiki/Djikstra%27s_algorithm

#lang racket

(struct graph (nodes-by-name) #:transparent)
(struct node  (name edgeset) #:transparent)
(struct edge  (weight dest-node) #:transparent)

(define (make-graph-from-edge-list things)
  (graph
   (for/fold ([nodes-by-name (make-immutable-hash)])
       ([thing things])
       (let ([source-node-name (first thing)]
             [dest-node-name   (second thing)]
             [edge-weight      (third thing)])
         (let* ([dest-node   (dict-ref nodes-by-name dest-node-name (node dest-node-name (set)))]
                [e           (edge edge-weight dest-node)]
                [source-node (dict-ref nodes-by-name source-node-name (node source-node-name (set e)))])
           (dict-set
            (dict-set nodes-by-name source-node-name (node (node-name source-node)
                                                           (set-add (node-edgeset source-node) e)))
            dest-node-name dest-node))))))

(provide main)
(define (main . args)
  (define g (make-graph-from-edge-list
             '((a b 10)
               (a c 5)
               (e f 1)
               (b c 3)
               (b d 12)
               (b e 10))))
  (for ([(name node) (in-dict (graph-nodes-by-name g))])
    (for ([e (node-edgeset node)])
      (printf  "~a: ~a ~a~%" name (node-name (edge-dest-node e)) (edge-weight e)))))

  ;; 1. Assign to every node a tentative distance value: set it to
  ;; zero for our initial node and to infinity for all other nodes.

  ;; 2. Mark all nodes unvisited.  Set the initial node as
  ;; current. Create a set of the unvisited nodes called the unvisited
  ;; set consisting of all the nodes except the initial node.
      ;; 3. For the current node, consider all of its unvisited
      ;; neighbors and calculate their tentative distances. For example,
      ;; if the current node A is marked with a tentative distance of 6,
      ;; and the edge connecting it with a neighbor B has length 2, then
      ;; the distance to B (through A) will be 6+2=8. If this distance
      ;; is less than the previously recorded tentative distance of B,
      ;; then overwrite that distance. Even though a neighbor has been
      ;; examined, it is not marked as visited at this time, and it
      ;; remains in the unvisited set.
      ;; 4. When we are done considering all of the neighbors of the
      ;; current node, mark the current node as visited and remove it
      ;; from the unvisited set. A visited node will never be checked
      ;; again; its distance recorded now is final and minimal.
      ;; 5.  If the destination node has been marked visited (when
      ;; planning a route between two specific nodes) or if the smallest
      ;; tentative distance among the nodes in the unvisited set is
      ;; infinity (when planning a complete traversal), then stop. The
      ;; algorithm has finished.
;; simple test
