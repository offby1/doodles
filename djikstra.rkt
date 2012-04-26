#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
PLTSTDERR=debug exec racket -l errortrace --require "$0" --main -- ${1+"$@"}
|#

;; http://en.wikipedia.org/wiki/Djikstra%27s_algorithm

#lang racket

;; graph node -> (listof node)
(define (find-shortest-path graph init)
  ;; assert INIT is a node in GRAPH

  ;; 1. Assign to every node a tentative distance value: set it to
  ;; zero for our initial node and to infinity for all other nodes.
  (for ([node graph])
    (node-set-distance!
     node
     (if (nodes=? node init)
         0
         +inf.0)))

  ;; 2. Mark all nodes unvisited.  Set the initial node as
  ;; current. Create a set of the unvisited nodes called the unvisited
  ;; set consisting of all the nodes except the initial node.
  (for ([node graph])
    (node-set-visited!
     node #f))

  (let loop ([current-node init])
    (let ([unvisited-set
           (set-subtract
            (set (graph-nodes graph))
            (set init))
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
      (for ([neighbor (filter (compose not node-visited?) (node-neighbors current))])
        (let ([distance (+ (node-distance current)
                           (edge-weight (node-edge-from current neighbor)))])
          (when (< distance (node-distance neighbor))
            (node-set-distance! neighbor distance))))

      ;; 4. When we are done considering all of the neighbors of the
      ;; current node, mark the current node as visited and remove it
      ;; from the unvisited set. A visited node will never be checked
      ;; again; its distance recorded now is final and minimal.
      (node-set-visited! current #t)
      (set! unvisited-set (set-remove unvisited-set current))


      ;; 5.  If the destination node has been marked visited (when
      ;; planning a route between two specific nodes) or if the smallest
      ;; tentative distance among the nodes in the unvisited set is
      ;; infinity (when planning a complete traversal), then stop. The
      ;; algorithm has finished.
      (let ([next (smallest-distance  unvisited)])
        (if (not (rational? (node-distance next)))
            graph
            (loop next))))))

;; simple test
(define g (make-graph-from-edges '((a b 10)
                                   (a c 5)
                                   (b c 3)
                                   (b d 12)
                                   (b e 10))))
(displayln (find-shortest-path g (get-named-node g 'a)))
