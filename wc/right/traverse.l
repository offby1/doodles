(defmacro while (test &rest body)
  `(do ()
       ((not ,test))
     ,@body))

;; Finds shortest paths from START to all nodes, and then returns the
;; longest.

(defun bfs-traversal (start)

  ;; Initialize nodes to indicate that they haven't been visited
  (map-cihash #'(lambda (key value)
                  (declare (ignore key))
                  (setf (node-color value) :white)
                  (setf (node-path-to-here value) nil))
              *the-network*)

  (setf (node-color             (get-cihash start *the-network*)) :gray)
  (setf (node-path-to-here      (get-cihash start *the-network*)) (list start))

  (let ((q (make-queue))                ;queue of words
        )
    
    (enqueue start q)

    (while (not (queue-empty? q))
      (let* ((qf-string (dequeue q))    ;"queue front"
             (qf-node (get-cihash qf-string *the-network*))
             (neighbors (remove-if-not
                         #'(lambda (w)
                             (multiple-value-bind (value in)
                                 (get-cihash w *the-network*)
                               (and in (eql :white (node-color value)))))
                         (node-neighbors qf-node))))

        (mapcar #'(lambda (neighbor-word)
                      
                    (let ((neighbor-node (get-cihash neighbor-word *the-network*)))

                      (setf (node-path-to-here neighbor-node) (cons neighbor-word (node-path-to-here qf-node)))
                      (setf (node-color neighbor-node) :gray)
                      (enqueue neighbor-word q)))

                neighbors)

        (if (queue-empty? q)
            (return (node-path-to-here qf-node)))))))

#+cmu
(profile:profile bfs-traversal)

;(format t "~A~&" (bfs-traversal "giant"))
