(in-package :wc)

(defstruct agenda-item
  trail
  word)

(defun bfs (start-node goal-node nodes-equal-p node-neighbors)
  (let ((already-seen (make-hash-table :test 'equalp))
        (the-queue (make-queue)))
    (queue-add the-queue (make-agenda-item :trail '()
                                           :word start-node))
    (loop
       (when (queue-empty-p the-queue)
         (return nil))
       (let* ((this-item (queue-front the-queue))
              (w (agenda-item-word this-item))
              (trail (agenda-item-trail this-item)))
         (when (funcall nodes-equal-p goal-node w)
           (return trail))
         (mapc #'(lambda (n)
                   (setf (gethash n already-seen) t)
                   (queue-add the-queue (make-agenda-item
                                         :trail (cons w trail)
                                         :word n)))
               (remove #'(lambda (word)
                           (gethash word already-seen))
                       (funcall node-neighbors w)))
         (queue-pop the-queue)))))

;; a little test
(progn
  (defun node-neighbors (n)
    (let ((probe (assoc n '(("start"   "a" "b")
                            ("a"       "start" "c")
                            ("b"       "start" "c" "e")
                            ("c"       "a" "b" "e")
                            ("e"       "b" "c" "d" "f")
                            ("d"       "e")
                            ("f"       "e" "goal")
                            ("goal"    "f")
                            ("outlier")
                            ("cycle-a" "cycle-b")
                            ("cycle-b" "cycle-a")
                            )
                        :test 'equalp)))
      (or
       (and probe (cdr probe))
       (error "Unknown node ~s" n))))
  (trace node-neighbors)
  (trace bfs)
  (bfs "start" "goal" 'equalp 'node-neighbors))
