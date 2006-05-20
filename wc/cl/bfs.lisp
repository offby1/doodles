(in-package :wc)

(defstruct agenda-item
  trail
  word)

(defun bfs (start-node goal-node nodes-equal-p node-neighbors)
  (let ((words-examined 0)
        (already-seen (make-hash-table :test 'equalp))
        (agenda (make-queue)))
    (queue-add agenda (make-agenda-item :trail '()
                                        :word start-node))
    (loop
     (when (queue-empty-p agenda)
       (return (values nil words-examined)))
     (let* ((this-item (queue-front agenda))
            (w (agenda-item-word this-item))
            (trail (agenda-item-trail this-item)))
       (incf words-examined)
       (when (funcall nodes-equal-p goal-node w)
         (return (values trail words-examined)))
       (mapc #'(lambda (n)
                 (setf (gethash n already-seen) t)
                 (queue-add agenda (make-agenda-item
                                    :trail (cons w trail)
                                    :word n)))
             (remove-if #'(lambda (word)
                            (gethash word already-seen))
                        (funcall node-neighbors w)))
       (queue-pop agenda)))))

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
  (bfs "start" "goal" 'equalp 'node-neighbors))
