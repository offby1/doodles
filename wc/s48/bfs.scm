(define (ep . args)
  (apply display args (list (current-error-port)))
  (newline (current-error-port)))

(define (remove proc seq)
  (filter (lambda (x)
            (not (proc x)))
          seq))

(define-record-type :agenda-item
  (make-agenda-item trail word)
  agenda-item?
  (trail agenda-item-trail set-agenda-item-trail!)
  (word  agenda-item-word  set-agenda-item-word!))

(define (bfs start-node goal-node nodes-equal? node-neighbors)

  (define *already-seen* (make-set))
  
  (define (already-seen? thing)
    (is-present? thing *already-seen*))
  
  (define (note! thing)
    (add! thing *already-seen*))

  (define (enqueue! thing)
    (insert-queue! *the-queue* thing))
  
  (define *the-queue* (make-queue (list (make-agenda-item '() start-node))))

  (define (front)
    (front-queue *the-queue*))

  (define (pop-queue!)
    (let ((rv (front)))
      (delete-queue! *the-queue*)
      rv))
  
  (define (loop)
    (if (empty-queue? *the-queue*) #f
      (let ((w     (agenda-item-word  (front)))
            (trail (agenda-item-trail (front))))

        (cond
         ((nodes-equal? goal-node w) trail)
         (else
          (for-each (lambda (n)
                      (note! n)
                      (enqueue! (make-agenda-item (cons w trail) n)))
                    (remove already-seen? (node-neighbors w)))
          (pop-queue!)
          (loop))))))

  (let ((rv (loop)))
    (and rv (reverse (cons goal-node rv)))))

;;; A fake network, for testing.

(define nodes-equal? string=?)
(define (node-neighbors n)
  (cond
   ((assoc n '(("start"   "a" "b")
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
               ))
    => cdr)
   (else
    (error "Unknown node" n))))

(ep "This should succeed: " (bfs "start" "goal" nodes-equal? node-neighbors))
(ep "This too: "            (bfs "goal" "start" nodes-equal? node-neighbors))
(ep "This should fail: "    (bfs "outlier" "goal" nodes-equal? node-neighbors))
(ep "This too: "            (bfs "cycle-a" "goal" nodes-equal? node-neighbors))
