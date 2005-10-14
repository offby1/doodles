(module m3 mzscheme
  (require "dfs.ss")
  (require "draw.ss")
  (require (lib "trace.ss"))
  (require (only (lib "compat.ss") sort))
  (require (only (lib "1.ss" "srfi") iota zip filter append-map))
  
  (define visited-nodes (make-hash-table 'equal))
  (define x-coordinate car)
  (define y-coordinate cdr)
  (define (shuffle-list l)
    (map cdr
         (sort (lambda (a b) (< (car a) (car b)))
               (map (lambda (elt)
                      (cons (random) elt))
                    l))))
  (define *x-max* 100)
  (define *y-max* *x-max*)

  (define *the-grid* (make-grid (add1 *x-max*)))
  (define (get-direction from to)
    (let ((dx (- (car to)
                 (car from)))
          (dy (- (cdr to)
                 (cdr from))))
      (unless (= 1 (+ (abs dx)
                      (abs dy)))
        (error "'From' point " from " and 'to' point "to  " aren't adjacent"))
      ;; TODO -- check for negative values
      (cond
       ((zero? dx)
        (if (positive? dy)
            'down
          'up))
        ((positive? dx)
         'right)
        (else
         'left))))

  (define *goal-node*
    (cons *x-max* *y-max*))
  
  (define *solution* #f)

  (define (set-visited! n path-to-here) 
    (when (not (null? path-to-here))
      (let* ((previous-node (car path-to-here))
             (direction-travelled (get-direction previous-node
                                                n)))
        ;; knock down the wall.
        (case direction-travelled
          ((right)
           (draw-line *the-grid*
                       (add1 (car previous-node))
                       (cdr previous-node)
                       'down
                       1 #f 'red))
          ((down) (draw-line *the-grid*
                              (car previous-node)
                              (add1 (cdr previous-node))
                              'right
                              1 #f 'red))
          ((left)
           (draw-line *the-grid*
                       (car previous-node)
                       (cdr previous-node)
                       'down
                       1 #f 'red))
          ((up)
           (draw-line *the-grid*
                       (car previous-node)
                       (cdr previous-node)
                       'right
                       1 #f 'red))
          (else
           (error "Uh oh." direction-travelled)))

        ;; now draw a line from the old position to the current position.
        (parameterize ((*offset* 1/2))
                      (draw-line *the-grid*
                                 (car previous-node)
                                 (cdr previous-node)
                                 direction-travelled
                                 1 #t 'black))))

    (hash-table-put! visited-nodes n #t)

    (when (equal? *goal-node* n)
      (set! *solution* (cons *goal-node* path-to-here))))
  
  (define (visited? n) (hash-table-get visited-nodes n (lambda () #f)))
  (define (enumerate-neighbors node)
    (shuffle-list
     (filter (lambda (candidate)
               (and (<= 0 (x-coordinate candidate) *x-max*)
                    (<= 0 (y-coordinate candidate) *y-max*)
                    (= 1 (+ (abs (- (x-coordinate candidate)
                                    (x-coordinate node)))
                            (abs (- (y-coordinate candidate)
                                    (y-coordinate node)))))))
             (map (lambda (offset)
                    (cons (+ (car offset)
                             (car node))
                          (+ (cdr offset)
                             (cdr node))))
                 
                  (append-map 
                   (lambda (n) 
                     (map (lambda (m)
                            (cons n m))
                          (iota 3 -1)))
                   (iota 3 -1))))))
                                        ;(trace enumerate-neighbors)

  (random-seed 0)
  
  (generic-dfs '(0 . 0)
               enumerate-neighbors
               '()
               (lambda (n) #f)
               set-visited!
               visited?)

  ;; draw the solution.
  (parameterize ((*offset* 1/2))
  (let loop ((trail *solution*))
    (unless (or (null? trail)
                (null? (cdr trail)))
      (let ((prev (car trail))
            (next (cadr trail)))
        (draw-line *the-grid*
                   (car prev)
                   (cdr prev)
                   (get-direction prev next)
                   1
                   #t 'gray))
      (loop (cdr trail)))))
  )
