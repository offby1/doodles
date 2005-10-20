#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec mred -M errortrace -qu "$0" ${1+"$@"}
|#

(module maze mzscheme
  (require (planet "dfs.ss" ("offby1" "my-plt-collects.plt")))
  (require (all-except "draw.ss" my-version))
  (require (lib "trace.ss"))
  (require (lib "cmdline.ss"))
  (require (only (lib "compat.ss") sort))
  (require (only (lib "1.ss" "srfi") iota filter append-map))
  
  (provide my-version)
  
  (define my-version "$Id$")

  (define visited-nodes #f)
  (define x-coordinate car)
  (define y-coordinate cdr)

  (define (shuffle-list l)
    (map cdr
         (sort (lambda (a b) (< (car a) (car b)))
               (map (lambda (elt)
                      
                      ;; this oddness causes the list to be shuffled
                      ;; only a little, making for a windier maze, and
                      ;; hence a longer solution.  Ideally we'd use a
                      ;; stable sort, but I don't know of one that's
                      ;; easily available to mzscheme.
                      (cons (let ((r (random)))
                              (if (< r 10/10)
                                  r
                                1)) elt)
                                        
                      )
                    l))))
  
  (define *lines-while-generating* (make-parameter #f))
  (define *solution* (make-parameter #f))
  
  (define (get-direction from to)
    (let ((dx (- (x-coordinate to)
                 (x-coordinate from)))
          (dy (- (y-coordinate to)
                 (y-coordinate from))))
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

  (define (set-visited! n path-to-here) 
    (when (not (null? path-to-here))
      (let* ((previous-node (car path-to-here))
             (direction-travelled (get-direction previous-node
                                                n)))
        ;; Mr. Gorbachev, knock down the wall.
        (case direction-travelled
          ((right)
           (draw-line *the-grid*
                       (add1 (x-coordinate previous-node))
                       (y-coordinate previous-node)
                       'down
                       1 #f 'white))
          ((down) (draw-line *the-grid*
                              (x-coordinate previous-node)
                              (add1 (y-coordinate previous-node))
                              'right
                              1 #f 'white))
          ((left)
           (draw-line *the-grid*
                       (x-coordinate previous-node)
                       (y-coordinate previous-node)
                       'down
                       1 #f 'white))
          ((up)
           (draw-line *the-grid*
                       (x-coordinate previous-node)
                       (y-coordinate previous-node)
                       'right
                       1 #f 'white))
          (else
           (error "Uh oh." direction-travelled)))
        
        ;; now draw a line from the old position to the current position.
        (when (*lines-while-generating*)
          (parameterize ((*offset* 1/2))
                        (draw-line *the-grid*
                                   (x-coordinate previous-node)
                                   (y-coordinate previous-node)
                                   direction-travelled
                                   1 #t 'black)))
                                              
                                              ))

    (hash-table-put! visited-nodes n #t)
    
    (when (and (= (*x-max*) (x-coordinate n) (y-coordinate n)))
      (*solution* (reverse (cons (cons (*x-max*)
                                       (*x-max*)) path-to-here)))))
  
  (define (visited? n) (hash-table-get visited-nodes n (lambda () #f)))
  (define (enumerate-neighbors node)
    (shuffle-list 
     (filter (lambda (candidate)
               (and (<= 0 (x-coordinate candidate) (*x-max*))
                    (<= 0 (y-coordinate candidate) (*x-max*))
                    (= 1 (+ (abs (- (x-coordinate candidate)
                                    (x-coordinate node)))
                            (abs (- (y-coordinate candidate)
                                    (y-coordinate node)))))))
             (map (lambda (offset)
                    (cons (+ (x-coordinate offset)
                             (x-coordinate node))
                          (+ (y-coordinate offset)
                             (y-coordinate node))))
                 
                  (append-map 
                   (lambda (n) 
                     (map (lambda (m)
                            (cons n m))
                          (iota 3 -1)))
                   (iota 3 -1))))))
                                        ;(trace enumerate-neighbors)

  ;(random-seed 0)
  
  (define (main)
    (*solution* #f)
    (set! visited-nodes (make-hash-table 'equal))
    (generic-dfs '(0 . 0)
                 enumerate-neighbors
                 '()
                 (lambda (n) #f)
                 set-visited!
                 visited?)

    ;; draw the solution.
    (parameterize ((*offset* 1/2))
                  (let loop ((trail (*solution*)))
                    (unless (or (null? trail)
                                (null? (cdr trail)))
                      (let ((prev (car trail))
                            (next (cadr trail)))
                        (draw-line *the-grid*
                                   (x-coordinate prev)
                                   (y-coordinate prev)
                                   (get-direction prev next)
                                   1
                                   #t 'gray))
                      (loop (cdr trail))))))
  (command-line
   "maze"
   (current-command-line-arguments)
   (once-each
    [ ;; one flag to toggle the drawing of the black lines while generating the maze.
     ("-l" "--lines-while-generating") "Draw black lines while generating the maze"
     (*lines-while-generating* #t)]
    ;; one integer to determine *x-max*.
    [("-s" "--size-of-side-in-cells") size "Maze will be this many cells wide & tall"
     (*x-max* (sub1 (string->number size)))]
    ;; one integer to determine the number of milliseconds to pause.
    [("-p" "--pause-in-milliseconds") pause "Pause this many milliseconds before drawing each line"
     (*pause* (/ (string->number pause) 1000))]
    )
   )
  (define *the-grid* (make-grid main))
  )
