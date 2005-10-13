#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

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
  (define *x-max* 1)
  (define *y-max* *x-max*)
  
  (define (goal-node? n)
    #f
                                        ;(and (pair? n) (= (car n ) *x-max*) (= (cdr n)  *y-max*))
    )
  (define *the-grid* (make-grid (add1 *x-max*)))
  (define (get-direction from to)
    (let ((dx (- (car to)
                 (car from)))
          (dy (- (cdr to)
                 (cdr from))))
      (unless (= 1 (+ (abs dx)
                      (abs dy)))
        (error "'From' point ~s and 'to' point ~s aren't adjacent" from to))
      ;; TODO -- check for negative values
      (if (zero? dx) 'down 'right)))

  (define (set-visited! n previous-node) 
    (hash-table-put! visited-nodes n #t)
    (when previous-node

      (let ((direction-travelled (get-direction previous-node
                                                n)))
        ;; knock down the wall.
        (if (eq? direction-travelled 'right)
            (erase-line *the-grid*
                        (add1 (car previous-node))
                        (cdr previous-node)
                        'down
                        1)
          (erase-line *the-grid*
                      (car previous-node)
                      (add1 (cdr previous-node))
                      'right
                      1))

        ;; now draw a line from the old position to the current position.
        (parameterize ((*offset* 1/2))
                      (draw-line *the-grid*
                                 (car previous-node)
                                 (cdr previous-node)
                                 direction-travelled
                                 1))

        )))
  (define (visited? n) (hash-table-get visited-nodes n (lambda () #f)))
  (define (enumerate-neighbors node)
    (printf "enumerate-neighbors: node ~s~%" node)
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
  (generic-dfs '(0 . 0)
               enumerate-neighbors
               '()
               goal-node?
               set-visited!
               visited?)
  )
