#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module m2 mzscheme
  (require (only (lib "1.ss" "srfi") filter alist-cons find))
  (require (only (lib "setf.ss" "swindle") rotate! inc!))

  ;(print-struct #t)
  
  (define-struct cell (visited? neighbors name) #f)

  (define (cell->string c)
    (format "~s ~a"
            (cell-name c)
            (map (lambda (n)
                   (format "~a: ~a" (car n)
                           (cell-name (cdr n)))) (cell-neighbors c))))
  
  (define fresh-cell
    (let ((serial-number 0))
      (lambda ()
        (begin0
          (make-cell #f '() serial-number)
          (inc! serial-number)))))

  (define (next-direction dir)
    (case dir
      ((north) 'east)
      ((east)  'south)
      ((south) 'west)
      ((west)  'north)
      (else (error "I don't know the direction after ~s" dir))))

  (define (opposite-direction dir)
    (next-direction (next-direction dir)))

  (define directions
    (let l ((d 'north)
            (result '()))
      (let ((n (next-direction d)))
        (if (eq? 'north n)
            (cons n result)
          (l n (cons n result))))))
  
  (define (neighbors-to-the cell dir)
    (assq dir (cell-neighbors cell)))
  (define (link-cells-in-direction! a b dir)
    (let ((o (opposite-direction dir)))
      (when (neighbors-to-the a dir)
        (error "Cell ~s already has a neighbor to the ~s" (cell->string a) dir))
      (when (neighbors-to-the b o)
        (error "Cell ~s already has a neighbor to the ~s" (cell->string b) o))
      (set-cell-neighbors! a (alist-cons dir  b (cell-neighbors a)))
      (set-cell-neighbors! b (alist-cons o a    (cell-neighbors b)))))
  (define (make-grid num-cells)
    (define (helper root-cell num-cells)
      (define (first-vacant-direction)
        (find (lambda (d) (not  (neighbors-to-the root-cell d)))
              directions))

      (printf "helper: ~s ~s~%" (cell->string root-cell) num-cells)
      
      (if (positive? num-cells)
        
          (let ((directions-with-neighbors (map car (cell-neighbors root-cell))))
            (let ((new-direction
                   (case (length directions-with-neighbors)
                     ((0) 'north)
                     ((1 2 3) (first-vacant-direction))
                     (else
                      (error "helper: can't link a cell ~s that already has 4 neighbors" (cell->string root-cell))))))
              (let ((new-cell (fresh-cell)))
              
                (link-cells-in-direction! root-cell new-cell new-direction)
             
                (helper new-cell (- num-cells 1)))))
        root-cell))

    (helper (fresh-cell) num-cells))
  (define (wander grid)
    
    (define (wander-from location)
      (define (already-visited?)
        (cell-visited? location))
      (define (uvs)
        (filter (lambda (c)
                  (not (cell-visited? c)))
                (filter (lambda (x) (not (null? x)))
                             (map cdr (cell-neighbors location)))))
      
      (printf "Wandering from ~s~%" (cell->string location))
      (when (already-visited?)
        (error "Can't wander from ~s, since it's already been visited" (cell->string location)))
      (set-cell-visited?! location #t)
      (let ((unvisited-neighbors (uvs)))
        (if (not (null? unvisited-neighbors))

            ;; TODO -- choose randomly, not first
            (wander-from (car unvisited-neighbors))
                                                   
          'done)))

    (wander-from grid))

  (wander (make-grid 10))
  )