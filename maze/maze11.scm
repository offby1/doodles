;; Generates a maze, then displays it using Ghostscript.

(require 'array)
(require 'filter)
(require 'random)

;;;;;;;;;;;;;;;;;;;; point ;;;;;;;;;;;;;;;;;;;;
(define make-point cons)
(define point-x car)
(define point-y cdr)
(define (translated-point p dx dy)
  (make-point (+ dx (point-x p))
              (+ dy (point-y p))))
;;;;;;;;;;;;;;;;;;;; point ;;;;;;;;;;;;;;;;;;;;

(define (uneven-random upper)
  (define (squish x)
    (expt x 1))
  (floor (* upper (squish (/ (random most-positive-fixnum)
                             most-positive-fixnum)))))

(define (make-maze width-in-cells)

  (define (direction-indicator source dest)
    (case (- (point-x dest)
             (point-x source))
      ((0)
       (case (- (point-y dest)
                (point-y source))
         ((1) 'north)
         ((-1) 'south)
         (else (error "Uh oh."))))
      ((-1) 'west)
      ((1) 'east)
      (else (error "Uh oh."))))

  ;;;;;;;;;;;;;;;;;;;; grid ;;;;;;;;;;;;;;;;;;;;
  
  ;; Each entry will be a list of directions from which we've left
  ;; this cell and moved onto a virgin cell.  Naturally the initial
  ;; value is the empty list.  The direction from which we most
  ;; recently left is the car of the list.

  (define grid (make-array '()
                           width-in-cells 

                           ;; It's rectangular to match the aspect
                           ;; ratio of a printed page oritented in
                           ;; "portrait mode".
                           (round (* 5/4 width-in-cells))))


  ;; Prepends VALUE to the list located at point P.
  (define (grid-mark-as-visited! p value)
    (let ((old-value (array-ref  grid    (point-x p) (point-y p))))
      (array-set! grid (cons value old-value) (point-x p) (point-y p))))

  ;; Returns TRUE if and only if P is the upper-right-most cell.  
  (define grid-is-exit?
    (let ((the-exit (make-point (random (car (array-dimensions grid)))
                                (random (cadr (array-dimensions grid))))))
      (lambda (p)
        (equal? p the-exit))))

  ;; Returns TRUE if and only if we've called `grid-mark-as-visited!'
  ;; on this cell.
  (define (grid-visited? p)
    (not (null? (array-ref  grid    (point-x p) (point-y p)))))

  ;; If the cell referred to by P has been visited at all, returns the
  ;; newest value.  Otherwise returns #f.
  (define (most-recently-explored-direction p)
    (let ((datum (array-ref  grid    (point-x p) (point-y p))))
      (if (pair? datum)
          (car datum)
        #f)))

  ;; Returns a list of points which are adjacent to POINT.  Note: some
  ;; of those points might lie outside the grid.
  (define (all-neighbors point)
    (let* ((px (point-x point))
           (py (point-y point)))
      (list
       (make-point (- px 1) py)         ;west
       (make-point px (+ py 1))         ;north
       (make-point (+ px 1) py)         ;east
       (make-point px (- py 1))         ;south
       )))
  
  ;; Returns a list of those neighbors that have not been visited.
  ;; The list might very well be empty.
  (define (unvisited-neighbors point)
    (filter (lambda (p)
              (and
               (array-in-bounds? grid (point-x p) (point-y p))
               (not (grid-visited? p))))
            (all-neighbors point)))

  ;; Returns the neighbor which we visted before getting to POINT for
  ;; the first time.  Useful for when we're backing up.
  (define (neighbor-that-brought-me-to point)
    (let ((result (filter (lambda (p)
                            (and
                             (array-in-bounds? grid (point-x p)
                                               (point-y p))
                             (eq? (direction-indicator p point)
                                  (most-recently-explored-direction p))))
                          (all-neighbors point))))
      (if (and (pair? result)
               (not (= 1 (length result))))
          (error "Damn."))
      (if (pair? result)
          (car result)
        #f)))

  ;; Returns a string that contains PostScript code to render grid on
  ;; one page, and the SOLUTION (which is just a list of points) on
  ;; another.
  (define (grid-render-in-postscript)
    (let* ((array-width (car (array-dimensions grid)))
           (array-height (cadr (array-dimensions grid)))
           (points-per-cell (/ (* 8 72) array-width)))
      (define (flatten-strings thing)
        (cond
         ((string? thing)
          thing)
         ((null? thing)
          "")
         ((list? thing)
          (string-append
           (flatten-strings (car thing))
           (flatten-strings (cdr thing))))))

      (define (with-isolated-context . stuff)
        (string-append
         "gsave "
         (flatten-strings stuff)
         " grestore\n"))

      (define (with-line-width w . stuff)
        (with-isolated-context
         (number->string (exact->inexact w))
         " setlinewidth\n"
         stuff))
      
      (define (with-origin p . stuff)
        (with-isolated-context
         (number->string (exact->inexact (point-x p)))
         " "
         (number->string (exact->inexact (point-y p)))
         " translate\n"
         stuff))

      (define (with-gray-level g . stuff)
        (with-isolated-context
         (number->string (exact->inexact g))
         " setgray\n"
         stuff))

      (define (with-rotation angle . stuff)
        (with-isolated-context
         (number->string (exact->inexact angle))
         " rotate\n"
         stuff))

      (define (move-to x y)
        (string-append
         (number->string (exact->inexact x))
         " "
         (number->string (exact->inexact y))
         " moveto\n"))
      (define (draw-to x y)
        (string-append
         (number->string (exact->inexact x))
         " "
         (number->string (exact->inexact y))
         " lineto\n"))
      (define (general-line x0 y0 x1 y1)
        (string-append
         (move-to x0 y0)
         (draw-to x1 y1)))
      
      (define (arrow x y direction)
        (define length .5)
        (let ((angle (case direction
                       ((north) 90)
                       ((south) -90)
                       ((east)  0)
                       ((west)  180))))
           
          (with-origin
           (make-point x y)
           (with-rotation
            angle
            (with-origin (make-point (- (/ length 2)) 0)
                         (move-to 0 0)
                         (draw-to length 0)
                         (draw-to (/ length 2)
                                  (/ length 2))
                         (move-to length 0)
                         (draw-to (/ length 2)
                                  (- (/ length 2)))
                         "stroke ")))))
      
      (define (vertical-line x y length)
        (general-line x y x (+ y length)))
           
      (define (horizontal-line x y length)
        (general-line x y (+ x length) y))

      (define (text string)
        ;; BUGBUG -- I probably have to somehow quote parentheses in
        ;; the string.
        (string-append
         "0 0 moveto "
         "(" string ") show\n"))
      
      (define document-prelude
        "%!PS-Adobe-3.0\n%%Title: Maze\n%%LanguageLevel: 2\n%%Pages: (atend)\n%%DocumentNeededResources: (atend)\n%%EndComments\n")
      (define document-coda
        "%%Trailer\n%%Pages: 2\n%%%%DocumentNeededResources: font Times-Roman\nEOF\n")
      (define (page-prelude title)
        (string-append
         "%%Page: "
         title
         "\n"
         "18 18 translate\n"
         ".1 setlinewidth 1 setlinecap 1 setlinejoin\n"
         (number->string (exact->inexact points-per-cell))
         " "
         (number->string (exact->inexact points-per-cell))
         " scale\n"
         "/Times-Roman .5 selectfont\n"))
      (define (arrow-path path)
        (if (> (length path)
               1)
            (string-append
             (arrow (point-x (car path))
                    (point-y (car path))
                    (direction-indicator (car path)
                                         (cadr path)))
             (arrow-path (cdr path)))
          ""))
      (define (render-path path)
        (string-append
         (move-to (point-x (car path))
                  (point-y (car path)))
         (let loop ((path path)
                    (result ""))
           (if (null? (cdr path))
               result
             (loop
              (cdr path)
              (string-append
               result
               (draw-to (point-x (cadr path))
                        (point-y (cadr path)))))))
         "stroke\n"))
      
      (define page-coda
        "stroke\nshowpage\n")

      (string-append
       document-prelude
       (page-prelude "The maze")
       (horizontal-line
        0
        array-height
        array-width)
       (vertical-line
        0
        0
        array-height)
       (let loop ((rows-processed 0)
                  (result ""))
         (if (= rows-processed array-height)
             result
           (loop (+ rows-processed 1)
                 (string-append
                  result
                  (let loop ((columns-processed 0)
                             (result ""))
                    (if (= columns-processed array-width)
                        result
                      (loop (+ columns-processed 1)
                            (string-append
                             result
                             
                             ;; Perhaps draw the right and bottom
                             ;; lines of this cell.  We draw the right
                             ;; line if the path neither went from
                             ;; this cell to the right-hand neighbor,
                             ;; nor vice-versa; we draw the bottom
                             ;; line similarly for the bottom
                             ;; neighbor. 

                             (let ((cell (array-ref grid columns-processed rows-processed))
                                   
                                   ;; Set RIGHT-NEIGHBOR either to the
                                   ;; contents of our right-hand
                                   ;; neighbor, or, if we're along the
                                   ;; right edge, to the empty list.
                                   ;; The empty list is, in effect, a
                                   ;; cell which we haven't entered
                                   ;; from this cell, and so we'll
                                   ;; draw a wall.  Thus we get the
                                   ;; rightmost border of the maze.

                                   (right-neighbor
                                    (if (= columns-processed (- array-width 1))
                                        '()
                                      (array-ref grid (+ 1 columns-processed) rows-processed)))
                                   (bottom-neighbor
                                    (if (= rows-processed 0)
                                        '()
                                      (array-ref grid columns-processed (- rows-processed 1)))))

                               (string-append
                                (if (or (memq 'east cell)
                                        (memq 'west right-neighbor))
                                    ""
                                  (vertical-line (+ 1 columns-processed)
                                                 rows-processed
                                                 1))
                                (if (or (memq 'south cell)
                                        (memq 'north bottom-neighbor))
                                    ""
                                  (horizontal-line columns-processed
                                                   rows-processed
                                                   1))
                                
                                ;; Special case: if this cell is on
                                ;; the path to the exit, and if it's
                                ;; the kind of cell where you have to
                                ;; decide which way to go, draw an
                                ;; arrow pointing the way to the exit.
                                ;(let ((path-to-exit (member (cons columns-processed rows-processed) solution)))
;                                  (if (and
;                                       (> (length cell)
;                                          2)
;                                       path-to-exit
;                                       (> (length path-to-exit) 1))
;                                      (with-gray-level
;                                       .825
;                                       (arrow (+ 1/2 columns-processed)
;                                              (+ 1/2 rows-processed)
;                                              (direction-indicator (car path-to-exit)
;                                                                   (cadr path-to-exit))))
;                                    ""))
                                ))))))))))
       (with-line-width
        .005
        (with-origin
         1/2 1/2
         (arrow-path
          solution)))
       "stroke\n"
       (let ((entrance (car solution))
             (exit (car (reverse solution))))
         (define (dot radius)
           (string-append
            "0 0 moveto 0 0 "
            (number->string (exact->inexact radius))
            " 0 360 arc fill\n"))
         (string-append
          (with-origin
           (translated-point entrance .5 .5)
           (dot .25)
           (with-gray-level .5 (text "Entrance")))
          (with-origin
           (translated-point exit .5 .5)
           (dot .25)
           (with-rotation
            180
            (with-gray-level .5 (text "Exit"))))))


       page-coda)))

  ;;;;;;;;;;;;;;;;;;;; GRID ;;;;;;;;;;;;;;;;;;;;

  ;; List of points describing how to get from where we started to the exit.
  (define solution '())
  
  ;; Modifies `grid' as if someone started at HERE, and at each cell,
  ;; chose at random one of its unvisited neighbors, wrote on the
  ;; floor a note indicating that they went to that cell, and
  ;; continuing from there.  If there are no unvisited neighbors, the
  ;; person backs up until either there are some unvisited neighbors,
  ;; or until they wind up where they started.

  ;; While this person is stumbling around randomly, if they happen to
  ;; land on the cell that we've arbitrarily designated the "exit",
  ;; they save the path that got them there in SOLUTION.  But they
  ;; continue wandering.
  (define (wander-from! here are-we-backing-up?)
          
    ;; Returns a list of cells that form a path from the origin to
    ;; P.  This is the solution to the maze, if P is the exit.
    (define (path-back-to-origin p)
      (let ((prev (neighbor-that-brought-me-to p)))
        (if (not prev)
            '()
          (cons prev (path-back-to-origin prev)))))
    
    ;; If we aren't backtracking, note that we've visited this cell.
    ;; Use a question mark instead of a direction indicator; that
    ;; shows that although we've been here, we haven't yet left.

    ;; If we marked the cell regardless of backtracking, then
    ;; ... something bad would happen, I think, but I forget what.
    (if (not are-we-backing-up?)
        (grid-mark-as-visited! here '?))

    ;; Make sure we don't fix the solution more than once.
    (if (and (grid-is-exit? here) (not are-we-backing-up?))
        (begin
          ;; (display "Hey!  We just stumbled across the exit.")
          ;; (newline)
          (set! solution (reverse (cons here (path-back-to-origin here))))))

    ;; The core algorithm.
    (let ((places-we-can-go (unvisited-neighbors here)))
        
      (if (null? places-we-can-go)

          ;; We can't go anywhere from here.  Go back to whence we came.
          (let ((whence (neighbor-that-brought-me-to here)))

            (if whence

                ;; Back up.
                (wander-from! whence #t)
              
              ;; We didn't arrive here from anywhere -- we must have
              ;; been placed here by God, which means we're at the
              ;; place we started, which means we're done.
              ))

        ;; Choose randomly among our choices, and continue wandering
        ;; from there.
        (let ((next-place (list-ref places-we-can-go 
                                    (uneven-random (length places-we-can-go))
                                    )))
          (grid-mark-as-visited! here (direction-indicator here next-place))
          (wander-from! next-place #f)))))
  
  ;;(display solution)
  ;;(newline)
  (wander-from! (make-point
                 (random (car  (array-dimensions grid)))
                 (random (cadr (array-dimensions grid)))
                 ) #f)
  (grid-render-in-postscript))

(define (x size)
  (let ((temp-file-name (tmpnam)))
    (call-with-output-file temp-file-name (lambda (port) (display (make-maze size) port)))
    (system (string-append "c:\\gstools\\gsview\\gsview32.exe " temp-file-name " && del " temp-file-name " 2>nul"))))
