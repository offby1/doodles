;; Generates a maze, then displays it using Ghostscript.

(require 'array)
(require 'filter)
(require 'random)
(require 'object->string)

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

(define (display-many . things)
  (for-each display things))

(define (make-maze grid-width)

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
  
  ;; Each entry will be two lists: one is a list of directions from
  ;; which we've left this cell and moved onto a virgin cell
  ;; (naturally its initial value is the empty list).  The direction
  ;; from which we most recently left is the car of the list.

  ;; The second list is a list of walls that this cell contains.  As
  ;; we wander through the maze, we'll knock down the walls.  These
  ;; two lists contain essentially the same information, but the first
  ;; list contains it in a form that's useful for constructing the
  ;; maze, and the second is in a form that's useful for solving it.

  ;; It's rectangular to match the aspect ratio of a printed page
  ;; oritented in "portrait mode".
  (define grid-height (round (* 5/4 grid-width)))

  (define grid (make-array '()
                           grid-width 
                           grid-height
                           ))

  (define (grid-initialize!)
    (let loop ((rows-initialized 0))
      (if (< rows-initialized grid-height)
          (begin
            (let loop ((columns-initialized 0))
              (if (< columns-initialized grid-width)
                  (begin
                    (array-set! grid (cons (list 'placeholder) (list 'east-wall 'south-wall)) columns-initialized rows-initialized)
                    (loop (+ 1 columns-initialized)))
                ))
            (loop (+ 1 rows-initialized))))))

  ;; prepends VALUE to the list located at point P.
  (define (grid-mark-as-visited! p value)
    (let ((cell (array-ref  grid    (point-x p) (point-y p))))
      (set-car! cell (cons value (car cell)))))

  (define (grid-knock-down-wall! p direction-indicator)
    (if (not (memq direction-indicator '(east south)))
        (error "grid-knock-down-wall! was called with a bad direction"))
    (let ((cell (array-ref grid (point-x p) (point-y p))))
      (set-cdr! cell (filter (lambda (wall)
                               (not (eq? wall (case direction-indicator
                                                ((east) 'east-wall)
                                                ((south) 'south-wall)
                                                (else '())))))
                             (cdr cell)))
      ))
  
  ;; List of directions that we've gone from this cell.
  (define (grid-where-weve-gone-from-here x y)
    (filter (lambda (direction)
              (not (eq? 'placeholder direction))) (car (array-ref grid x y))))
  
  (define (grid-this-cells-walls x y)
    (cdr (array-ref grid x y)))
  
  ;; Returns TRUE if and only if P is the cell that we arbitrarily
  ;; chose to be the exit.
  (define grid-is-exit?
    (let ((the-exit (make-point 
                     (- grid-width 1)
                     (- grid-height 1)
                     ;;(random grid-width)
                     ;;(random grid-height)
                     )))
      (lambda (p)
        (equal? p the-exit))))

  ;; Returns TRUE if and only if we've called `grid-mark-as-visited!'
  ;; on this cell.
  (define (grid-visited? p)
    (not (null? (grid-where-weve-gone-from-here (point-x p) (point-y
                                                            p)))))

  ;; If the cell referred to by P has been visited at all, returns the
  ;; newest value.  Otherwise returns #f.
  (define (most-recently-explored-direction p)
    (let ((datum (grid-where-weve-gone-from-here (point-x p) (point-y p))))
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
    (let* ((points-per-cell (/ (* 8 72) grid-width)))
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
         "grestore\n"))

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

      (define (with-scale px py . stuff)
        (with-isolated-context
         (number->string (exact->inexact px))
         " "
         (number->string (exact->inexact py))
         " scale\n"
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
      
      ;; Draws an arrow centered at x y.
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
         "/Times-Roman 1 selectfont\n"
         "0 0 moveto "
         "(" string ") show\n"))
      
      (define document-prelude
        "%!PS-Adobe-3.0\n%%Title: Maze\n%%LanguageLevel: 2\n%%Pages: (atend)\n%%DocumentNeededResources: (atend)\n%%EndComments\n")
      (define document-coda
        "%%Trailer\n%%Pages: 1\n%%DocumentNeededResources: font Times-Roman\n%%EOF\n")
      (define page-prelude
        ".1 setlinewidth 1 setlinecap 1 setlinejoin\n")
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
       page-prelude
       (with-origin
        (make-point 18 18)
        (with-scale
         points-per-cell points-per-cell
         ;;(with-origin (make-point (/ grid-width 2) (+ grid-height 1/3)) (text "A maze"))
         "% Top wall\n"
         (horizontal-line
          0
          grid-height
          grid-width)
         "% Left wall\n"
         (vertical-line
          0
          0
          grid-height)
         "% Maze proper\n"
         (let loop ((rows-processed 0)
                    (result ""))
           (if (= rows-processed grid-height)
               result
             (loop (+ rows-processed 1)
                   (string-append
                    result
                    (let loop ((columns-processed 0)
                               (result ""))
                      (if (= columns-processed grid-width)
                          result
                        (loop (+ columns-processed 1)
                              (string-append
                               result

                               (if #t ""
                                 (let ((cell (array-ref grid columns-processed rows-processed)))
                                   (with-origin
                                    (translated-point (make-point columns-processed rows-processed)
                                                      .1 .1)
                                    (with-scale
                                     .05 .05
                                     (text (object->string
                                            (filter (lambda (dir)
                                                      (not (eq? dir 'I-dead-ended-here-once))) (grid-where-weve-gone-from-here columns-processed rows-processed)))))
                                    (with-origin
                                     (make-point 0 .15)
                                     (with-scale
                                      .05 .05
                                      (text (object->string (cdr cell)))))
                                    )))
                               
                               
                               (let ((walls (grid-this-cells-walls
                                             columns-processed
                                             rows-processed)))
                                 
                                 (string-append
                                  (if (memq 'east-wall walls)
                                      (vertical-line (+ 1 columns-processed)
                                                     rows-processed
                                                     1)
                                    "")
                                  (if  (memq 'south-wall walls)
                                      (horizontal-line columns-processed
                                                       rows-processed
                                                       1)
                                    "")))))))))))

         "stroke\n"
         "% The solution\n"
         (with-line-width
          1/3
          (with-gray-level
           .75
           (with-origin
            (make-point 1/2 1/2)
            (render-path
             solution))))
         (let ((entrance (car solution))
               (exit (car (reverse solution))))
           (define (dot radius)
             (string-append
              "0 0 moveto 0 0 "
              (number->string (exact->inexact radius))
              " 0 360 arc fill\n"))
           (string-append
            "% The entrance\n"
            (with-origin
             (translated-point entrance .5 .5)
             (dot 1/3)
             ;;(with-gray-level .5 (text "Entrance"))
             )
            "% The exit\n"
            (with-origin
             (translated-point exit .5 .5)
             (dot 1/3)
             ;;(with-rotation 180 (with-gray-level .5 (text "Exit")))
             )))         )      
        )
       page-coda
       document-coda)))

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

    ;; Make sure we don't set! the solution more than once.
    (if (and (grid-is-exit? here) (not are-we-backing-up?))
        (set! solution (reverse (cons here (path-back-to-origin here)))))

    ;; The core algorithm.
    (let ((places-we-can-go (unvisited-neighbors here)))
        
      (if (null? places-we-can-go)

          ;; We can't go anywhere from here.  Go back to whence we came.
          (let ((whence (neighbor-that-brought-me-to here)))

            (grid-mark-as-visited! here 'I-dead-ended-here-once)
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
          (let ((which-way-to-go  (direction-indicator here next-place)))
            (grid-mark-as-visited! here which-way-to-go)
            
            ;; Only knock down east or south walls.  If we're headed
            ;; north or west, knock down the neighbor's wall.
               (case which-way-to-go
                 ((east south) (grid-knock-down-wall! here which-way-to-go))
                 ((north) (grid-knock-down-wall! (translated-point here 0 1) 'south))
                 ((west) (grid-knock-down-wall! (translated-point here -1 0) 'east))
                 (else (error "Uh oh."))
            ))
          (wander-from! next-place #f)))))

  (grid-initialize!)
  (wander-from! (make-point
                 0 0
                 ;;(random grid-width)
                 ;;(random grid-height)
                 ) #f)

  (grid-render-in-postscript))

(define (x size)
  (let ((temp-file-name (tmpnam)))
    (call-with-output-file temp-file-name (lambda (port) (display (make-maze size) port)))
    (system (string-append "c:\\gstools\\gsview\\gsview32.exe " temp-file-name " && del " temp-file-name " 2>nul"))))
