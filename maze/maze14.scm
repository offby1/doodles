
;; Generates a maze, then displays it using Ghostscript.

(require 'filter)
(require 'random)
(require 'object->string)
(require 'note)

;;;;;;;;;;;;;;;;;;;; array ;;;;;;;;;;;;;;;;;;;;
;; Arrays are lists with three elements.  The first element is its
;; width; the second is its height; the third is a vector holding the
;; data.

;; Yes, I know that SLIB has an array module; but that module used so
;; much memory that my computer ground to a halt.
(define array-width car)
(define array-height cadr)

(define (make-array init width height)
  (list width height (make-u8vector (* width height) init)))

(define (array-in-bounds? array column row)
  (and (not (negative? column))
       (not (negative? row))
       (< column (array-width  array))
       (< row    (array-height array))))

(define (array:column-row-to-vector-offset array column row)
  (+ column (* row (array-width array))))

(define (array:vector-offset-to-column-and-row array offset)
  (cons
   (remainder offset (array-width array)) ;column
   (quotient offset (array-width array)) ;row
   ))

(define (array-ref array column row)
  (u8vector-ref (caddr array) (array:column-row-to-vector-offset array column row)))

(define (array-set! array datum column row)
  (if (not (is-cell? datum))
      (error "You must only put cells in this array."))
  (u8vector-set! (caddr array) (array:column-row-to-vector-offset array column row) datum))

;;;;;;;;;;;;;;;;;;;; array ;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;; bit ;;;;;;;;;;;;;;;;;;;;
(define bit:int->vector
  (let ((result (make-vector 8 #f)))
    (lambda (int)
      (let loop ((int int)
                 (bits-set 0))
        (if (= 8 bits-set)
            result
          (begin
            (vector-set! result bits-set (not (zero? (remainder int 2))))
            (loop (quotient int 2)
                  (+ bits-set 1))))))))

(define bit:vector->int
  (lambda (v)
    (let loop ((result 0)
               (bits-examined 0)
               (power-of-two 1))
      (if (= bits-examined 8)
          result
        (loop (+ result (if (vector-ref v bits-examined)
                            power-of-two
                          0))
              (+ 1 bits-examined)
              (* 2 power-of-two))))))

(define bit
  (lambda (b n)
    (vector-ref (bit:int->vector  n) (- 7 b))))

(define (set-bit b n value)
  (if (not (boolean? value))
      (error "Hey!!"))

  (let ((v (bit:int->vector n)))
    (vector-set! v (- 7 b) value)
    (bit:vector->int v)))

;;;;;;;;;;;;;;;;;;;; bit ;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;; cell ;;;;;;;;;;;;;;;;;;;;

;; Cells contain a few pieces of data:

;; a flag (bit 0) that says if the cell was ever visited by our wanderer;

;; Only if the above flag is set, a flag indicating the direction the
;; wanderer most recently headed from this cell (bit 1 - east; bit 2 -
;; south; bit 3 - west; bit 4 - north) .  At most one of these flags
;; will be set.

;; a flag (bit 5) indicating the presence of the east wall, and a flag
;; (bit 6) indicating the presence of the south wall.

(define (make-cell)
  (note 'make-cell "Making a new cell\n")
  (set-bit 5 (set-bit 6 0 #t) #t))
(define (is-cell? c)
  (and (integer? c)
       (exact? c)
       (not (negative? c))
       (< c 256)))

(define (cell->string cell)
  (string-append
   (if (cell-was-visited? cell)
       (string-append
        "visited "
        (object->string (cell-exit-most-recently-taken cell))
        )
     "")
   (apply string-append (map (lambda (direction-symbol)
                                  (string-append (symbol->string
                                                  direction-symbol)
                                                 " wall")) (cell-walls cell)))
   ))

(define (cell-was-visited? cell) (bit 0 cell))
(define (cell-exit-most-recently-taken cell)

  (if (not (cell-was-visited? cell))
      (error "Don't ask what exit was taken if the cell was never visited!"))

  (cond
   ((bit 1 cell)
    (if (or (bit 2 cell)
            (bit 3 cell)
            (bit 4 cell))
        (error "Uh oh!")
      'east))
   ((bit 2 cell)
    (if (or (bit 1 cell)
            (bit 3 cell)
            (bit 4 cell))
        (error "Oh no!")
      'south))
   ((bit 3 cell)
    (if (or (bit 1 cell)
            (bit 2 cell)
            (bit 4 cell))
        (error"Eeek!")
      'west))
   ((bit 4 cell)
    (if (or (bit 1 cell)
            (bit 2 cell)
            (bit 3 cell))
        (error "Damn!")
      'north))
   (#t #f)))

(define (cell-mark-last-visit cell direction)
  (set! cell (set-bit 1 cell #f))
  (set! cell (set-bit 2 cell #f))
  (set! cell (set-bit 3 cell #f))
  (set! cell (set-bit 4 cell #f))

  (if (eq? 'unknown direction)
      (set-bit 0 cell #t)
    (set-bit
     (case direction
       ((east )  1)
       ((south)  2)
       ((west )  3)
       ((north)  4)
       (else (error "Bad direction")))
     cell
     #t)))

(define (cell-walls cell)
  (let ((result '()))
    (if (bit 5 cell)
        (set! result (cons 'east result)))

    (if (bit 6 cell)
        (set! result (cons 'south result)))

    result))

(define (cell-remove-wall cell wall)
  (set-bit
   (case wall
     ((east ) 5)
     ((south) 6)
     (else (error "Drat!!")))
   cell
   #f))

;;;;;;;;;;;;;;;;;;;; cell ;;;;;;;;;;;;;;;;;;;;


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

  (define grid (make-array 0
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
                    (array-set! grid (make-cell) columns-initialized rows-initialized)
                    (loop (+ 1 columns-initialized)))
                ))
            (loop (+ 1 rows-initialized))))))

  ;; prepends VALUE to the list located at point P.
  (define (grid-mark-as-visited! p value)
    (let ((cell (array-ref  grid    (point-x p) (point-y p))))
      (array-set! grid (cell-mark-last-visit cell value) (point-x p) (point-y p))))

  (define (grid-knock-down-wall! p direction-indicator)
    (if (not (memq direction-indicator '(east south)))
        (error "grid-knock-down-wall! was called with a bad direction"))
    (let ((cell (array-ref grid (point-x p) (point-y p))))
      (array-set! grid (cell-remove-wall cell direction-indicator)
                   (point-x p) (point-y p))
      ))

  (define (grid-this-cells-walls x y)
    (cell-walls (array-ref grid x y)))
  
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
    (cell-was-visited? (array-ref grid (point-x p) (point-y p))))

  ;; If the cell referred to by P has been visited at all, returns the
  ;; newest value.  Otherwise returns #f.
  (define (most-recently-explored-direction p)
    (cell-exit-most-recently-taken (array-ref grid (point-x p) (point-y p))))

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
                             (and
                              (grid-visited? p)
                              (eq? (direction-indicator p point)
                                   (most-recently-explored-direction p)))))
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
    (let ((points-per-cell (/ (* 8 72) grid-width)))
      (define (flatten-strings thing)
        (note 'flatten-strings "Flattening\n" )
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
         (general-line
          0
          grid-height
          grid-width
          grid-height)
         "% Left wall\n"
         (general-line
          0
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
                        (begin
                          (note 'render "Rendering one cell\n")
                          (loop (+ columns-processed 1)
                                (string-append
                                 result
                               
                                 (let ((walls (grid-this-cells-walls
                                               columns-processed
                                               rows-processed)))
                                 
                                   (string-append
                                    (if (memq 'east walls)
                                        (general-line (+ 1 columns-processed)
                                                      rows-processed
                                                      (+ 1 columns-processed)
                                                      (+ 1 rows-processed))
                                      "")
                                    (if  (memq 'south walls)
                                        (general-line columns-processed
                                                      rows-processed
                                                      (+ 1 columns-processed)
                                                      rows-processed)
                                      ""))))))))))))

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
             (dot 1/3))
            
            "% The exit\n"
            (with-origin
             (translated-point exit .5 .5)
             (dot 1/3))))))
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

    (note 'wander "Wandering\n")
    (grid-mark-as-visited! here 'unknown)

    ;; Make sure we don't set! the solution more than once.
    (if (and (grid-is-exit? here) (not are-we-backing-up?))
        (set! solution (reverse (cons here (path-back-to-origin here)))))

    ;; The core algorithm.
    (let ((places-we-can-go (unvisited-neighbors here)))
        
      (if (null? places-we-can-go)

          ;; We can't go anywhere from here.  Go back to whence we came.
          (let ((whence (neighbor-that-brought-me-to here)))

            (if whence

                ;; Back up.
                (begin
                  (note 'backup "Backing up\n")
                  (wander-from! whence #t))
              
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
            (note 'new-ground "Heading somewhere new\n")
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
  
  (time (grid-initialize!))
  
  (time (wander-from! (make-point
                 0 0
                 ;;(random grid-width)
                 ;;(random grid-height)
                 ) #f))
  
  (time (grid-render-in-postscript)))

(define (x size)
  (let ((temp-file-name (tmpnam)))
    (note #t)
    (call-with-output-file temp-file-name (lambda (port) (display (make-maze size) port)))
    (note #f)
    (system (string-append "c:\\gstools\\gsview\\gsview32.exe " temp-file-name " && del " temp-file-name " 2>nul"))))
