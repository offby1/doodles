(require 'array)
(require 'filter)
(require 'random)
(require 'sort)
(require 'object->string)
;;;;;;;;;;;;;;;;;;;; point ;;;;;;;;;;;;;;;;;;;;
(define make-point cons)
(define point-x car)
(define point-y cdr)
;;;;;;;;;;;;;;;;;;;; point ;;;;;;;;;;;;;;;;;;;;

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
  
  ;; Each entry will be a list of directions from which we've already
  ;; explorered.  Naturally the initial value is the empty list.  The
  ;; direction we most recently explored is the car of the list.
  (define grid (make-array '() width-in-cells (round (* 5/4 width-in-cells))))

  (define (grid-mark-as-visited! p value)
    (let ((old-value (array-ref  grid    (point-x p) (point-y p))))
      (array-set! grid (cons value old-value) (point-x p) (point-y p))))

  (define (grid-is-exit? p)
    (and (= (point-x p)
            (- (car (array-dimensions grid))
               1))
         (= (point-y p)
            (- (cadr (array-dimensions grid))
               1))))
  
  (define (grid-visited? p)
    (not (null? (array-ref  grid    (point-x p) (point-y p)))))

  (define (most-recently-explored-direction p)
    (let ((datum (array-ref  grid    (point-x p) (point-y p))))
      (if (pair? datum)
          (car datum)
        #f)))
  
  (define (all-neighbors point)
    (let* ((px (point-x point))
           (py (point-y point)))
      (list (make-point (- px 1) py)
            (make-point (+ px 1) py)
            (make-point px (- py 1))
            (make-point px (+ py 1)))))
  
  (define (unvisited-neighbors point)
    (filter (lambda (p)
              (and
               (array-in-bounds? grid (point-x p) (point-y p))
               (not (grid-visited? p))))
            (all-neighbors point)))

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

  (define (grid-render-in-postscript solution)
    (let* ((array-width (car (array-dimensions grid)))
           (array-height (cadr (array-dimensions grid)))
           (points-per-cell (/ (* 6 72) array-width)))
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

      (define (vertical-line x y length)
        (general-line x y x (+ y length)))
           
      (define (horizontal-line x y length)
        (general-line x y (+ x length) y))
           
      (define document-prelude
        "%!PS-Adobe-3.0\n%%Title: Maze\n%%LanguageLevel: 2\n%%EndComments\n")
      (define page-prelude
        (string-append
         "18 18 translate\n"
         ".1 "
         ;;(number->string (exact->inexact points-per-cell))
         ;;" div "
         "setlinewidth\n"
         (number->string (exact->inexact points-per-cell))
         " "
         (number->string (exact->inexact points-per-cell))
         " scale\n"))
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
                        (point-y (cadr path)))))))))
      
      (define page-coda
        "stroke\nshowpage\n")
      (string-append
       document-prelude
       page-prelude
       "% - The maze\n"
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
                             (let ((cell (array-ref grid columns-processed rows-processed))
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
                                ))))))))))
       page-coda
       page-prelude
       "% - The solution\n"
       ".01 setlinewidth\n"
       (render-path (map (lambda (p)
                           (cons (+ 1/2 (car p))
                                 (+ 1/2 (cdr p)))) solution))
       page-coda)))

  ;;;;;;;;;;;;;;;;;;;; GRID ;;;;;;;;;;;;;;;;;;;;

  (define solution '())
  (define (wander-from! here are-we-backing-up?)
          
    (define (path-back-to-origin p)
      (let ((prev (neighbor-that-brought-me-to p)))
        (if (not prev)
            '()
          (cons prev (path-back-to-origin prev)))))
    
    (if (not are-we-backing-up?)
        (grid-mark-as-visited! here '?))

    (if (and (grid-is-exit? here) (not are-we-backing-up?))
        (begin (display "Hey!  We just stumbled across the exit.")
               (newline)
               (set! solution (cons here (path-back-to-origin here)))))

    (let ((places-we-can-go (unvisited-neighbors here)))
        
      (if (null? places-we-can-go)

          ;; We can't go anywhere from here.  Go back to whence we came.
          (let ((whence (neighbor-that-brought-me-to here)))

            (if whence

                ;; Back up.
                (wander-from! whence #t)
              
              ;; We didn't arrive here from anywhere -- we must have
              ;; been placed here by God, which means we're done.
              ))

        ;; Choose randomly among our choices, and continue wandering
        ;; from there.
        (let ((next-place (list-ref places-we-can-go (random (length places-we-can-go)))))
          (grid-mark-as-visited! here (direction-indicator here next-place))
          (wander-from! next-place #f))))

    solution)

  
  (let ((solution (wander-from! (make-point 0 0) #f)))
    (display solution)
    (newline)
    (grid-render-in-postscript solution)))

(let ((temp-file-name (tmpnam)))
  (system (string-append "del " temp-file-name " 2>nul"))
  (call-with-output-file temp-file-name (lambda (port) (display (make-maze 20) port)))
  (system (string-append "start c:\\gs\\gswin.exe " temp-file-name)))
