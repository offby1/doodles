;; Take a random walk through a grid.  Don't step on any square that
;; we've already stepped on -- unless we find that we have no other
;; choice, in which case back up until we do have a choice, or until
;; there's no more steps to back up.  I expect to fill the entire grid
;; this way.

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

(define-macro (debug variable)
  `(begin
     (display (string-append (symbol->string ',variable) " is "))
     (display ,variable)
     (newline)))

;; x increases to the right; y increases upward.
(define (array->ps a)
  (define (vertical-line x y length)
    (string-append
     (number->string x)
     " "
     (number->string y)
     " moveto "
     (number->string x)
     " "
     (number->string (+ y length))
     " lineto\n"))
  (define (horizontal-line x y length)
    (string-append
     (number->string x)
     " "
     (number->string y)
     " moveto "
     (number->string (+ x length))
     " "
     (number->string y)
     " lineto\n"))
  (define usual-prelude
    "%!PS-Adobe-3.0\n%%Title: Maze\n%%LanguageLevel: 2\n%%EndComments\n3 3 scale\n1 72 div setlinewidth\n")
  (define usual-coda
    "stroke\nshowpage\n")
  (string-append
   usual-prelude
   (let ((array-width (car (array-dimensions a)))
         (array-height (cadr (array-dimensions a))))
     (string-append
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
                            (let ((cell (array-ref a columns-processed rows-processed))
                                  (right-neighbor
                                   (if (= columns-processed (- array-width 1))
                                       '()
                                     (array-ref a (+ 1 columns-processed) rows-processed)))
                                  (bottom-neighbor
                                   (if (= rows-processed 0)
                                       '()
                                     (array-ref a columns-processed (- rows-processed 1)))))
                              
                              (string-append
                               (if (or (memq 'east cell)
                                       (memq 'west right-neighbor))
                                   ""
                                 (vertical-line columns-processed
                                                rows-processed
                                                1))
                               (if (or (memq 'south cell)
                                       (memq 'north bottom-neighbor))
                                   ""
                                 (horizontal-line columns-processed
                                                  rows-processed
                                                  1))
                               ))))))))))))
   usual-coda))

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

(define (wander-from x y)

  ;;;;;;;;;;;;;;;;;;;; grid ;;;;;;;;;;;;;;;;;;;;
  
  ;; Each entry will be a list of directions from which we've already
  ;; explorered.  Naturally the initial value is the empty list.  The
  ;; direction we most recently explored is the car of the list.
  (define grid (make-array '()  100 100))

  (define (grid-mark-as-visited! p value)
    (let ((old-value (array-ref  grid    (point-x p) (point-y p))))
      (array-set! grid (cons value old-value) (point-x p) (point-y p))))

  (define (grid-visited? p)
    (not (null? (array-ref  grid    (point-x p) (point-y p)))))

  (define (most-recently-explored-direction p)
     (car (array-ref  grid    (point-x p) (point-y p))))
  
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
                            (and (array-in-bounds? grid (point-x p)
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
  
  ;;;;;;;;;;;;;;;;;;;; grid ;;;;;;;;;;;;;;;;;;;;

  (define (journeys-from here are-we-backing-up? path-length-so-far)

    (if (not are-we-backing-up?)
        (grid-mark-as-visited! here '?))

    (let ((places-we-can-go (unvisited-neighbors here)))
        
      (if (null? places-we-can-go)

          ;; We can't go anywhere from here.  Go back to whence we came.
          (let ((whence (neighbor-that-brought-me-to here)))
            
            (if whence

                ;; Back up.
                (journeys-from whence #t (- path-length-so-far 1))
              
              ;; We didn't arrive here from anywhere -- we must have
              ;; been placed here by God, which means we're done.
              ))

        ;; Choose randomly among our choices, and continue wandering
        ;; from there.
        (let ((next-place (list-ref places-we-can-go (random (length places-we-can-go)))))
          (grid-mark-as-visited! here (direction-indicator here next-place))
          (journeys-from next-place #f (+ path-length-so-far 1))))

      grid))

  (array->ps (journeys-from (make-point x y) #f 0)))

(define (longest seq)
  (accumulate (lambda (l1 l2)
                (if (> (length l1)
                       (length l2))
                    l1
                  l2))
              (car seq) seq))

(let ((temp-file-name (tmpnam)))
  (system (string-append "del " temp-file-name " 2>nul"))
  (call-with-output-file temp-file-name (lambda (port) (display (wander-from 0 0) port)))
  (system (string-append "start c:\\gs\\gswin.exe " temp-file-name)))
