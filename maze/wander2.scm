;; Take a random walk through a grid.  Don't step on any square that
;; we've already stepped on -- unless we find that we have no other
;; choice, in which case back up until we do have a choice, or until
;; there's no more steps to back up.  I expect to fill the entire grid
;; this way.

(require 'array)
(require 'filter)
(require 'random)
(require 'sort)

;;;;;;;;;;;;;;;;;;;; point ;;;;;;;;;;;;;;;;;;;;
(define make-point cons)
(define point-x car)
(define point-y cdr)
;;;;;;;;;;;;;;;;;;;; point ;;;;;;;;;;;;;;;;;;;;

;; x increases to the right; y increases upward.
(define (array->string a)
  (let ((array-width (car (array-dimensions a)))
        (array-height (cadr (array-dimensions a))))
    (let loop ((rows-to-process array-height)
               (result ""))
      (if (zero? rows-to-process)
          result
        (loop (- rows-to-process 1)
              (string-append
               result
               (let loop ((columns-processed 0)
                          (result ""))
                 (if (= columns-processed array-width)
                     result
                   (loop (+ 1 columns-processed)
                         (string-append
                          result
                          (let ((thing (array-ref a columns-processed (- rows-to-process 1))))
                            (cond
                             ((symbol? thing)
                              (symbol->string thing))
                             ((number? thing)
                              (number->string (remainder thing 10)))
                             (#t ".")))
                          "|"))))
               "\n"
               (make-string (* 2 array-width) #\-)
               "\n"))))))

(define (direction-indicator source dest)
  (case (- (point-x dest)
           (point-x source))
    ((0)
     (case (- (point-y dest)
              (point-y source))
       ((1) '^)
       ((-1) 'v)
       (else (error "Uh oh."))))
    ((-1) '<)
    ((1) '>)
    (else (error "Uh oh."))))

(define (wander-from x y)

  ;;;;;;;;;;;;;;;;;;;; grid ;;;;;;;;;;;;;;;;;;;;
  
  ;; Each entry will be either #f, to indicate that it's never been
  ;; visited; or #t.
  (define grid (make-array #f 10 10))

  (define (grid-mark-as-visited! p value)
    (array-set! grid value (point-x p) (point-y p)))

  (define (grid-visited? p)
    (array-ref  grid    (point-x p) (point-y p)))

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
                                 (grid-visited? p))))
                          (all-neighbors point))))
      (if (and (pair? result)
               (not (= 1 (length result))))
          (error "Damn."))
         (if (pair? result)
             (car result)
           #f)))
  
  
  ;;;;;;;;;;;;;;;;;;;; grid ;;;;;;;;;;;;;;;;;;;;

  (define (journeys-from here are-we-backing-up?)

    (let ((places-we-can-go (unvisited-neighbors here)))
        
      (if (null? places-we-can-go)
            
          ;; We can't go anywhere from here.  Back up...
            
          (begin

            ;; If we just started backing up, our journey just ended.
            ;; Show how we got here.
            (if (not are-we-backing-up?) (grid-mark-as-visited! here '?))

            (let ((whence (neighbor-that-brought-me-to here)))
              
              (if whence

                  ;; Back up.
                  (journeys-from whence #t)
              
                ;; We can't even back up, so we must be done.
                )))

        ;; Choose randomly among our choices, and continue wandering
        ;; from there.
        (let ((next-place (list-ref places-we-can-go (random (length places-we-can-go)))))
          (grid-mark-as-visited! here (direction-indicator here next-place))
          (journeys-from next-place #f)))

         (array->string grid)))

  (journeys-from (make-point x y) #f))

(define (longest seq)
  (accumulate (lambda (l1 l2)
                (if (> (length l1)
                       (length l2))
                    l1
                  l2))
              (car seq) seq))
