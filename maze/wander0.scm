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

;;;;;;;;;;;;;;;;;;;; stack ;;;;;;;;;;;;;;;;;;;;
;; SLIB defines a queue that does what I want ... except there's no
;; way to display a queue.

(define-macro (stack-push! stack datum)
  `(set! ,stack (cons ,datum ,stack)))

(define-macro (stack-pop! stack)
  `(let ((result (car ,stack)))
     (set! ,stack (cdr ,stack))
     result))
        
(define (make-stack) '())

(define stack-empty? null?)
;;;;;;;;;;;;;;;;;;;; stack ;;;;;;;;;;;;;;;;;;;;

(define (wander-from x y)

  (define history (make-stack))
  (define are-we-backing-up? #f)

  ;;;;;;;;;;;;;;;;;;;; grid ;;;;;;;;;;;;;;;;;;;;
  
  ;; Each entry will be either #f, to indicate that it's never been
  ;; visted; or #t.
  (define grid (make-array #f 10 10))

  (define (grid-mark-as-visited! p)
    (array-set! grid #t (point-x p) (point-y p)))

  (define (grid-visited? p)
    (array-ref  grid    (point-x p) (point-y p)))

  ;;;;;;;;;;;;;;;;;;;; grid ;;;;;;;;;;;;;;;;;;;;
    
  (define (internal-wander-from origin)

    (define (unvisited-neighbors point)
      (let* ((px (point-x point))
             (py (point-y point))
             (all-neighbors (list (make-point (- px 1) py)
                                  (make-point (+ px 1) py)
                                  (make-point px (- py 1))
                                  (make-point px (+ py 1)))))
        (filter (lambda (point)
                  (and
                   (array-in-bounds? grid (point-x point) (point-y point))
                   (not (grid-visited? point))))
                all-neighbors)))

    (grid-mark-as-visited! origin)

    (let ((places-we-can-go (unvisited-neighbors origin)))
        
      (if (null? places-we-can-go)
            
          ;; We can't go anywhere from here.  Back up...
            
          (if (not (stack-empty? history))

              (begin
                
                ;; If we just started backing up, show how we got here.
                (if (not are-we-backing-up?)
                    (begin
                      (display (reverse (append (list origin) history)))
                      (newline)))

                (set! are-we-backing-up? #t)
                (internal-wander-from (stack-pop! history)))
              
            ;; We can't even back up, so we must be done.

            "We're done.")

        ;; Choose randomly among our choices, and continue wandering
        ;; from there.
        (let ((next-place (list-ref places-we-can-go (random (length places-we-can-go)))))
          (set! are-we-backing-up? #f)
          (stack-push! history origin)
          (internal-wander-from next-place)))))

  (internal-wander-from (make-point x y)))
