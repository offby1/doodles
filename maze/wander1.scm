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

  ;;;;;;;;;;;;;;;;;;;; grid ;;;;;;;;;;;;;;;;;;;;
  
  ;; Each entry will be either #f, to indicate that it's never been
  ;; visted; or #t.
  (define grid (make-array #f 5 5))

  (define (grid-mark-as-visited! p)
    (array-set! grid #t (point-x p) (point-y p)))

  (define (grid-visited? p)
    (array-ref  grid    (point-x p) (point-y p)))

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
  ;;;;;;;;;;;;;;;;;;;; grid ;;;;;;;;;;;;;;;;;;;;

  (define how-I-got-here (make-stack))
  (define are-we-backing-up? #f)
  (define result '())    

  (define (journeys-from here)

    (grid-mark-as-visited! here)

    (let ((places-we-can-go (unvisited-neighbors here)))
        
      (if (null? places-we-can-go)
            
          ;; We can't go anywhere from here.  Back up...
            
          (begin

            ;; If we just started backing up, our journey just ended.
            ;; Show how we got here.
            (if (not are-we-backing-up?)
                (set! result (append result (list (reverse (append (list here) how-I-got-here))))))

            (if (not (stack-empty? how-I-got-here))

                (begin

                  (set! are-we-backing-up? #t)
                  (journeys-from (stack-pop! how-I-got-here)))
              
              ;; We can't even back up, so we must be done.

              ))

        ;; Choose randomly among our choices, and continue wandering
        ;; from there.
        (let ((next-place (list-ref places-we-can-go (random (length places-we-can-go)))))
          ;; If we were backing up, clear out the history.
          (if are-we-backing-up? (set! how-I-got-here (make-stack)))
          (set! are-we-backing-up? #f)
          (stack-push! how-I-got-here here)
          (journeys-from next-place)))

         result))

  (journeys-from (make-point x y)))

(define (longest seq)
  (accumulate (lambda (l1 l2)
                (if (> (length l1)
                       (length l2))
                    l1
                  l2))
              (car seq) seq))