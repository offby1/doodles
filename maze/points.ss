(define rot-90-ccw
  (case-lambda
   ((p  ) (make-rectangular (- (imag-part p)) (real-part p)))
   ((p o) (+ o (rot-90-ccw (- p o))))))

(define (four-neighbors p)
  (let* ((above (rot-90-ccw (add1 p) p))
         (left  (rot-90-ccw above    p))
         (below (rot-90-ccw left     p))
         (right (rot-90-ccw below    p)))
    (list above left below right)))

(define pi/3 (acos 1/2))

(define rot-60-ccw
  (lambda (p)
    (let ((m (magnitude p))
          (a (angle p)))
      (make-polar m
                  (+ a pi/3)))))

;; given an "id" of a hexagon, return the coordinates of its center.
(define id-to-coordinates
  (let ((sqrt-3/4 (sqrt 3/4)))
    (lambda (id size-length)
      (let ((p (car id))
            (q (cdr id)))
        (cons (* p 3/2 size-length)
              (if (even? p)
                  (* q 2 sqrt-3/4)
                (* 2 (+ 1/2 q) sqrt-3/4)))))))