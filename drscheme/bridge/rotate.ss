;; TODO: figure out how to prevent this module from loading if any of
;; its self-tests fail.

(module rotate mzscheme
  (require (lib "cards.ss" "games" "cards")) ; for region et al.
  (require (lib "class.ss"))

  (require (lib "test.ss"    "schemeunit"))
  (require (lib "text-ui.ss" "schemeunit"))
  (require (lib "list.ss"))

  (provide make-point
           point-x
           point-y
           rotate-region)
  (define make-point make-rectangular)
  (define point-x real-part)
  (define point-y imag-part)

  ;; subtlety: since we're dealing with computer graphics, the origin
  ;; is in the upper left (not the lower left), and y values increase
  ;; downwards, not upwards.  Thus point x, y goes to y, -x.  (If the
  ;; origin were the lower left, and y increased upwards, x, y would
  ;; go to -y, x.)
  (define (rotate-quarter-turn-ccw p)
    (make-point (point-y p)
                (- (point-x p))))

  (define (rotate-some p quarter-turns)
    (set! quarter-turns (modulo quarter-turns 4))
    (if  (zero? quarter-turns)
        p
      (rotate-some (rotate-quarter-turn-ccw p)
                   (sub1 quarter-turns))))

  (define (rotate-about p origin quarter-turns)
    (+ origin
       (rotate-some (- p origin) quarter-turns)))

  (define (translate p dx dy)
    (make-point (+ dx (point-x p))
                (+ dy (point-y p))))

  (define (region->string r)
    (format "ul: ~A, ~A; w: ~A; h: ~A"
            (region-x r)
            (region-y r)
            (region-w r)
            (region-h r)))
  (define (region->points r)
    (let* ((ul (make-point (region-x r) (region-y r)))
           (lr (translate ul (region-w r) (region-h r))))
      (list ul lr)))

  (define (points->region points label callback)
    (define p1 (first points))
    (define p2 (second points))
    (define ul
      (make-point (min (point-x p1)
                       (point-x p2))
                  (min (point-y p1)
                       (point-y p2))))
    
    (make-region (point-x ul)
                 (point-y ul)
                 (abs (- (point-x p1)
                         (point-x p2)))
                 (abs (- (point-y p1)
                         (point-y p2)))
                 label callback))        
                          
  (define rotate-region
    (lambda (r origin quarter-turns)
      (points->region (map
                       (lambda (p)
                         (rotate-about p origin quarter-turns))
                       (region->points r))
                                                 
                      (region-label r)
                      (region-callback r))
      ))

  (define-assertion (assert-member obj seq)
    (member obj seq))

  (test/text-ui
   (let ((origin (make-point 0 0))
         (victim (make-point 1 1)))
     (make-test-suite
      "everything"

      (make-test-case
       "Rotate 10, 10"
       (let ((rotated (rotate-quarter-turn-ccw (make-point 10 10))))
         (assert-equal? rotated (make-point 10 -10))))
      
      (make-test-case 
       "zero"
       (let ((actual (rotate-about victim origin 0)))
         (assert-true (exact? actual))
         (assert-equal? victim actual)))

      (make-test-case
       "1"
       (assert-equal? (make-point 1 -1)
                      (rotate-about victim origin 1)))

      (make-test-case
       "2"
       (assert-equal? -1-1i
                      (rotate-about victim origin 2)))

      (make-test-case
       "wrap"
       (assert-equal? (rotate-about victim origin 1)
                      (rotate-about victim origin 5)))

      (make-test-case
       "assertion on non-integer rotations"
       (assert-exn exn:application:type?
                   (lambda ()
                     (rotate-about victim origin 1.333)
                     ))
       (assert-exn exn:application:type?
                   (lambda ()
                     (rotate-about victim origin 1/8)
                     )))

      (make-test-case
       "Region->points 1"
       (let ((points (region->points  (make-region 0 0 10 0 #f #f))))
         (assert-equal? 2 (length points) "exactly four points")
         (assert-member (make-point  0  0) points "0 0")
         (assert-member (make-point 10  0) points "10 0")
         ))
      (make-test-case
       "Region->points 2"
       (let ((points (region->points (make-region 2 3 4 5 #f #f))))
         (assert-equal? 2 (length points) "exactly four points")
         (assert-member (make-point  2 3) points )
         (assert-member (make-point  6 8) points )
         ))
      (make-test-case
       "region->points 3"
       (let ((p (region->points (points->region (list (make-point 10 10)
                                                      (make-point 20 10)) #f #f))))
         (assert-member (make-point 10 10) p)
         (assert-member (make-point 20 10) p)))
      (make-test-case
       "points->region 1"
       (let* ((points (region->points (make-region 2 3 4 5 #f #f)))
              (r (points->region points #f #f)))
         (assert-equal? 2 (region-x r))
         (assert-equal? 3 (region-y r))
         (assert-equal? 4 (region-w r))
         (assert-equal? 5 (region-h r)))
       )

      (make-test-case
       "points->region 2"
       (let* ((points (region->points (make-region 3 4 5 8 #f #f)))
              (r (points->region points #f #f)))
         (assert-equal? 3(region-x r))
         (assert-equal? 4(region-y r))
         (assert-equal? 5(region-w r))
         (assert-equal? 8(region-h r)))
       )                
      (make-test-case
       "points->region 3"
       (let* ((r (points->region (list (make-point 10 10)
                                       (make-point 20 10)) #f #f)))
         (assert-equal? (region-x r)  10)
         (assert-equal? (region-y r)  10)
         (assert-equal? (region-w r)  10)
         (assert-equal? (region-h r)   0)))
      (make-test-case
       "Region 1"
       (let ((actual (rotate-region (make-region 0 0 10 0 #f #f) origin 1)))
         (assert-equal? (region-x actual)  0)
         (assert-equal? (region-y actual) -10)
         (assert-equal? (region-w actual)  0)
         (assert-equal? (region-h actual) 10)))

      (make-test-case
       "Region 2"
       (let ((actual (rotate-region (make-region 10 10 10 0 #f #f) origin 1)))
         (assert-equal? (region-x actual)  10)
         (assert-equal? (region-y actual) -20)
         (assert-equal? (region-w actual)   0)
         (assert-equal? (region-h actual)  10)))

      (make-test-case
       "preserves label and callback"
       (let* ((before (make-region 0 0 0 0 "Yo momma" add1))
              (after (rotate-region before (make-point 0 0)
                                    1)))
         (assert-equal? (region-label before)
                        (region-label after))
         (assert-equal? 2 ((region-callback after) 1))))
      ))
   ))
