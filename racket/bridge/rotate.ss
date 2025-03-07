#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mred -qu "$0" ${1+"$@"}
|#

;; TODO: figure out how to prevent this module from loading if any of
;; its self-tests fail.

(module rotate mzscheme
  (require (lib "cards.ss" "games" "cards")) ; for region et al.
  (require (lib "class.ss"))

  (require (planet "test.ss"    ("schematics" "schemeunit.plt" 2)))
  (require (planet "text-ui.ss" ("schematics" "schemeunit.plt" 2)))
  (require (lib "list.ss"))

  (provide make-point
           rotate-region)

  ;; I could have defined a structure named point, but this way I get
  ;; addition and subtraction for free.
  (define make-point make-rectangular)
  (define point-x real-part)
  (define point-y imag-part)

  ;; subtlety: since we're dealing with computer graphics, the origin
  ;; is in the upper left (not the lower left), and y values increase
  ;; downwards, not upwards.  Thus point [x, y] goes to [y, -x], not
  ;; [-y, x] as it would if the origin were the lower left, and y
  ;; increased upwards.
  (define (rotate-quarter-turn-ccw p)
    (make-point (point-y p)
                (- (point-x p))))

  (define (rotate-some p quarter-turns)
    (set! quarter-turns (modulo quarter-turns 4))
    (if (zero? quarter-turns)
        p
      (rotate-some (rotate-quarter-turn-ccw p)
                   (sub1 quarter-turns))))

  (define (rotate-about p origin quarter-turns)
    (+ origin
       (rotate-some (- p origin) quarter-turns)))

  (define (translate p dx dy)
    (make-point (+ dx (point-x p))
                (+ dy (point-y p))))

  (define (region->points r)
    (let* ((ul (make-point (region-x r) (region-y r)))
           (lr (translate ul (region-w r) (region-h r))))
      (list ul lr)))

  (define (points->region points label callback)
    (define p1 (first points))
    (define p2 (second points))

    (make-region (min (point-x p1)
                      (point-x p2))
                 (min (point-y p1)
                      (point-y p2))
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

  (define-check (check-member obj seq)
    (member obj seq))

  ;; this elaborate test suite seems like overkill, but it helped me
  ;; find the bug wherein I failed to understand the subtlety referred
  ;; to in the comment by rotate-quarter-turn-ccw, above
  (test/text-ui
   (let ((origin (make-point 0 0))
         (victim (make-point 1 1)))
     (test-suite
      "everything"

      (test-case
       "Rotate 10, 10"
       (let ((rotated (rotate-quarter-turn-ccw (make-point 10 10))))
         (check-equal? rotated (make-point 10 -10))))

      (test-case
       "zero"
       (let ((actual (rotate-about victim origin 0)))
         (check-true (exact? actual))
         (check-equal? victim actual)))

      (test-case
       "1"
       (check-equal? (make-point 1 -1)
                      (rotate-about victim origin 1)))

      (test-case
       "2"
       (check-equal? -1-1i
                      (rotate-about victim origin 2)))

      (test-case
       "wrap"
       (check-equal? (rotate-about victim origin 1)
                      (rotate-about victim origin 5)))

      (test-case
       "check on non-integer rotations"
       (check-exn exn:fail:contract?
                   (lambda ()
                     (rotate-about victim origin 1.333)
                     ))
       (check-exn exn:fail:contract?
                   (lambda ()
                     (rotate-about victim origin 1/8)
                     )))

      (test-case
       "Region->points 1"
       (let ((points (region->points  (make-region 0 0 10 0 #f #f))))
         (check-equal? 2 (length points) "exactly four points")
         (check-member (make-point  0  0) points "0 0")
         (check-member (make-point 10  0) points "10 0")
         ))
      (test-case
       "Region->points 2"
       (let ((points (region->points (make-region 2 3 4 5 #f #f))))
         (check-equal? 2 (length points) "exactly four points")
         (check-member (make-point  2 3) points )
         (check-member (make-point  6 8) points )
         ))
      (test-case
       "region->points 3"
       (let ((p (region->points (points->region (list (make-point 10 10)
                                                      (make-point 20 10)) #f #f))))
         (check-member (make-point 10 10) p)
         (check-member (make-point 20 10) p)))
      (test-case
       "points->region 1"
       (let* ((points (region->points (make-region 2 3 4 5 #f #f)))
              (r (points->region points #f #f)))
         (check-equal? 2 (region-x r))
         (check-equal? 3 (region-y r))
         (check-equal? 4 (region-w r))
         (check-equal? 5 (region-h r)))
       )

      (test-case
       "points->region 2"
       (let* ((points (region->points (make-region 3 4 5 8 #f #f)))
              (r (points->region points #f #f)))
         (check-equal? 3(region-x r))
         (check-equal? 4(region-y r))
         (check-equal? 5(region-w r))
         (check-equal? 8(region-h r)))
       )
      (test-case
       "points->region 3"
       (let* ((r (points->region (list (make-point 10 10)
                                       (make-point 20 10)) #f #f)))
         (check-equal? (region-x r)  10)
         (check-equal? (region-y r)  10)
         (check-equal? (region-w r)  10)
         (check-equal? (region-h r)   0)))
      (test-case
       "Region 1"
       (let ((actual (rotate-region (make-region 0 0 10 0 #f #f) origin 1)))
         (check-equal? (region-x actual)  0)
         (check-equal? (region-y actual) -10)
         (check-equal? (region-w actual)  0)
         (check-equal? (region-h actual) 10)))

      (test-case
       "Region 2"
       (let ((actual (rotate-region (make-region 10 10 10 0 #f #f) origin 1)))
         (check-equal? (region-x actual)  10)
         (check-equal? (region-y actual) -20)
         (check-equal? (region-w actual)   0)
         (check-equal? (region-h actual)  10)))

      (test-case
       "preserves label and callback"
       (let* ((before (make-region 0 0 0 0 "Yo momma" add1))
              (after (rotate-region before (make-point 0 0)
                                    1)))
         (check-equal? (region-label before)
                        (region-label after))
         (check-equal? 2 ((region-callback after) 1))))
      ))
   ))
