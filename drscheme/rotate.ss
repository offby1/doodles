(module rotate mzscheme
  (provide make-point
           point-x
           point-y
           rotate-about)
  (define make-point make-rectangular)
  (define point-x real-part)
  (define point-y imag-part)

  (define (rotate-quarter-turn-ccw p)
    (make-point (- (point-y p))
                (point-x p)))

  (define (rotate-some p turns)
    (set! turns (modulo turns 4))
    (if  (zero? turns)
        p
      (rotate-some (rotate-quarter-turn-ccw p)
                   (sub1 turns))))

  (define (rotate-about p origin turns)
    (+ origin
       (rotate-some (- p origin) turns))))