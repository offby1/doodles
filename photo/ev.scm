(define (ev shutter-speed-denominator f-stop)
  (define log2
    (let ((l2 (log 2)))
      (lambda (x)
        (/ (log x)
           l2))))
  (inexact->exact
   (round
    (+ (log2 shutter-speed-denominator)
       (* 2 (log2 f-stop))))))

;; f/1 @ 1 second = EV 0
