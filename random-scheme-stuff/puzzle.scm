;; The hour hand and the minute hand are both pointing exactly to a
;; minute mark, and are two minutes apart.  What time is it?

(require 'pretty-print)

(let loop ((minutes 0)
           (result '()))
  (let ((hour-hour-hand-points-to   (floor (/ minutes 60)))
        (minute-hour-hand-points-to (/ minutes 12)))
    (if (< minutes (* 12 60))
        (loop (+ 12 minutes)            ; don't bother checking every
                                        ; minute; the hour hand points
                                        ; exactly to a minute only
                                        ; once every 12 minutes.

              ;; Are the two hands two minutes apart?
              (if (memq (modulo (- minute-hour-hand-points-to minutes)
                                60)
                        (list 2 58))

                ;; Yes, save their values in the result.
                (cons 
                 (string-append 
                  (number->string (inexact->exact hour-hour-hand-points-to))
                  ":"
                  (number->string (remainder minutes 60))
                  " (hour hand points to "
                  (number->string minute-hour-hand-points-to)
                  ")")
                 result)
                          
                ;; No; keep going with the old results.
                result
                ))
      
      ;; We've moved the minute hand once around the dial; let's see
      ;; what results we bagged.
      (begin
        (newline)
        (pretty-print (reverse result))))))
