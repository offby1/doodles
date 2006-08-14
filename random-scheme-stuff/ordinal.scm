(require 'format)

(define (rational->string Q)
  
  ;; assume Q is exact.  If it isn't, we'll get an error from
  ;; `format'. 

  (if (integer? Q)
      (format "~R" Q)
    (string-append (format "~R" (numerator Q))
                   " "
                   (if (not (= (denominator Q) 2))
                       (string-append
                        (format "~:R" (denominator Q))
                        (format "~P" (abs (numerator Q))))
                     (if (= 1 (abs (numerator Q)))
                         "half"
                       "halves")))))

; (map rational->string (list -1/2 -3/2 1/2 3/2 4 -4 1999)) => ("minus one half" "minus three halves" "one half" "three halves" "four" "minus four" "one thousand, nine hundred ninety-nine")
