(define parse 
  (lambda (time-string)
    (let ((rm (string-match "([0-9][0-9]?):([0-9][0-9]?)([ \t]*([aApP][mM]))?" time-string)))
      (write rm) (newline)
      (and rm
           (let ((hours   (match:substring rm 1))
                 (minutes (match:substring rm 2))
                 (am-pm   (match:substring rm 4)))
             (+  (* 60 (remainder (string->number hours) 12))
                 (string->number minutes) 
                 (if (and
                      am-pm
                      (char-ci=? #\p (string-ref am-pm 0)))
                     (* 60 12)
                   0)))))))