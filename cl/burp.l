(let ((digits nil)
      (x 100))
  (when (or (not (integerp x))
            (minusp x))
    (error "Silly boy."))
  (loop
     (when (zerop x)
       (return))
     (cond 
       ((oddp x)
        (decf x)
        (push #\1 digits))
       (t 
        (push #\0 digits)))
     (setf x (/ x 2)))
              
  (concatenate 'string digits))