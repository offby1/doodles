;; practice using call-with-current-continuation to recover gracefully
;; from errors.


(let loop ()
  (newline)
  (display "Eric> ")

  (let ((result (call-with-current-continuation
		 (lambda (bail-out)

		   (let ((input (read)))
		     (if (memq input '(quit exit error))
			 (bail-out input))
		     (write input)
                     input)))))

    (display "Result is ")
    (display result)
    (newline)
    (case result
      ((error)(display "That was an error."))
      ((quit) (display "You want to quit."))
      ((exit) (display "You want to exit."))
      (else (loop)))))
