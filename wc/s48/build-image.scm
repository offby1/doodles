;; This is not exactly Scheme -- it is Scheme48's "exec" language,
;; which is a superset of Scheme.
(config '(load "packages.scm"))
(config '(open boink))
(build (lambda (x)
         (display "resumer's argument is ")
         (write x)
         (newline)
         (boink)
         )
       "boink.image")
