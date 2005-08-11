;; This is not exactly Scheme -- it is Scheme48's "exec" language,
;; which is a superset of Scheme.
(config '(load "packages.scm"))
(user '(open wc))

;; todo -- figure out how to tell if this command fails, and if so,
;; exit with a non-zero status.
(user '(build (lambda (x)
                (display "resumer's argument is ")
                (write x)
                (newline)
                (wc "fred")
                0                       ;exit status
                )
              "boink.image"))
