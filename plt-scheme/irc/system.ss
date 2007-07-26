#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

;; odd that this isn't defined for me somewhere
(module system mzscheme
(require (lib "process.ss"))

;; (listof string?) -> string?
(define (system-args->string exe . args)
  (let ((output-port (open-output-string (cons exe args))))
    (let-values (((stdout-ip stdin-op pid err-ip controller)
                  (apply values
                         (apply
                          process*/ports
                          output-port
                          (open-input-string "")
                          (current-error-port)
                          exe
                          args))))

      (controller 'wait)
      (get-output-string output-port)))
  )
(display (system-args->string "/usr/local/bin/svnversion" "."))
)