#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

;; odd that this isn't defined for me somewhere
(module system mzscheme

;; it's entirely possible that I don't need process.ss, and could
;; instead use "subprocess"; I just haven't read the docs carefully
;; enough to tell which is best.  This works, anyway.
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
(display (system-args->string (path->string (find-executable-path "svnversion")) "."))
)