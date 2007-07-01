#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace -qu "$0" ${1+"$@"}
|#

(module url-server mzscheme
(define (process-lines ip func)
  (let loop ()
    (let ((line (read-line ip)))
      (when (not (eof-object? line))
        (func line)
        (loop)))))

(call-with-input-file "/etc/passwd"
  (lambda (ip)
    (process-lines ip (lambda (line)
                        (printf "->|~a~%" line)))))

)