#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme --no-init-file --mute-banner --version --require "$0"
|#
(module server mzscheme
(require (lib "thread.ss"))
(run-server
 1234
 (lambda (ip op)
   (file-stream-buffer-mode op 'line)
   (let loop ()
     (let ((one-datum (read ip)))
       (when (not (eof-object? one-datum))
         (write (eval one-datum) op)
         (newline op)
         (loop)))))
 #f)

(provide (all-defined))
)
