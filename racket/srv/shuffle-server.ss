#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme --no-init-file --mute-banner --version --require "$0"
|#
(module shuffle-server mzscheme
(require (lib "date.ss")
         (lib "thread.ss")
         (only (lib "1.ss" "srfi")
               iota)
         (planet "shuffle.ss" ("offby1" "offby1.plt")))

(file-stream-buffer-mode (current-output-port) 'line)
(file-stream-buffer-mode (current-error-port) 'line)

(define *connections* 0)

(run-server
 1122
 (lambda (ip op)
   ;; possible race here
   (set! *connections* (add1 *connections*))


   (let-values (((local-ip
                  local-port
                  remote-ip
                  remote-port)
                 (tcp-addresses ip #t)))
     (fprintf (current-error-port)
              "~a ~a: ~a ... "
              (date->string (seconds->date (current-seconds)) #t)
              *connections*
              remote-ip))


   (file-stream-buffer-mode op 'line)

   (let ((deck (shuffle (apply vector (iota 52)))))
     (fprintf op "~a~%" deck))

   (fprintf (current-error-port)
            "Thass all for that client.~%"))

 #f
 void
 (lambda (port-k max-allow-wait-k reuse?)
   (tcp-listen port-k 10 reuse?))))
