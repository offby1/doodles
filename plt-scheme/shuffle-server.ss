(module shuffle-server mzscheme
(require (lib "thread.ss")
         (only (lib "1.ss" "srfi")
               iota)
         (planet "fys.ss" ("offby1" "offby1.plt")))

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
              "Connection ~a: ~a ~a~%"
              *connections*
              remote-ip
              remote-port))


   (file-stream-buffer-mode op 'line)

   (let ((deck (apply vector (iota 52))))
     (fisher-yates-shuffle! deck)
     (fprintf op "~a~%" deck))

   (fprintf (current-error-port)
            "Thass all for that client.~%"))

 #f))
