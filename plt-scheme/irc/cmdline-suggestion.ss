(require  (lib "cmdline.ss"))

(define *haircut-interval*
  (make-parameter
   20
   (lambda (value)
     (if (and (number? value)
              (integer? value)
              (exact? value)
              (positive? value))
         value
       (raise-user-error
        '*haircut-interval*
        "Yo, I want an exact positive integer, not ~s"
        value)))))

(define (cmdline-parser argvector)
  (command-line
   "yo"
   argvector
   (once-each
    (("--haircut-interval") secs "Seconds to wait before getting a haircut"
     (*haircut-interval* secs))
    )))

(with-handlers
    ((exn:fail:user?
      (lambda (exn)
        (display (exn-message exn)
                 (current-error-port))
        (newline (current-error-port))
        (fprintf (current-error-port)
                 "Allow me to show you the proper syntax:~%")
        (cmdline-parser (vector "--help")))))
  (cmdline-parser (current-command-line-arguments)))

(printf "Everything is good: *haircut-interval* is ~s~%"
        (*haircut-interval*))
