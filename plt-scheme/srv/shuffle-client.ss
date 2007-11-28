(module shuffle-client mzscheme
(define *times-per-thunk* 100)
(define *threads* 100)
(define spam-thunk
  (lambda ()
    (with-handlers
        ([exn:fail:network?
          (lambda (e)
            (fprintf (current-error-port)
                     "Uh oh: ~a~%" (exn-message e)))])
      (let loop ((times 0))
        (when (< times *times-per-thunk*)
          (let-values (((ip op)
                        (tcp-connect/enable-break "192.168.0.5" 1122)))
            (close-output-port op)
            (let ((datum (read ip)))
              (cond
               ((vector? datum)
                (printf "."))
               (else
                (printf "??!!~%"))))
            (close-input-port ip))
          (loop (add1 times))))
      (printf "~%"))))

(for-each sync
          (let loop ((threads-to-make *threads*)
                     (threads '()))
            (if (positive? threads-to-make)
                (begin
                  (printf "~a~%" threads-to-make)
                  (loop (sub1 threads-to-make)
                        (cons (thread spam-thunk )
                              threads)))
                threads)))
)