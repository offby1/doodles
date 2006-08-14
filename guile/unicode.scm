(let ()

  ;; Redundant on Unix, but what the heck.
  (define (with-output-to-binary-file fn proc)
    (let* ((op (open-file fn "wb"))
           (return (proc op)))
      (close-port op)
      return))

  (with-output-to-binary-file
   "/tmp/all-unicode-characters.txt"
   (lambda (port)
     (let loop ((chars 0))
       (if (< chars 65536)
           (begin
             (display (integer->char (quotient chars 256)) port)
             (display (integer->char (remainder chars 256)) port)
             (loop (+ 1 chars))
             ))))))