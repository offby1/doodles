(require (lib "1.ss" "srfi"))

;; just an example of using "unfold"
(define (port->lines ip)
  (unfold (lambda (ip)
            (eof-object? (peek-char ip)))
          read-line
          values
          ip))

(define (file->lines fn)
  (call-with-input-file fn port->lines))

(write (file->lines "/etc/passwd"))
(newline)
